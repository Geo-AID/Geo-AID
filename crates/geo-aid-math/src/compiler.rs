use std::mem;
use cranelift::codegen::ir::{FuncRef, Function, UserExternalName};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use crate::{ComparisonKind, Condition, Context, Expr, ExprKind, Float};

#[cfg(feature = "f64")]
const FLOAT: Type = types::F64;
#[cfg(not(feature = "f64"))]
const FLOAT: Type = types::F32;

#[derive(Clone, Copy)]
struct External {
    sin: FuncRef,
    cos: FuncRef,
    pow: FuncRef
}

fn link_external(func: &mut Function, module: &mut JITModule) -> External {
    // sin(f64) -> f64
    let mut sig = Signature::new(module.target_config().default_call_conv);
    sig.params.push(AbiParam::new(FLOAT));
    sig.returns.push(AbiParam::new(FLOAT));

    let sin = module.declare_function("sin", Linkage::Import, &sig).unwrap();
    let sin = func.declare_imported_user_function(UserExternalName::new(0, sin.as_u32()));
    let sin_sig = func.import_signature(sig);
    let sin = func.import_function(ExtFuncData {
        name: ExternalName::User(sin),
        colocated: false,
        signature: sin_sig
    });

    // cos(f64) -> f64
    let mut sig = Signature::new(module.target_config().default_call_conv);
    sig.params.push(AbiParam::new(FLOAT));
    sig.returns.push(AbiParam::new(FLOAT));

    let cos = module.declare_function("cos", Linkage::Import, &sig).unwrap();
    let cos = func.declare_imported_user_function(UserExternalName::new(0, cos.as_u32()));
    let cos_sig = func.import_signature(sig);
    let cos = func.import_function(ExtFuncData {
        name: ExternalName::User(cos),
        colocated: false,
        signature: cos_sig
    });

    // pow(f64, f64) -> f64
    let mut sig = Signature::new(module.target_config().default_call_conv);
    sig.params.push(AbiParam::new(FLOAT));
    sig.params.push(AbiParam::new(FLOAT));
    sig.returns.push(AbiParam::new(FLOAT));

    let pow = module.declare_function("pow", Linkage::Import, &sig).unwrap();
    let pow = func.declare_imported_user_function(UserExternalName::new(0, pow.as_u32()));
    let pow_sig = func.import_signature(sig);
    let pow = func.import_function(ExtFuncData {
        name: ExternalName::User(pow),
        colocated: false,
        signature: pow_sig
    });

    External {
        sin, cos, pow
    }
}

#[must_use]
pub fn compile(context: &Context, exprs: &[Expr]) -> fn(&[Float], &mut [Float]) {
    let mut flag_builder = settings::builder();
    flag_builder.set("use_colocated_libcalls", "false").unwrap();
    flag_builder.set("opt_level", "speed").unwrap();
    flag_builder.set("is_pic", "false").unwrap();

    let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
        panic!("internal error: host machine is not supported: {msg}")
    });
    let isa = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .unwrap();
    let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

    let mut module = JITModule::new(builder);

    let mut builder_ctx = FunctionBuilderContext::new();
    let mut ctx = module.make_context();

    // Link external functions
    let external = link_external(&mut ctx.func, &mut module);

    // The function takes a slice of inputs and a mutable slice of outputs.
    let ptr = module.target_config().pointer_type();

    // Inputs
    ctx.func.signature.params.push(AbiParam::new(ptr));
    // Destination
    ctx.func.signature.params.push(AbiParam::new(ptr));

    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);
    let entry = builder.create_block();
    builder.append_block_params_for_function_params(entry);

    builder.switch_to_block(entry);
    builder.seal_block(entry);

    // We save the params to variables
    let inputs = Variable::new(0);
    let outputs = Variable::new(1);
    let args = builder.block_params(entry);
    let (inputs_arg, outputs_arg) = (args[0], args[1]);
    builder.declare_var(inputs, ptr);
    builder.def_var(inputs, inputs_arg);
    builder.declare_var(outputs, ptr);
    builder.def_var(outputs, outputs_arg);


    // Parse all expressions.
    for (i, expr) in context.exprs.iter().enumerate() {
        let var = Variable::new(i);
        builder.declare_var(var, FLOAT);
        let value = compile_expr(&mut builder, expr.kind, external);
        builder.def_var(var, value);
    }

    for (i, expr) in exprs.iter().enumerate() {
        let v = builder.use_var(var(*expr));
        let outputs = builder.use_var(Variable::new(1));
        builder.ins().store(
            MemFlags::new().with_notrap().with_aligned(),
            v, outputs, Offset32::new(i as i32)
        );
    }

    builder.finalize();

    let id = module.declare_function("func", Linkage::Export, &ctx.func.signature).unwrap();
    module.define_function(id, &mut ctx).unwrap();
    module.clear_context(&mut ctx);
    module.finalize_definitions().unwrap();

    let func = module.get_finalized_function(id);

    unsafe {
        mem::transmute(func)
    }
}

fn compile_expr(builder: &mut FunctionBuilder, kind: ExprKind, external: External) -> Value {
    match kind {
        ExprKind::Constant(v) => float_constant(builder, v),
        ExprKind::Add(a, b) => {
            let a = builder.use_var(var(a));
            let b = builder.use_var(var(b));
            builder.ins().fadd(a, b)
        },
        ExprKind::Sub(a, b) => {
            let a = builder.use_var(var(a));
            let b = builder.use_var(var(b));
            builder.ins().fsub(a, b)
        }
        ExprKind::Mul(a, b) => {
            let a = builder.use_var(var(a));
            let b = builder.use_var(var(b));
            builder.ins().fmul(a, b)
        }
        ExprKind::Div(a, b) => {
            let a = builder.use_var(var(a));
            let b = builder.use_var(var(b));
            builder.ins().fdiv(a, b)
        }
        ExprKind::Input(index) => {
            let inputs = builder.use_var(Variable::new(0));
            builder.ins().load(
                FLOAT,
                MemFlags::new().with_readonly().with_notrap().with_aligned(),
                inputs,
                Offset32::new(index as i32)
            )
        }
        ExprKind::Sin(v) => {
            let v = builder.use_var(var(v));
            let tmp = builder.ins().call(external.sin, &[v]);
            builder.inst_results(tmp)[0]
        }
        ExprKind::Cos(v) => {
            let v = builder.use_var(var(v));
            let tmp = builder.ins().call(external.cos, &[v]);
            builder.inst_results(tmp)[0]
        }
        ExprKind::Neg(v) => {
            let v = builder.use_var(var(v));
            builder.ins().fneg(v)
        }
        ExprKind::Ternary(cond, then, else_) => {
            let v = match cond {
                Condition::Comparison(cmp) => {
                    let cc = match cmp.kind {
                        // ComparisonKind::Eq => FloatCC::Equal,
                        // ComparisonKind::Neq => FloatCC::NotEqual,
                        // ComparisonKind::Gt => FloatCC::GreaterThan,
                        ComparisonKind::Lt => FloatCC::LessThan,
                        // ComparisonKind::Gteq => FloatCC::GreaterThanOrEqual,
                        // ComparisonKind::Lteq => FloatCC::LessThanOrEqual
                    };
                    let a = builder.use_var(var(cmp.a));
                    let b = builder.use_var(var(cmp.b));
                    builder.ins().fcmp(cc, a, b)
                }
            };
            let then = builder.use_var(var(then));
            let else_ = builder.use_var(var(else_));

            let then_block = builder.create_block();
            let else_block = builder.create_block();
            let merge_block = builder.create_block();
            builder.append_block_param(merge_block, FLOAT);

            builder.ins().brif(v, then_block, &[], else_block, &[]);
            builder.seal_block(then_block);
            builder.seal_block(else_block);

            builder.switch_to_block(then_block);
            builder.ins().jump(merge_block, &[then]);

            builder.switch_to_block(else_block);
            builder.ins().jump(merge_block, &[else_]);

            builder.seal_block(merge_block);
            builder.switch_to_block(merge_block);
            builder.block_params(merge_block)[0]
        }
        ExprKind::Pow(v, e) => {
            let v = builder.use_var(var(v));
            let e = float_constant(builder, e);
            let tmp = builder.ins().call(external.pow, &[v, e]);
            builder.inst_results(tmp)[0]
        }
    }
}

fn var(expr: Expr) -> Variable {
    Variable::new(expr.0 + 2)
}

#[cfg(feature = "f64")]
fn float_constant(builder: &mut FunctionBuilder, value: Float) -> Value {
    builder.ins().f64const(value)
}

#[cfg(not(feature = "f64"))]
fn float_constant(builder: &mut FunctionBuilder, value: Float) -> Value {
    builder.ins().f32const(value)
}