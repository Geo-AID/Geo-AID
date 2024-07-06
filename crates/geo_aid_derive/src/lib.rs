extern crate proc_macro;

use proc_macro::TokenStream;

use quote::{format_ident, quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::Paren;
use syn::{
    braced, parenthesized, parse_macro_input, Attribute, Data, DeriveInput, Expr, Fields,
    GenericArgument, GenericParam, Generics, Ident, Lit, Path, PathArguments, PathSegment, Token,
    Type, TypePath, Block,
};

/// Assumes there's no where clause.
fn create_type_from_def(ident: &Ident, generics: &Generics) -> Type {
    let mut args = generics.params.iter().map(|param| match param {
        GenericParam::Lifetime(lf) => GenericArgument::Lifetime(lf.lifetime.clone()),
        GenericParam::Type(ty) => GenericArgument::Type(Type::Path(TypePath {
            qself: None,
            path: Path {
                leading_colon: None,
                segments: vec![PathSegment {
                    ident: ty.ident.clone(),
                    arguments: PathArguments::None,
                }]
                .into_iter()
                .collect(),
            },
        })),
        GenericParam::Const(c) => GenericArgument::Const({
            let ident = &c.ident;
            let expanded = quote! { #ident };

            syn::parse2(expanded).unwrap()
        }),
    });

    if let Some(first) = args.next() {
        syn::parse2(quote! {
            #ident<#first, #(#args),*>
        })
        .unwrap()
    } else {
        syn::parse2(quote! {
            #ident
        })
        .unwrap()
    }
}

#[proc_macro_derive(Execute)]
pub fn derive_execute(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let enum_name = input.ident;
    let enum_data = match &input.data {
        Data::Enum(v) => v,
        _ => panic!("invalid Execute input"),
    };
    let variant1 = enum_data.variants.iter().map(|v| &v.ident);

    let expanded = quote! {
        impl Execute for #enum_name {
            unsafe fn execute(&self, args: &mut [Value]) {
                match self {
                    #(Self::#variant1(v) => v.execute(args),)*
                }
            }
        }
    };

    expanded.into()
}

// enum DefinitionParam {
//     Entity,
//     NoEntity,
//     Variable,
//     Sequence,
//     Map,
//     Expression,
//     Order(Expr),
// }

// impl DefinitionParam {
//     #[must_use]
//     pub fn is_entity(&self) -> bool {
//         matches!(self, Self::Entity)
//     }
// }

// impl Parse for DefinitionParam {
//     fn parse(input: ParseStream) -> syn::Result<Self> {
//         let ident: Ident = input.parse()?;

//         Ok(match ident.to_string().as_str() {
//             "entity" => Self::Entity,
//             "order" => Self::Order({
//                 let content;
//                 let _ = parenthesized!(content in input);

//                 content.parse()?
//             }),
//             "no_entity" => Self::NoEntity,
//             "variable" => Self::Variable,
//             "sequence" => Self::Sequence,
//             "map" => Self::Map,
//             &_ => panic!("invalid def"),
//         })
//     }
// }

// impl From<&Vec<Attribute>> for DefinitionParam {
//     fn from(value: &Vec<Attribute>) -> Self {
//         value
//             .iter()
//             .find(|a| a.path().is_ident("def"))
//             .map_or(DefinitionParam::Expression, |x| x.parse_args().unwrap())
//     }
// }

// fn definition_handle_enum(
//     name: &Ident,
//     generics: &Generics,
//     attrs: &Vec<Attribute>,
//     enum_data: &DataEnum,
// ) -> TokenStream {
//     let where_clause = &generics.where_clause;

//     let variant1_code = enum_data.variants.iter().map(|variant| {
//         let name = &variant.ident;

//         let field_ident = variant.fields.iter().map(|f| {
//             if DefinitionParam::from(&f.attrs).is_entity() {
//                 format_ident!("id")
//             } else {
//                 format_ident!("_")
//             }
//         });

//         let field_getter = if let DefinitionParam::Order(order) = DefinitionParam::from(attrs) {
//             quote! {#order}
//         } else {
//             variant
//                 .fields
//                 .iter()
//                 .find(|f| DefinitionParam::from(&f.attrs).is_entity())
//                 .map_or_else(
//                     || quote! {0},
//                     |_| {
//                         quote! {
//                             context.get_entity(*id).order(context)
//                         }
//                     },
//                 )
//         };

//         let fields = if variant.fields.is_empty() {
//             quote! {}
//         } else {
//             quote! {(#(#field_ident),*)}
//         };

//         quote! {
//             Self::#name #fields => {
//                 #field_getter
//             }
//         }
//     });

//     let variant2_code = enum_data.variants.iter().map(|variant| {
//         let name = &variant.ident;

//         let field_ident = (0..variant.fields.len()).map(|i| format_ident!("v{i}"));

//         let field_checker = variant.fields.iter().enumerate().map(|(i, field)| {
//             let field_ident = format_ident!("v{i}");
//             let field_def = DefinitionParam::from(&field.attrs);

//             match field_def {
//                 DefinitionParam::Entity => quote! {
//                     (if *#field_ident == entity {
//                         true
//                     } else {
//                         context.get_entity(*#field_ident).contains_entity(entity, context)
//                     })
//                 },
//                 DefinitionParam::Variable => quote! {
//                     #field_ident.borrow().definition.contains_entity(entity, context)
//                 },
//                 DefinitionParam::Sequence => quote! {
//                     #field_ident.iter().any(|x| x.contains_entity(entity, context))
//                 },
//                 DefinitionParam::Map => quote! {
//                     #field_ident.values().any(|x| x.contains_entity(entity, context))
//                 },
//                 DefinitionParam::NoEntity | DefinitionParam::Order(_) => quote! {
//                     false
//                 },
//                 DefinitionParam::Expression => quote! {
//                     #field_ident.contains_entity(entity, context)
//                 },
//             }
//         });

//         let fields = if variant.fields.is_empty() {
//             quote! {}
//         } else {
//             quote! {(#(#field_ident),*)}
//         };

//         quote! {
//             Self::#name #fields => {
//                 #(#field_checker ||)* false
//             }
//         }
//     });

//     let expanded = quote! {
//         impl #generics Definition for #name #generics #where_clause {
//             fn order(&self, context: &CompileContext) -> usize {
//                 match self {
//                     #(#variant1_code)*
//                 }
//             }

//             fn contains_entity(&self, entity: usize, context: &CompileContext) -> bool {
//                 match self {
//                     #(#variant2_code)*
//                 }
//             }
//         }
//     };
//     // panic!("{}", expanded.to_string());

//     expanded.into()
// }

// #[proc_macro_derive(Definition, attributes(def))]
// pub fn derive_definition(input: TokenStream) -> TokenStream {
//     let input = parse_macro_input!(input as DeriveInput);

//     let name = &input.ident;
//     let generics = &input.generics;

//     match &input.data {
//         Data::Enum(v) => definition_handle_enum(name, generics, &input.attrs, v),
//         Data::Struct(struct_data) => {
//             let order_field_code = if struct_data.fields.is_empty() {
//                 quote! {0}
//             } else {
//                 struct_data
//                     .fields
//                     .iter()
//                     .flat_map(|field| {
//                         if DefinitionParam::from(&field.attrs).is_entity() {
//                             let field_name = field.ident.as_ref().unwrap();
//                             Some(quote! {
//                                 self.#field_name.order(context)
//                             })
//                         } else {
//                             None
//                         }
//                     })
//                     .next()
//                     .unwrap()
//             };

//             let contains_field_code = if struct_data.fields.is_empty() {
//                 quote! {false}
//             } else {
//                 struct_data
//                     .fields
//                     .iter()
//                     .flat_map(|field| {
//                         if DefinitionParam::from(&field.attrs).is_entity() {
//                             let field_name = field.ident.as_ref().unwrap();
//                             Some(quote! {
//                                 self.#field_name.contains_entity(entity, context)
//                             })
//                         } else {
//                             None
//                         }
//                     })
//                     .next()
//                     .unwrap()
//             };

//             let expanded = quote! {
//                 impl Definition for #name {
//                     fn order(&self, context: &CompileContext) -> usize {
//                         #order_field_code
//                     }

//                     fn contains_entity(&self, entity: usize, context: &CompileContext) -> bool {
//                         #contains_field_code
//                     }
//                 }
//             };

//             expanded.into()
//         }
//         _ => panic!("unsupported"),
//     }
// }

enum GType {
    Simple(Ident),
    Collection(usize),
    Bundle(String),
}

// impl GType {
//     fn get_conversion_target(&self) -> Ident {
//         match self {
//             GType::Simple(s) => match s.to_string().as_str() {
//                 "DISTANCE"
//                 | "ANGLE"
//                 | "SCALAR" => format_ident!("Scalar"),
//                 "POINT" => format_ident!("Point"),
//                 "CIRCLE" => format_ident!("Circle"),
//                 "LINE" => format_ident!("Line"),
//                 &_ => unreachable!()
//             },
//             GType::Collection(_) => format_ident!("PointCollection"),
//             GType::Bundle(_) => format_ident!("Bundle")
//         }
//     }
// }

struct OverloadFunction {
    params: Vec<GType>,
    param_group: Option<GType>,
    return_type: GType,
    func: Expr,
}

struct OverloadRule {
    lhs: GType,
    rhs: GType,
    func: Expr,
}

enum OverloadInput {
    Function(OverloadFunction),
    Rule(OverloadRule),
}

impl ToTokens for GType {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            GType::Simple(ident) => tokens.extend(quote! {
                crate::script::ty::#ident
            }),
            GType::Collection(l) => tokens.extend(quote! {
                crate::script::ty::collection(#l)
            }),
            GType::Bundle(name) => tokens.extend(quote! {
                crate::script::ty::bundle(#name)
            }),
        }
    }
}

impl GType {
    fn parse(input: ParseStream) -> Option<Self> {
        if !input.peek(Ident) && !input.peek(Lit) {
            return None;
        }

        if let Some(ident) = input.parse::<Option<Ident>>().ok()? {
            match ident.to_string().as_str() {
                "DISTANCE" | "ANGLE" | "SCALAR" | "POINT" | "LINE" | "CIRCLE" => {
                    Some(Self::Simple(ident))
                }
                other => Some(Self::Bundle(other.to_string())),
            }
        } else {
            let l = input.parse::<Lit>().ok()?;
            let _ = input.parse::<Token![-]>().ok()?;
            let ident = input.parse::<Ident>().ok()?;
            if ident.to_string().as_str() == "P" {
                Some(Self::Collection(match l {
                    Lit::Int(i) => i.base10_parse().unwrap(),
                    _ => panic!("WRONG"),
                }))
            } else {
                panic!("WRONG")
            }
        }
    }
}

impl Parse for OverloadInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Some(lhs) = GType::parse(input) {
            let _: Ident = input.parse()?;
            let rhs = GType::parse(input).unwrap();

            let _: Token![:] = input.parse()?;
            let func = input.parse()?;

            Ok(OverloadInput::Rule(OverloadRule { lhs, rhs, func }))
        } else {
            let content;
            let _ = parenthesized!(content in input);

            let mut params = Vec::new();
            while let Some(param) = GType::parse(&content) {
                params.push(param);

                if content.parse::<Option<Token![,]>>()?.is_none() {
                    break;
                }
            }

            let param_group = if !content.is_empty() {
                let _: Token![.] = content.parse()?;
                let _: Token![.] = content.parse()?;
                let _: Token![.] = content.parse()?;
                Some(GType::parse(&content).unwrap())
            } else {
                None
            };

            let _: Token![->] = input.parse()?;
            let return_type = GType::parse(input).unwrap();

            let func = if input.parse::<Option<Token![:]>>()?.is_some() {
                input.parse()?
            } else {
                let content;
                let _ = braced!(content in input);

                content.parse()?
            };

            Ok(OverloadInput::Function(OverloadFunction {
                params,
                param_group,
                return_type,
                func,
            }))
        }
    }
}

#[proc_macro]
pub fn overload(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as OverloadInput);

    match input {
        OverloadInput::Function(OverloadFunction {
            params,
            param_group,
            return_type,
            func,
        }) => {
            let params_it = params.iter();

            let converted_it = params.iter().map(|_| {
                quote! {
                    crate::script::unroll::Convert::convert(args.next().unwrap(), context),
                }
            });

            let converted_group = param_group.as_ref().map(|_| quote! {
                args.map(|x| crate::script::unroll::Convert::convert(x, context)).collect::<Vec<_>>(),
            }).into_iter();

            let param_group_it = param_group
                .as_ref()
                .map_or_else(|| quote! {None}, |x| quote! {Some(#x)});

            let expanded = quote! {
                crate::script::unroll::FunctionOverload {
                    returned_type: #return_type,
                    definition: crate::script::unroll::FunctionDefinition(Box::new(
                        |args, context, display| {
                            let mut args = args.into_iter();
                            crate::script::unroll::AnyExpr::from((#func)(
                                #(#converted_it)*
                                #(#converted_group)*
                                context,
                                display
                            ))
                        }
                    )),
                    params: vec![#(#params_it),*],
                    param_group: #param_group_it
                }
            };

            // panic!("{}", expanded);
            expanded.into()
        }
        OverloadInput::Rule(OverloadRule { lhs, rhs, func }) => {
            let expanded = quote! {
                crate::script::unroll::RuleOverload {
                    definition: crate::script::unroll::RuleDefinition(Box::new(
                        |lhs, rhs, context: &mut crate::script::unroll::context::CompileContext, properties, invert, weight| {
                            std::boxed::Box::new((#func)(
                                crate::script::unroll::Convert::convert(lhs, context),
                                crate::script::unroll::Convert::convert(rhs, context),
                                context,
                                properties,
                                invert,
                                weight
                            ))
                        }
                    )),
                    params: (
                        #lhs,
                        #rhs
                    )
                }
            };

            expanded.into()
        }
    }
}

#[proc_macro_derive(CloneWithNode)]
pub fn derive_clone_with_node(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let generics = &input.generics;
    let where_clause = &generics.where_clause;

    match &input.data {
        Data::Enum(enum_data) => {
            let variant1 = enum_data.variants.iter().map(|v| {
                let v_name = &v.ident;
                let arg_name = v
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, _)| format_ident!("v{i}"));

                let arg_name2 = arg_name.clone();

                let maybe_args = if v.fields.is_empty() {
                    quote! {}
                } else {
                    quote! {(#(#arg_name),*)}
                };

                let maybe_args2 = if v.fields.is_empty() {
                    quote! {}
                } else {
                    quote! {(#(#arg_name2.clone_with_node()),*)}
                };

                quote! {
                    Self::#v_name #maybe_args => Self::#v_name #maybe_args2
                }
            });

            let variant2 = enum_data.variants.iter().map(|v| {
                let v_name = &v.ident;
                let arg_name = v
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, _)| format_ident!("v{i}"));

                let arg_name2 = arg_name.clone();

                let maybe_args = if v.fields.is_empty() {
                    quote! {}
                } else {
                    quote! {(#(#arg_name),*)}
                };

                let maybe_args2 = if v.fields.is_empty() {
                    quote! {}
                } else {
                    quote! {(#(#arg_name2.clone_without_node()),*)}
                };

                quote! {
                    Self::#v_name #maybe_args => Self::#v_name #maybe_args2
                }
            });

            let expanded = quote! {
                impl #generics CloneWithNode for #name #generics #where_clause {
                    fn clone_with_node(&mut self) -> Self {
                        match self {
                            #(#variant1),*
                        }
                    }

                    fn clone_without_node(&self) -> Self {
                        match self {
                            #(#variant2),*
                        }
                    }
                }
            };

            expanded.into()
        }
        Data::Struct(struct_data) => {
            let variant1;
            let variant2;

            match &struct_data.fields {
                Fields::Named(fields) => {
                    variant1 = fields.named.iter().map(|f| {
                        let f_name1 = f.ident.clone().unwrap();
                        let f_name2 = f_name1.clone();

                        quote! {
                            #f_name1: self.#f_name2.clone_with_node()
                        }
                    });

                    variant2 = fields.named.iter().map(|f| {
                        let f_name1 = f.ident.clone().unwrap();
                        let f_name2 = f_name1.clone();

                        quote! {
                            #f_name1: self.#f_name2.clone_without_node()
                        }
                    });
                }
                _ => unimplemented!("Only named fields supported."),
            }

            let expanded = quote! {
                impl #generics CloneWithNode for #name #generics #where_clause {
                    fn clone_with_node(&mut self) -> Self {
                        Self {
                            #(#variant1),*
                        }
                    }

                    fn clone_without_node(&self) -> Self {
                        Self {
                            #(#variant2),*
                        }
                    }
                }
            };

            expanded.into()
        }
        _ => panic!("unsupported"),
    }
}

enum ParseAttr {
    Token,
    Standard,
    FirstToken(Type),
}

impl Parse for ParseAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;

        Ok(match ident.to_string().as_str() {
            "token" => Self::Token,
            "standard" => Self::Standard,
            "first_token" => {
                let _: Token![=] = input.parse()?;
                Self::FirstToken(input.parse()?)
            }
            &_ => panic!("invalid def"),
        })
    }
}

impl From<&Vec<Attribute>> for ParseAttr {
    fn from(value: &Vec<Attribute>) -> Self {
        value
            .iter()
            .find(|a| a.path().is_ident("parse"))
            .map_or(ParseAttr::Standard, |x| x.parse_args().unwrap())
    }
}

#[proc_macro_derive(Parse, attributes(parse))]
pub fn derive_parse(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let attr = ParseAttr::from(&input.attrs);

    let name = create_type_from_def(&input.ident, &input.generics);
    let generics = &input.generics;

    let expanded = match &attr {
        ParseAttr::Token => match &input.data {
            Data::Struct(_) => quote! {
                impl #generics Parse for #name {
                    type FirstToken = #name;

                    fn parse<'t, I: Iterator<Item = &'t Token> + Clone>(
                        input: &mut crate::script::parser::InputStream<'t, I>,
                    ) -> Result<Self, Error> {
                        match input.get_token()? {
                            Token::#name(tok) => Ok(tok.clone()),
                            t => Err(Error::InvalidToken { token: t.clone() }),
                        }
                    }

                    fn get_span(&self) -> Span {
                        self.span
                    }
                }
            },
            Data::Enum(data) => {
                let variant = data.variants.iter().map(|x| &x.ident);

                quote! {
                    impl #generics Parse for #name {
                        type FirstToken = #name;

                        fn parse<'t, I: Iterator<Item = &'t Token> + Clone>(
                            input: &mut crate::script::parser::InputStream<'t, I>,
                        ) -> Result<Self, Error> {
                            match input.get_token()? {
                                Token::#name(tok) => Ok(tok.clone()),
                                t => Err(Error::InvalidToken { token: t.clone() }),
                            }
                        }

                        fn get_span(&self) -> Span {
                            match self {
                                #(Self::#variant(v) => v.span,)*
                            }
                        }
                    }
                }
            }
            Data::Union(_) => panic!("cannot be a token"),
        },
        ParseAttr::FirstToken(_) | ParseAttr::Standard => match &input.data {
            Data::Enum(data) => {
                let variant = data.variants.iter().map(|x| &x.ident);
                let variant_span = variant.clone();

                let first_token = if let ParseAttr::FirstToken(first) = &attr {
                    quote! {#first}
                } else {
                    let mut variant_type = data.variants.iter().map(|x| match &x.fields {
                        Fields::Unnamed(fields) => &fields.unnamed.first().unwrap().ty,
                        _ => unreachable!(),
                    });

                    let first_type = variant_type.next().unwrap();

                    let mut first_token = quote! {
                        <#first_type as Parse>::FirstToken
                    };

                    for ty in variant_type {
                        first_token = quote! {
                            TokenOr<Maybe<#first_token>, <#ty as Parse>::FirstToken>
                        }
                    }

                    first_token
                };

                quote! {
                    impl #generics Parse for #name {
                        type FirstToken = #first_token;

                        fn parse<'t, I: Iterator<Item = &'t Token> + Clone>(
                            input: &mut crate::script::parser::InputStream<'t, I>,
                        ) -> Result<Self, Error> {
                            input.expect_token()?;

                            #(
                                if let Some(v) = input.parse()? {
                                    Ok(Self::#variant(v))
                                } else
                            )* {
                                Err(Error::InvalidToken { token: input.get_token()?.clone() })
                            }
                        }

                        fn get_span(&self) -> Span {
                            match self {
                                #(Self::#variant_span(v) => v.get_span(),)*
                            }
                        }
                    }
                }
            }
            Data::Struct(data) => {
                let field = match &data.fields {
                    Fields::Named(fields) => fields.named.iter().map(|x| x.ident.as_ref().unwrap()),
                    _ => unreachable!("not allowed"),
                };
                let field_span = field.clone();

                let first_token = if let ParseAttr::FirstToken(first) = &attr {
                    quote! {#first}
                } else {
                    let mut field_type = match &data.fields {
                        Fields::Named(fields) => fields.named.iter().map(|x| &x.ty),
                        _ => unreachable!("not allowed"),
                    };

                    let first_type = field_type.next().unwrap();

                    let mut first_token = quote! {
                        <#first_type as Parse>::FirstToken
                    };

                    for ty in field_type {
                        first_token = quote! {
                            TokenOr<#first_token, <#ty as Parse>::FirstToken>
                        }
                    }

                    first_token
                };

                quote! {
                    impl #generics Parse for #name {
                        type FirstToken = #first_token;

                        fn parse<'t, I: Iterator<Item = &'t Token> + Clone>(
                            input: &mut crate::script::parser::InputStream<'t, I>,
                        ) -> Result<Self, Error> {
                            Ok(Self {
                                #(#field: input.parse()?,)*
                            })
                        }

                        fn get_span(&self) -> Span {
                            Span::empty()
                                #(.join(self.#field_span.get_span()))*
                        }
                    }
                }
            }
            _ => todo!(),
        },
    };

    expanded.into()
}

#[derive(Debug)]
enum InstrArgType {
    Line,
    Circle,
    Complex,
    Real
}

impl InstrArgType {
    fn construct_value(&self, block: &Block) -> proc_macro2::TokenStream {
        match self {
            Self::Line => quote! { Value { line: #block } },
            Self::Circle => quote! { Value { circle: #block } },
            Self::Complex => quote! { Value { complex: #block } },
            Self::Real => quote! { Value { complex: Complex::real(#block) } },
        }
    }
}

impl Parse for InstrArgType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?;

        Ok(match ident.to_string().as_str() {
            "Line" => Self::Line,
            "Circle" => Self::Circle,
            "Complex" => Self::Complex,
            "Real" => Self::Real,
            _ => return Err(syn::Error::new_spanned(ident, "invalid type"))
        })
    }
}

impl ToTokens for InstrArgType {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.extend(match self {
            Self::Line => quote! { line },
            Self::Circle => quote! { circle },
            Self::Complex => quote! { complex },
            Self::Real => quote! { complex.real },
        });
    }
}

#[derive(Debug)]
struct InstructionArg {
    name: Ident,
    _colon: Token![:],
    ty: InstrArgType
}

impl Parse for InstructionArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            name: input.parse()?,
            _colon: input.parse()?,
            ty: input.parse()?
        })
    }
}

#[derive(Debug)]
struct Instruction {
    name: Ident,
    _paren: Paren,
    args: Punctuated<InstructionArg, Token![,]>,
    _arrow: Token![->],
    returned: InstrArgType,
    code: Block
}

impl Parse for Instruction {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let params;
        let parsed = Self {
            name: input.parse()?,
            _paren: parenthesized!(params in input),
            args: params.parse_terminated(InstructionArg::parse, Token![,])?,
            _arrow: input.parse()?,
            returned: input.parse()?,
            code: input.parse()?
        };

        // println!("{parsed:?}");

        Ok(parsed)
    }
}

struct InstructionsInput {
    instructions: Vec<Instruction>
}

impl Parse for InstructionsInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut instructions = Vec::new();

        while !input.is_empty() {
            instructions.push(input.parse()?);
        }

        Ok(Self {
            instructions
        })
    }
}

/// Instruction generator macro
#[proc_macro]
pub fn instructions(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as InstructionsInput);

    let inst_code = input.instructions.into_iter().map(|inst| {
        let name = inst.name;
        let param_name1 = inst.args.iter().map(|x| &x.name);
        let param_name2 = param_name1.clone();
        let param_name3 = param_name1.clone();
        let param_type = inst.args.iter().map(|x| &x.ty);
        let value = inst.returned.construct_value(&inst.code);

        quote! {
            #[derive(Debug, Clone, Copy, Serialize)]
            pub struct #name {
                #(pub #param_name1: Loc,)*
                pub target: Loc
            }

            impl Execute for #name {
                unsafe fn execute(&self, memory: &mut [Value]) {
                    let (#(#param_name2),*) = unsafe {(
                        #(memory.get_unchecked(self.#param_name3).#param_type),*
                    )};

                    let value = #value;
                    *memory.get_unchecked_mut(self.target) = value;
                }
            }
        }
    });

    // panic!("{}", inst_code.next().unwrap());

    let expanded = quote! {#(#inst_code)*};
    expanded.into()
}
