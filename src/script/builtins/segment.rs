/*
Copyright (c) 2023 Michał Wilczek, Michał Margos

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the “Software”), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

use crate::{script::{parser::PropertyValue, token::StrLit}, span};

use super::prelude::*;
use geo_aid_derive::overload;

fn segment_function_point_point(
    mut a: Expr<Point>,
    mut b: Expr<Point>,
    context: &CompileContext,
    mut display: Properties,
) -> Expr<Bundle> {
    let mut expr = construct_bundle!(Segment { A: a, B: b });

    if let Some(node) = &mut expr.node {
        display.ignore("default-label");
        node.root.display = display.get("display").maybe_unset(true);

        let display_segment = display.get("display_segment").maybe_unset(true);
        let style = display.get("style").maybe_unset(Style::default());

        node.insert_data("display_segment", display_segment);
        node.insert_data("style", style);
        node.set_associated(Associated);
    }

    display.finish(context);

    expr
}

/// ```
/// struct Associated {
///     display_segment: bool,
///     style: Mode
/// }
/// ```
#[derive(Debug)]
pub struct Associated;

impl BuildAssociated<BundleNode> for Associated {
    fn build_associated(
        self: Box<Self>,
        compiler: &mut Compiler,
        figure: &mut Figure,
        associated: &mut HierarchyNode<BundleNode>,
    ) {
        let display_segment = associated
            .get_data("display_segment")
            .unwrap()
            .as_bool()
            .unwrap();
        let style = associated.get_data("style").unwrap().as_style().unwrap();

        if display_segment.unwrap() {
            figure.segments.push((
                compiler.compile(associated.root.children["A"].as_point().unwrap()),
                compiler.compile(associated.root.children["B"].as_point().unwrap()),
                style.unwrap(),
            ));
        }
    }
}

#[allow(clippy::needless_pass_by_value)]
fn len(segment: Expr<Bundle>, context: &CompileContext, mut display: Properties) -> Expr<Scalar> {
    display.add_if_not_present("display_segment", PropertyValue::String(StrLit {
        span: span!(0, 0, 0,0 ),
        content: String::from("false")
    }));

    super::dst::distance_function_pp(
        field!(no-node POINT segment, A with context),
        field!(no-node POINT segment, B with context),
        context,
        display
    )
}

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("Segment"),
        Function {
            overloads: vec![
                overload!((2-P) -> Segment {
                    |mut col: Expr<PointCollection>, context, display| call!(context:segment_function_point_point(
                        index!(node col, 0),
                        index!(node col, 1)
                    ) with display)
                }),
                overload!((POINT, POINT) -> Segment : segment_function_point_point),
            ],
        },
    );

    library.functions.insert(
        String::from("[Segment]::len"),
        Function {
            overloads: vec![overload!((Segment) -> Scalar : len)]
        }
    );

    library.bundles.insert("Segment", ["A", "B"].into());
}
