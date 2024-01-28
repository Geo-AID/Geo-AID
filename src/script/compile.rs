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

// fn read_flags(flags: &HashMap<String, Flag>) -> Flags {
//     Flags {
//         optimizations: Optimizations {
//             identical_expressions: flags["optimizations"].as_set().unwrap()
//                 ["identical_expressions"]
//                 .as_bool()
//                 .unwrap(),
//         },
//         point_bounds: flags["point_bounds"].as_bool().unwrap(),
//     }
// }

// /// Inequality principle and the point plane limit.
// fn add_bounds(
//     points: &[(Arc<Expression<PointExpr>>, usize)],
//     criteria: &mut Vec<Criteria>,
//     flags: &Flags,
// ) {
//     // Point inequality principle.
//     for (i, (pt_i, adj)) in points.iter().enumerate() {
//         // For each of the next points, add an inequality rule.
//         for (pt_j, _) in points.iter().skip(i + 1) {
//             criteria.push(Criteria::new(
//                 CriteriaKind::Inverse(Box::new(CriteriaKind::EqualPoint(
//                     Arc::clone(pt_i),
//                     Arc::clone(pt_j),
//                 ))),
//                 FastFloat::One,
//             ));
//         }

//         if flags.point_bounds {
//             // For each point, add a rule limiting its range.
//             criteria.push(Criteria::new(
//                 CriteriaKind::Greater(
//                     Arc::new(Expression {
//                         weights: Weights::one_at(*adj),
//                         kind: ScalarExpr::PointX(PointX {
//                             point: Arc::clone(pt_i),
//                         }),
//                     }),
//                     Arc::new(Expression {
//                         weights: Weights::empty(),
//                         kind: ScalarExpr::Literal(Literal { value: 0.0 }),
//                     }),
//                 ),
//                 FastFloat::One,
//             )); // x > 0

//             criteria.push(Criteria::new(
//                 CriteriaKind::Greater(
//                     Arc::new(Expression {
//                         weights: Weights::one_at(*adj),
//                         kind: ScalarExpr::PointY(PointY {
//                             point: Arc::clone(pt_i),
//                         }),
//                     }),
//                     Arc::new(Expression {
//                         weights: Weights::empty(),
//                         kind: ScalarExpr::Literal(Literal { value: 1.0 }),
//                     }),
//                 ),
//                 FastFloat::One,
//             )); // y > 0

//             criteria.push(Criteria::new(
//                 CriteriaKind::Less(
//                     Arc::new(Expression {
//                         weights: Weights::one_at(*adj),
//                         kind: ScalarExpr::PointX(PointX {
//                             point: Arc::clone(pt_i),
//                         }),
//                     }),
//                     Arc::new(Expression {
//                         weights: Weights::empty(),
//                         kind: ScalarExpr::Literal(Literal { value: 1.0 }),
//                     }),
//                 ),
//                 FastFloat::One,
//             )); // x < 1

//             criteria.push(Criteria::new(
//                 CriteriaKind::Less(
//                     Arc::new(Expression {
//                         weights: Weights::one_at(*adj),
//                         kind: ScalarExpr::PointY(PointY {
//                             point: Arc::clone(pt_i),
//                         }),
//                     }),
//                     Arc::new(Expression {
//                         weights: Weights::empty(),
//                         kind: ScalarExpr::Literal(Literal { value: 1.0 }),
//                     }),
//                 ),
//                 FastFloat::One,
//             )); // y < 1
//         }
//     }
// }
