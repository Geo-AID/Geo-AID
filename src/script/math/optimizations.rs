/*
 Copyright (c) 2024 Michał Wilczek, Michał Margos

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

use num_traits::Zero;
use crate::script::math::{HandleEntity, Entity, Number, Point, Rule, SimpleRule};

use super::ExprTypes;

pub struct ZeroLineDst;

impl ZeroLineDst {
    pub fn process(rule: &mut Option<SimpleRule>, entities: &mut [Entity<ExprTypes<()>>]) -> bool {
        let Some(Rule::NumberEq(a, b)) = rule
            else { return false };

        // If 'a' is a constant, swab references for the sake of latter processing.
        let (a, b) = if let Number::Const { .. } = a.kind.as_ref() {
            (b, a)
        } else {
            (a, b)
        };

        let Number::PointLineDistance { p: a, k: ln } = a.kind.as_ref()
            else { return false };

        let Point::Entity { id: a } = a.kind.as_ref()
            else { return false };

        let Number::Const { value: b } = b.kind.as_ref()
            else { return false };

        if !b.is_zero() {
            return false;
        }

        if !ln.contains_entity(*a, entities) {
            return false;
        }

        // 'a' is a point entity with a zero distance from the line ln independent of 'a'.
        // Should it be beneficial, the rule should be deleted and the entity definition replaced.
        if entities[*a].degree() <= 1 {
            return false;
        }

        entities[*a] = Entity::PointOnLine(ln.clone());
        *rule = None;
    }
}