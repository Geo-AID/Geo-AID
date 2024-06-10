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
use crate::script::math::{HandleEntity, EntityKind, Number, Point, SimpleRule};
use crate::script::token::number::ProcNum;

use super::{Any, Circle, Expr, MathTypes, Rule, RuleKind};

/// If a free point is at distance 0 from a line, it should be turned into a line clip.
pub struct ZeroLineDst;

impl ZeroLineDst {
    pub fn process(rule: &mut Option<SimpleRule>, entities: &mut [EntityKind<MathTypes>]) -> bool {
        let Some(Rule { kind: RuleKind::NumberEq(a, b), .. }) = &rule
            else { return false };

        // If 'a' is a constant, swap references for the sake of latter processing.
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

        // Additional checks
        let mut on_line = None;

        match &entities[a.0] {
            EntityKind::PointOnLine(on_ln) => {
                if on_ln == ln {
                    // This rule is useless.
                    *rule = None;
                    return true;
                }

                on_line = Some(on_ln.clone());
            }
            _ => ()
        }

        // 'a' is a point entity with a zero distance from the line ln independent of 'a'.
        // Should it be beneficial, the rule should be deleted and the entity definition replaced.
        match &mut entities[a.0] {
            ent @ EntityKind::FreePoint => {
                *ent = EntityKind::PointOnLine(ln.clone());
            }
            ent @ EntityKind::PointOnLine(_) => {
                *ent = EntityKind::Bind(Any::Point(Point::LineLineIntersection {
                    k: on_line.unwrap(), l: ln.clone()
                }));
            }
            ent @ EntityKind::FreeReal => unreachable!(),
            EntityKind::PointOnCircle(_)
            | EntityKind::Bind(_) => return false
        }

        *rule = None;
        true
    }
}

/// If two sides of an equality are exactly the same, the rule should be omitted.
pub struct EqExpressions;

impl EqExpressions {
    pub fn process(rule: &mut Option<SimpleRule>) -> bool {
        let Some(Rule { kind: RuleKind::NumberEq(a, b), .. }) = rule else {
            let Some(Rule { kind: RuleKind::PointEq(a, b), .. }) = rule else {
                return false;
            };

            if a == b {
                *rule = None;
                return true;
            }

            return false;
        };

        if a == b {
            *rule = None;
            true
        } else {
            false
        }
    }
}

/// If two points are equally distant from a certain single point, they lie on a circle with the point as the origin.
pub struct EqPointDst;

impl EqPointDst {
    pub fn process(rule: &mut Option<SimpleRule>, entities: &mut [EntityKind<MathTypes>]) -> bool {
        let Some(Rule {
            kind: RuleKind::NumberEq(a, b),
            ..
        }) = rule else {return false};

        let Number::PointPointDistance { p, q } = a.kind.as_ref() else { return false };
        let Number::PointPointDistance { p: r, q: s } = b.kind.as_ref() else { return false };

        // q = s?
        if q != s {
            return false;
        }

        // r must be an entity (if there is one, it's definitely r, because normalization and ordering)
        let Point::Entity { id: r_id } = r.kind.as_ref() else { return false };

        // r must be a free point, otherwise it won't work.
        if !matches!(entities[r_id.0], EntityKind::FreePoint) {
            return false;
        }

        let circle = Expr::new(Circle::Construct {
            center: q.clone(),
            radius: Expr::new(Number::PointPointDistance { p: p.clone(), q: q.clone() })
        });
        entities[r_id.0] = EntityKind::PointOnCircle(circle);
        *rule = None;
        true
    }
}

/// If angle ABC is a right angle,
pub struct RightAngle;

impl RightAngle {
    pub fn process(rule: &mut Option<SimpleRule>) -> bool {
        let Some(Rule {
             kind: RuleKind::NumberEq(a, b),
             ..
         }) = rule else {return false};

        let Number::ThreePointAngle { p, q, r } = a.kind.as_ref() else { return false };
        let Number::Const { value } = b.kind.as_ref() else { return false };

        if *value != ProcNum::pi() {
            return false;
        }

        let mid = Expr::new(Point::Average {
            items: vec![p.clone(), r.clone()]
        });
        let p = p.clone();
        let q = q.clone();

        if let Some(rule) = rule {
            rule.kind = RuleKind::NumberEq(
                Expr::new(Number::PointPointDistance { p: mid.clone(), q }),
                Expr::new(Number::PointPointDistance { p, q: mid })
            );
        }

        true
    }
}