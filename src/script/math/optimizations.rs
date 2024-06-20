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

use crate::script::math::{EntityKind, ExprKind, ContainsEntity, ExprType, DeepClone, Math};
use crate::script::token::number::ProcNum;
use num_traits::Zero;

use super::{Rule, RuleKind};

/// If a free point is at distance 0 from a line, it should be turned into a line clip.
pub struct ZeroLineDst;

impl ZeroLineDst {
    pub fn process(
        rule: &mut Option<Rule>,
        math: &mut Math,
    ) -> bool {
        let Some(Rule {
            kind: RuleKind::NumberEq(a, b),
            ..
        }) = &rule
        else {
            return false;
        };

        // If 'a' is a constant, swap references for the sake of later processing.
        let (a, b) = if let ExprKind::Const { .. } = &math.at(*a).kind {
            (b, a)
        } else {
            (a, b)
        };

        let &ExprKind::PointLineDistance { point: a, line: ln } = &math.at(*a).kind else {
            return false;
        };

        let &ExprKind::Entity { id: a } = &math.at(a).kind else {
            return false;
        };

        let ExprKind::Const { value: b } = &math.at(*b).kind else {
            return false;
        };

        if !b.is_zero() {
            return false;
        }

        if !ln.contains_entity(a, math) {
            return false;
        }

        // Additional checks
        let mut on_line = None;

        if let EntityKind::PointOnLine { line: on_ln } = &math.entities[a.0] {
            if math.at(*on_ln) == math.at(ln) {
                // This rule is useless.
                *rule = None;
                return true;
            }

            on_line = Some(*on_ln);
        }

        // 'a' is a point entity with a zero distance from the line ln independent of 'a'.
        // Should it be beneficial, the rule should be deleted and the entity definition replaced.
        let ent = math.entities[a.0].clone(); // It's a copy, really
        match ent {
            EntityKind::FreePoint => {
                math.entities[a.0] = EntityKind::PointOnLine { line: ln }; // The rule is going to be removed, so this is essentially like moving.
            }
            EntityKind::PointOnLine { .. } => {
                let expr = math.store(ExprKind::LineLineIntersection {
                    k: on_line.unwrap(),
                    l: ln, // We're moving ln here
                }, ExprType::Point);
                math.entities[a.0] = EntityKind::Bind(expr);
            }
            EntityKind::FreeReal => unreachable!(),
            EntityKind::PointOnCircle { .. } | EntityKind::Bind(_) => return false,
        }

        *rule = None;
        true
    }
}

/// If two sides of an equality are exactly the same, the rule should be omitted.
pub struct EqExpressions;

impl EqExpressions {
    pub fn process(rule: &mut Option<Rule>, math: &Math) -> bool {
        let Some(Rule {
            kind: RuleKind::NumberEq(a, b) | RuleKind::PointEq(a, b),
            ..
        }) = rule
        else {
            return false;
        };

        if math.at(*a) == math.at(*b) {
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
    pub fn process(rule: &mut Option<Rule>, math: &mut Math) -> bool {
        let Some(Rule {
            kind: RuleKind::NumberEq(a, b),
            ..
        }) = rule
        else {
            return false;
        };

        let &ExprKind::PointPointDistance { p, q } = &math.at(*a).kind else {
            return false;
        };
        let &ExprKind::PointPointDistance { p: r, q: s } = &math.at(*b).kind else {
            return false;
        };

        // q = s?
        let (r, s) = if math.compare(q, s).is_eq() {
            (r, s)
        } else if math.compare(q, r).is_eq() {
            // If not, check q=r? and if so, swap r and s.
            (s, r)
        } else {
            return false;
        };

        // p must be an entity (if there is one, it's definitely p, because normalization and ordering)
        let &ExprKind::Entity { id: p_id } = &math.at(p).kind else {
            return false;
        };

        // p must be a free point, otherwise it won't work.
        if !matches!(math.entities[p_id.0], EntityKind::FreePoint) {
            return false;
        }

        // p is an entity that is as distant to q=s as r.

        let radius = math.store(ExprKind::PointPointDistance {
            p: r, // "moving" r here
            q, // "moving" q
        }, ExprType::Number);
        let circle = math.store(ExprKind::ConstructCircle {
            center: s, // "moving" s=q
            radius,
        }, ExprType::Circle);
        math.entities[p_id.0] = EntityKind::PointOnCircle { circle };
        *rule = None;
        true
    }
}

/// If angle ABC is a right angle,
pub struct RightAngle;

impl RightAngle {
    pub fn process(rule: &mut Option<Rule>, math: &mut Math) -> bool {
        let Some(Rule {
            kind: RuleKind::NumberEq(a, b),
            ..
        }) = rule
        else {
            return false;
        };

        let &ExprKind::ThreePointAngle { p, q, r } = &math.at(*a).kind else {
            return false;
        };
        let ExprKind::Const { value } = &math.at(*b).kind else {
            return false;
        };

        if *value != ProcNum::pi() {
            return false;
        }

        let p_cloned = p.deep_clone(math);

        let mid = math.store(ExprKind::AveragePoint {
            items: vec![p, r], // Here again, moving the values
        }, ExprType::Point);
        let mid_cloned = mid.deep_clone(math);
        let p = p_cloned;

        if let Some(rule) = rule {
            rule.kind = RuleKind::NumberEq(
                math.store(ExprKind::PointPointDistance { p: mid, q }, ExprType::Number),
                math.store(ExprKind::PointPointDistance { p, q: mid_cloned }, ExprType::Number),
            );
        }

        true
    }
}
