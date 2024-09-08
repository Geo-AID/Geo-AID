use crate::script::math::{
    ContainsEntity, DeepClone, EntityKind, ExprKind, ExprType, Math, VarIndex,
};
use crate::script::token::number::ProcNum;
use num_traits::{FromPrimitive, Zero};

use super::{Rule, RuleKind};

/// If a free point is at distance 0 from a line, it should be turned into a line clip.
pub struct ZeroLineDst;

impl ZeroLineDst {
    pub fn process(rule: &mut Option<Rule>, math: &mut Math) -> bool {
        let Some(Rule {
            kind: RuleKind::NumberEq(a, b),
            ..
        }) = &rule
        else {
            return false;
        };

        // If 'a' is a constant, swap references for the sake of later processing.
        let (a, b) = if let ExprKind::Const { .. } = &math.at(a).kind {
            (b, a)
        } else {
            (a, b)
        };

        let ExprKind::PointLineDistance { point: a, line: ln } = &math.at(a).kind else {
            return false;
        };

        let &ExprKind::Entity { id: a } = &math.at(a).kind else {
            return false;
        };

        let ExprKind::Const { value: b } = &math.at(b).kind else {
            return false;
        };

        if !b.is_zero() {
            return false;
        }

        if ln.contains_entity(a, math) {
            return false;
        }

        // Additional checks
        let mut on_line = None;

        if let EntityKind::PointOnLine { line: on_ln } = &math.entities[a.0] {
            if math.at(on_ln) == math.at(ln) {
                // This rule is useless.
                *rule = None;
                return true;
            }

            on_line = Some(on_ln.clone());
        }

        // 'a' is a point entity with a zero distance from the line ln independent of 'a'.
        // Should it be beneficial, the rule should be deleted and the entity definition replaced.
        let ent = math.entities[a.0].clone();
        match ent {
            EntityKind::FreePoint => {
                let ln = ln.clone();
                math.entities[a.0] = EntityKind::PointOnLine { line: ln }; // The rule is going to be removed, so this is essentially like moving.
            }
            EntityKind::PointOnLine { .. } => {
                let ln = ln.clone();
                let expr = math.store(
                    ExprKind::LineLineIntersection {
                        k: on_line.unwrap(),
                        l: ln, // We're moving ln here
                    },
                    ExprType::Point,
                );
                math.entities[a.0] = EntityKind::Bind(expr);
            }
            EntityKind::DistanceUnit | EntityKind::FreeReal => unreachable!(),
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

        if math.at(a) == math.at(b) {
            *rule = None;
            true
        } else {
            false
        }
    }
}

/// If a distance between two points is equal to something, maybe we can turn this into a circle.
pub struct EqPointDst;

impl EqPointDst {
    // Assumes that if rule has an entity, it's on the left of dst(a, b) = c
    fn process_pp_of_ent_x(
        a: VarIndex,
        b: VarIndex,
        c: VarIndex,
        math: &mut Math,
    ) -> Result<(), (VarIndex, VarIndex, VarIndex)> {
        let &ExprKind::Entity { id } = &math.at(&a).kind else {
            return Err((a, b, c));
        };

        if b.contains_entity(id, math) || c.contains_entity(id, math) {
            return Err((a, b, c));
        }

        // Check if `id` is viable for encircling
        let ent = math.entities[id.0].clone();
        if !matches!(ent, EntityKind::FreePoint) {
            return Err((a, b, c));
        }

        // We can make it a circle now.
        let circle = math.store(
            ExprKind::ConstructCircle {
                center: b, // We're moving b
                radius: c, // We're also moving c
            },
            ExprType::Circle,
        );
        math.entities[id.0] = EntityKind::PointOnCircle { circle };

        Ok(())
    }

    // Assumes that if rule compares a pp distance, the distance on left of a = b
    fn process_pp_x(a: &VarIndex, b: VarIndex, math: &mut Math) -> bool {
        let ExprKind::PointPointDistance { p, q } = &math.at(a).kind else {
            return false;
        };

        // Check both ways
        Self::process_pp_of_ent_x(p.clone(), q.clone(), b, math)
            .or_else(|(p, q, b)| Self::process_pp_of_ent_x(q, p, b, math))
            .is_ok()
    }

    pub fn process(rule: &mut Option<Rule>, math: &mut Math) -> bool {
        let Some(Rule {
            kind: RuleKind::NumberEq(a, b),
            ..
        }) = rule
        else {
            return false;
        };

        if Self::process_pp_x(a, b.clone(), math) || Self::process_pp_x(b, a.clone(), math) {
            *rule = None;
            true
        } else {
            false
        }
    }
}

/// If angle ABC is a right angle,
pub struct RightAngle;

impl RightAngle {
    #[allow(clippy::many_single_char_names)]
    pub fn process(rule: &mut Option<Rule>, math: &mut Math) -> bool {
        let Some(Rule {
            kind: RuleKind::NumberEq(a, b),
            ..
        }) = rule
        else {
            return false;
        };

        let ExprKind::ThreePointAngle { p, q, r } = &math.at(a).kind else {
            return false;
        };
        let ExprKind::Const { value } = &math.at(b).kind else {
            return false;
        };

        if *value != ProcNum::pi() / &ProcNum::from_u64(2).unwrap() {
            return false;
        }

        let (p, q, r) = (p.clone(), q.clone(), r.clone());
        let p_cloned = p.deep_clone(math);

        let mid = math.store(
            ExprKind::AveragePoint {
                items: vec![p.clone(), r.clone()], // Here again, moving the values
            },
            ExprType::Point,
        );
        let mid_cloned = mid.deep_clone(math);
        let p = p_cloned;

        if let Some(rule) = rule {
            rule.kind = RuleKind::NumberEq(
                math.store(ExprKind::PointPointDistance { p: mid, q }, ExprType::Number),
                math.store(
                    ExprKind::PointPointDistance { p, q: mid_cloned },
                    ExprType::Number,
                ),
            );
        }

        true
    }
}
