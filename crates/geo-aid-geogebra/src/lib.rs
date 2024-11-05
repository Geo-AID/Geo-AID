//! Geo-AID is capable of outputting figures as a simple Geogebra file.
use std::io::{Seek, Write};

use geo_aid_figure::math_string::{MathChar, MathIndex, MathSpecial, MathString, SPECIAL_MATH};
use geo_aid_figure::{
    CircleItem, EntityIndex, EntityKind, Figure, Item, Label, LineItem, PointItem, Style,
    TwoPointItem, Value,
};
use geo_aid_figure::{Expression, ExpressionKind};

use geogebra_types::{prelude::*, Var};
use geogebra_types::{Geogebra as Ggb, LineStyle, LineType};

use num_traits::ToPrimitive;

/// The SVG format writer.
pub struct Geogebra<'f> {
    /// Geogebra workspace
    workspace: Ggb,
    /// Figure that is drawn
    figure: &'f Figure,
    /// Variables from the variable vector
    variables: Vec<VarKind>,
    /// Entities
    entities: Vec<Option<VarKind>>,
    /// Wether the entity should be drawn.
    draw_entities: Vec<Option<PointItem>>,
}

enum VarKind {
    Point(Var<Point>),
    Line(Var<Line>),
    Circle(Var<Conic>),
    Number(Var<Numeric>),
}

impl From<Var<Numeric>> for VarKind {
    fn from(v: Var<Numeric>) -> Self {
        Self::Number(v)
    }
}

impl From<Var<Conic>> for VarKind {
    fn from(v: Var<Conic>) -> Self {
        Self::Circle(v)
    }
}

impl From<Var<Line>> for VarKind {
    fn from(v: Var<Line>) -> Self {
        Self::Line(v)
    }
}

impl From<Var<Point>> for VarKind {
    fn from(v: Var<Point>) -> Self {
        Self::Point(v)
    }
}

impl VarKind {
    #[must_use]
    fn as_line(&self) -> Option<&Var<Line>> {
        if let Self::Line(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    fn as_circle(&self) -> Option<&Var<Conic>> {
        if let Self::Circle(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    fn as_point(&self) -> Option<&Var<Point>> {
        if let Self::Point(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    fn as_number(&self) -> Option<&Var<Numeric>> {
        if let Self::Number(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl<'f> Geogebra<'f> {
    /// Get the figure in SVG format.
    pub fn draw(figure: &'f Figure, writer: impl Write + Seek) -> Result<(), std::io::Error> {
        let mut ggb = Self {
            workspace: Ggb::default(),
            figure,
            variables: Vec::new(),
            entities: Vec::new(),
            draw_entities: Vec::new(),
        };

        ggb.draw_entities.resize(figure.entities.len(), None);
        ggb.entities.resize_with(figure.entities.len(), || None);

        // First, check what entities should be drawn by analyzing the items.
        for item in &figure.items {
            if let Item::Point(point) = item {
                if let ExpressionKind::Entity { id } = &figure.expressions[point.id.0].kind {
                    ggb.draw_entities[id.0] = Some(point.clone());
                }
            }
        }

        // Now, when drawing an entity, we'll have access to all required information.

        for var in &figure.expressions {
            ggb.load_expr(var);
        }

        for item in &figure.items {
            match item {
                Item::Point(point) => ggb.draw_point(point),
                Item::Line(line) => ggb.draw_line(line),
                Item::Ray(ray) => ggb.draw_ray(ray),
                Item::Segment(segment) => ggb.draw_segment(segment),
                Item::Circle(circle) => ggb.draw_circle(circle),
            }
        }

        ggb.workspace.write(writer)
    }
}

impl<'f> Geogebra<'f> {
    /// Convert the given math string into a LaTeX string.
    fn math_to_latex(math: &MathString) -> String {
        let mut s = String::new();

        for c in math.iter().copied() {
            match c {
                MathChar::Ascii(c) => s.push(c),
                MathChar::Special(special) => match special {
                    MathSpecial::Quote => s += "\"",
                    special => s += &format!("\\{}", SPECIAL_MATH[special.to_usize().unwrap()]),
                },
                MathChar::SetIndex(i) => {
                    s += match i {
                        MathIndex::Normal => "}",
                        MathIndex::Lower => "_{",
                    }
                }
                MathChar::Prime => s += "^{\\prime}",
            }
        }

        s
    }

    fn load_expr(&mut self, expr: &Expression) {
        let var = match &expr.kind {
            ExpressionKind::Entity { id } => {
                if self.entities[id.0].is_none() {
                    self.load_entity(*id);
                }

                match &self.entities[id.0].as_ref().unwrap() {
                    VarKind::Point(v) => self.workspace.var(v).into(),
                    VarKind::Line(v) => self.workspace.var(v).into(),
                    VarKind::Circle(v) => self.workspace.var(v).into(),
                    VarKind::Number(v) => self.workspace.var(v).into(),
                }
            }
            ExpressionKind::LineLineIntersection { k, l } => {
                let k = self.variables[k.0].as_line().unwrap();
                let l = self.variables[l.0].as_line().unwrap();
                self.workspace.var(Point::intersect(k, l)).into()
            }
            ExpressionKind::AveragePoint { items } => {
                let list: List<Point> = items
                    .iter()
                    .map(|v| self.variables[v.0].as_point().unwrap())
                    .into();

                let list_var = self.workspace.var(list);
                self.workspace
                    .var((list_var.mean_x(), list_var.mean_y()))
                    .into()
            }
            ExpressionKind::CircleCenter { circle } => {
                let circle = self.variables[circle.0].as_circle().unwrap();
                self.workspace.var(circle.center()).into()
            }
            ExpressionKind::ComplexToPoint { number } => {
                let number = self.variables[number.0].as_number().unwrap();
                self.workspace.var(number.point()).into()
            }
            ExpressionKind::Sum { plus, minus } => {
                let plus: List<Numeric> = plus
                    .iter()
                    .map(|v| self.variables[v.0].as_number().unwrap())
                    .into();

                let plus_var = self.workspace.var(plus);

                let minus: List<Numeric> = minus
                    .iter()
                    .map(|v| self.variables[v.0].as_number().unwrap())
                    .into();

                let minus_var = self.workspace.var(minus);

                self.workspace.var(plus_var.sum() - minus_var.sum()).into()
            }
            ExpressionKind::Product { times, by } => {
                let times: List<Numeric> = times
                    .iter()
                    .map(|v| self.variables[v.0].as_number().unwrap())
                    .into();

                let times_var: Var<List<Numeric>> = self.workspace.var(times);

                let by: List<Numeric> = by
                    .iter()
                    .map(|v| self.variables[v.0].as_number().unwrap())
                    .into();

                let by_var: Var<List<Numeric>> = self.workspace.var(by);

                self.workspace
                    .var(times_var.product() / by_var.product())
                    .into()
            }
            ExpressionKind::Const { value } => self
                .workspace
                .var(Numeric::complex(value.real, value.imaginary))
                .into(),
            ExpressionKind::Power { value, exponent } => {
                #[allow(clippy::cast_precision_loss)]
                let exponent = self
                    .workspace
                    .var((exponent.num as f64 / i64::from(exponent.denom) as f64).real());
                let value = self.variables[value.0].as_number().unwrap();

                self.workspace
                    .var(Numeric::complex(
                        value.real().pow(&exponent),
                        value.imaginary().pow(&exponent),
                    ))
                    .into()
            }
            ExpressionKind::PointPointDistance { p, q } => {
                let p = self.variables[p.0].as_point().unwrap();
                let q = self.variables[q.0].as_point().unwrap();

                self.workspace.var(Numeric::distance(p, q)).into()
            }
            ExpressionKind::PointLineDistance { point, line } => {
                let point = self.variables[point.0].as_point().unwrap();
                let line = self.variables[line.0].as_line().unwrap();

                self.workspace.var(Numeric::distance(point, line)).into()
            }
            ExpressionKind::ThreePointAngle { a, b, c } => {
                let a = self.variables[a.0].as_point().unwrap();
                let b = self.variables[b.0].as_point().unwrap();
                let c = self.variables[c.0].as_point().unwrap();

                self.workspace.var(Numeric::angle(a, b, c)).into()
            }
            ExpressionKind::ThreePointAngleDir { a, b, c } => {
                let a = self.variables[a.0].as_point().unwrap().complex();
                let b = self.variables[b.0].as_point().unwrap().complex();
                let c = self.variables[c.0].as_point().unwrap().complex();

                let div = (c - b.clone()) / (a - b);

                self.workspace.var(div.arg()).into()
            }
            ExpressionKind::TwoLineAngle { k, l } => {
                let k = self.variables[k.0].as_line().unwrap();
                let l = self.variables[l.0].as_line().unwrap();

                self.workspace.var(Numeric::angle_lines(k, l)).into()
            }
            ExpressionKind::PointX { point } => {
                let point = self.variables[point.0].as_point().unwrap();
                self.workspace.var(point.x()).into()
            }
            ExpressionKind::PointY { point } => {
                let point = self.variables[point.0].as_point().unwrap();
                self.workspace.var(point.y()).into()
            }
            ExpressionKind::PointToComplex { point } => {
                let point = self.variables[point.0].as_point().unwrap();
                self.workspace.var(point.complex()).into()
            }
            ExpressionKind::Real { number } => {
                let number = self.variables[number.0].as_number().unwrap();
                self.workspace.var(number.real()).into()
            }
            ExpressionKind::Imaginary { number } => {
                let number = self.variables[number.0].as_number().unwrap();
                self.workspace.var(number.imaginary()).into()
            }
            ExpressionKind::Log { number } => {
                let number = self.variables[number.0].as_number().unwrap();
                self.workspace.var(number.ln()).into()
            }
            ExpressionKind::Exp { number } => {
                let number = self.variables[number.0].as_number().unwrap();
                self.workspace.var(number.exp()).into()
            }
            ExpressionKind::Sin { angle } => {
                let angle = self.variables[angle.0].as_number().unwrap();
                self.workspace.var(angle.sin()).into()
            }
            ExpressionKind::Cos { angle } => {
                let angle = self.variables[angle.0].as_number().unwrap();
                self.workspace.var(angle.cos()).into()
            }
            ExpressionKind::Asin { value } => {
                let value = self.variables[value.0].as_number().unwrap();
                self.workspace.var(value.asin()).into()
            }
            ExpressionKind::Acos { value } => {
                let value = self.variables[value.0].as_number().unwrap();
                self.workspace.var(value.acos()).into()
            }
            ExpressionKind::Atan { value } => {
                let value = self.variables[value.0].as_number().unwrap();
                self.workspace.var(value.atan()).into()
            }
            ExpressionKind::Atan2 { y, x } => {
                let y = self.variables[y.0].as_number().unwrap();
                let x = self.variables[x.0].as_number().unwrap();
                self.workspace.var(Numeric::atan2(y, x)).into()
            }
            ExpressionKind::PointPoint { p, q } => {
                let p = self.variables[p.0].as_point().unwrap();
                let q = self.variables[q.0].as_point().unwrap();

                self.workspace.var(Line::new(p, q)).into()
            }
            ExpressionKind::AngleBisector { p, q, r } => {
                let p = self.variables[p.0].as_point().unwrap();
                let q = self.variables[q.0].as_point().unwrap();
                let r = self.variables[r.0].as_point().unwrap();

                self.workspace.var(Line::angle_bisector(p, q, r)).into()
            }
            ExpressionKind::PerpendicularThrough { point, line } => {
                let point = self.variables[point.0].as_point().unwrap();
                let line = self.variables[line.0].as_line().unwrap();

                self.workspace.var(Line::perpendicular(line, point)).into()
            }
            ExpressionKind::ParallelThrough { point, line } => {
                let point = self.variables[point.0].as_point().unwrap();
                let line = self.variables[line.0].as_line().unwrap();

                self.workspace.var(Line::parallel(line, point)).into()
            }
            ExpressionKind::ConstructCircle { center, radius } => {
                let center = self.variables[center.0].as_point().unwrap();
                let radius = self.variables[radius.0].as_number().unwrap();

                self.workspace.var(Conic::circle(center, radius)).into()
            }
        };

        self.variables.push(var);
    }

    fn load_entity(&mut self, id: EntityIndex) {
        let point = self.draw_entities[id.0].clone();

        let ent = self.figure.entities[id.0].clone();

        let var: VarKind = match &ent.kind {
            EntityKind::FreePoint => {
                let pos = if let Value::Complex(v) = ent.hint {
                    v
                } else {
                    panic!("Invalid hint type")
                };

                if let Some(item) = point {
                    let mut point = Point::from((pos.real, pos.imaginary));
                    point.set_display_label(item.label.is_some());
                    self.workspace.add(
                        point,
                        item.label
                            .map(|x| format!("${}$", Self::math_to_latex(&x.content)))
                            .unwrap_or_default(),
                    )
                } else {
                    self.workspace.var((pos.real, pos.imaginary))
                }
                .into()
            }
            EntityKind::PointOnLine { line } => {
                let pos = if let Value::Complex(v) = ent.hint {
                    v
                } else {
                    panic!("Invalid hint type")
                };

                if let Some(item) = point {
                    let mut point = Point::on(self.variables[line.0].as_line().unwrap());
                    point.set_display_label(item.label.is_some());
                    self.workspace.add_point(
                        point,
                        item.label
                            .map(|x| format!("${}$", Self::math_to_latex(&x.content)))
                            .unwrap_or_default(),
                        (pos.real, pos.imaginary),
                    )
                } else {
                    self.workspace.var((pos.real, pos.imaginary))
                }
                .into()
            }
            EntityKind::PointOnCircle { circle } => {
                let pos = if let Value::Complex(v) = ent.hint {
                    v
                } else {
                    panic!("Invalid hint type")
                };

                if let Some(item) = point {
                    let mut point = Point::on(self.variables[circle.0].as_circle().unwrap());
                    point.set_display_label(item.label.is_some());
                    self.workspace.add_point(
                        point,
                        item.label
                            .map(|x| format!("${}$", Self::math_to_latex(&x.content)))
                            .unwrap_or_default(),
                        (pos.real, pos.imaginary),
                    )
                } else {
                    self.workspace.var((pos.real, pos.imaginary))
                }
                .into()
            }
            EntityKind::DistanceUnit | EntityKind::FreeReal => {
                let num = if let Value::Complex(v) = ent.hint {
                    v
                } else {
                    panic!("Invalid hint type")
                };

                self.workspace
                    .var(Numeric::complex(num.real, num.imaginary))
                    .into()
            }
        };

        self.entities[id.0] = Some(var);
    }

    /// Get the style of a line
    fn get_style(style: Style) -> LineStyle {
        match style {
            Style::Solid => LineStyle::default(),
            Style::Dotted => LineStyle {
                type_: Some(LineType::Dotted),
                ..LineStyle::default()
            },
            Style::Bold => LineStyle {
                thickness: Some(8),
                ..LineStyle::default()
            },
            Style::Dashed => LineStyle {
                type_: Some(LineType::DashedLong),
                ..LineStyle::default()
            },
        }
    }

    fn get_caption(label: &Option<Label>) -> String {
        label
            .as_ref()
            .map(|x| format!("${}$", Self::math_to_latex(&x.content)))
            .unwrap_or_default()
    }

    fn draw_point(&mut self, point: &PointItem) {
        // Only draw this if it's not an entity
        if let ExpressionKind::Entity { id: _ } = self.figure.expressions[point.id.0].kind {
            return;
        }

        let v = self.variables[point.id.0].as_point().unwrap();

        self.workspace.add(
            v,
            point
                .label
                .as_ref()
                .map(|x| format!("${}$", Self::math_to_latex(&x.content)))
                .unwrap_or_default(),
        );
    }

    fn draw_line(&mut self, line: &LineItem) {
        let mut v = Line::from(self.variables[line.id.0].as_line().unwrap());
        v.set_style(Self::get_style(line.style));
        v.set_display_label(line.label.is_some());

        self.workspace.add(v, Self::get_caption(&line.label));
    }

    fn draw_ray(&mut self, ray: &TwoPointItem) {
        let origin = self.variables[ray.p_id.0].as_point().unwrap();
        let through = self.variables[ray.q_id.0].as_point().unwrap();

        let mut v = Ray::new(origin, through);
        v.set_style(Self::get_style(ray.style));
        v.set_display_label(ray.label.is_some());

        self.workspace.add(v, Self::get_caption(&ray.label));
    }

    fn draw_segment(&mut self, segment: &TwoPointItem) {
        let a = self.variables[segment.p_id.0].as_point().unwrap();
        let b = self.variables[segment.q_id.0].as_point().unwrap();

        let mut v = Segment::new(a, b);
        v.set_style(Self::get_style(segment.style));
        v.set_display_label(segment.label.is_some());

        self.workspace.add(v, Self::get_caption(&segment.label));
    }

    // fn draw_angle(&mut self, angle: &RenderedAngle) {
    //     let x: u32 = 45;
    //     self.content += &format!(
    //         r#"
    //             <g transform="translate({}, {}) rotate({}, 0, 0)" fill="transparent">
    //                 <path stroke-dasharray="{}" d="M {}, 0 A 45, 45, 0, 0, 0, {}, {}" stroke="black" stroke-width="{}"/>
    //             </g>
    //         "#,
    //         angle.points.1.real,
    //         angle.points.1.imaginary,
    //         geometry::get_line(angle.points.1, angle.points.0)
    //             .direction
    //             .arg()
    //             .to_degrees(),
    //         Self::get_style_dashing(angle.item),
    //         x, // It should probably be a constant. For now, we will leave it like this.
    //         angle.angle_value.cos() * 45.0,
    //         -angle.angle_value.sin() * 45.0,
    //         style_width(angle.style),
    //     );
    // }

    fn draw_circle(&mut self, circle: &CircleItem) {
        let mut v = Conic::from(self.variables[circle.id.0].as_circle().unwrap());
        v.set_style(Self::get_style(circle.style));
        v.set_display_label(circle.label.is_some());

        self.workspace.add(v, Self::get_caption(&circle.label));
    }
}
