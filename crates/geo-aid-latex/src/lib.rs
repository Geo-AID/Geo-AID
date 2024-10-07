//! Geo-AID is capable of outputting its figures in LaTeX, using tikz and tikz-euclide packages.

use geo_aid_figure::math_string::{MathChar, MathIndex, MathSpecial, MathString, SPECIAL_MATH};
use geo_aid_figure::{
    CircleItem, Figure, Item, LineItem, PointItem, Position, Style, TwoPointItem,
};
use num_traits::ToPrimitive;
use std::string::String;

/// The LaTeX format writer.
#[derive(Debug, Default)]
pub struct Latex {
    /// The current file contents
    content: String,
}

impl Latex {
    /// Get the figure in LaTeX format.
    #[must_use]
    pub fn draw(figure: &Figure) -> String {
        let mut latex = Self::default();

        latex.begin();

        for item in &figure.items {
            match item {
                Item::Point(point) => latex.draw_point(point),
                Item::Line(line) => latex.draw_line(line),
                Item::Ray(ray) => latex.draw_ray(ray),
                Item::Segment(segment) => latex.draw_segment(segment),
                Item::Circle(circle) => latex.draw_circle(circle),
            }
        }

        latex.end();

        latex.content
    }

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

    /// Get the tikz name of the requested [`Style`]
    fn get_style_name(style: Style) -> &'static str {
        match style {
            Style::Dotted => "dotted",
            Style::Dashed => "dashed",
            Style::Bold => "ultra thick",
            Style::Solid => "thin",
        }
    }

    /// Draw a styled segment delimited by two points.
    fn draw_simple_segment(&mut self, points: &(Position, Position), style: Style) {
        self.content += &format!(
            r#"
                \begin{{scope}}
                    \coordinate (A) at ({},{});
                    \coordinate (B) at ({},{});
                    \tkzDrawSegment[{}](A,B)
                \end{{scope}}
            "#,
            points.0.x,
            points.0.y,
            points.1.x,
            points.1.y,
            Self::get_style_name(style)
        );
    }

    fn begin(&mut self) {
        self.content = String::from(
            r"
                \documentclass{article}
                \usepackage{tikz}
                \usepackage{tkz-euclide}
                \usetikzlibrary {angles,calc,quotes}
                \begin{document}
                \begin{tikzpicture}
            ",
        );
    }

    fn draw_point(&mut self, point: &PointItem) {
        let pos = point.position;
        let id = format!("expr{}", point.id.0);

        self.content += &format!(
            r#"
                \coordinate ({}) at ({}, {}); \fill[black] ({}) circle (1pt);
            "#,
            id, pos.x, pos.y, id
        );

        if let Some(label) = &point.label {
            let label_pos = label.position;

            self.content += &format!(
                r#"
                \node at ({}, {}) {{${}$}};
            "#,
                label_pos.x,
                label_pos.y,
                Self::math_to_latex(&label.content)
            );
        }
    }

    fn draw_line(&mut self, line: &LineItem) {
        self.draw_simple_segment(&line.points, line.style);
    }

    fn draw_ray(&mut self, ray: &TwoPointItem) {
        self.draw_simple_segment(&ray.points, ray.style);
    }

    fn draw_segment(&mut self, segment: &TwoPointItem) {
        self.draw_simple_segment(&segment.points, segment.style);
    }

    // fn draw_angle(&mut self, angle: &RenderedAngle) {
    //     let no_arcs = String::from("l"); // Requires a change later! It has to be based on info from the script
    //
    //     self.content += &match &angle.expr.kind {
    //         ScalarExpr::AnglePoint(AnglePoint { arm1, origin, arm2 }) => {
    //             format!(
    //                 r#"
    //                 \begin{{scope}}
    //                     \coordinate (A) at {};
    //                     \coordinate (B) at {};
    //                     \coordinate (C) at {};
    //                         \tkzMarkAngle[size = 0.5,mark = none,arc={no_arcs},mkcolor = black, {}](A,B,C)
    //                 \end{{scope}}
    //                 "#,
    //                 get_point_name(arm1, output, angle.points.0, self.scale),
    //                 get_point_name(origin, output, angle.points.1, self.scale),
    //                 get_point_name(arm2, output, angle.points.2, self.scale),
    //                 styling(angle.style)
    //             )
    //         }
    //         // There are hard coded values in \coordinate, it is intentional, every point has it's label marked by Rendered::Point sequence above
    //         ScalarExpr::AngleLine(_) => {
    //             format!(
    //                 r#"
    //                 \begin{{scope}}
    //                     \coordinate (A) at ({}, {});
    //                     \coordinate (B) at ({}, {});
    //                     \coordinate (C) at ({}, {});
    //                         \tkzMarkAngle[size = 2,mark = none,arc={no_arcs},mkcolor = black, {}](A,B,C)
    //                 \end{{scope}}
    //                 "#,
    //                 angle.points.0.real,
    //                 angle.points.0.imaginary,
    //                 angle.points.1.real,
    //                 angle.points.1.imaginary,
    //                 angle.points.2.real,
    //                 angle.points.2.imaginary,
    //                 styling(angle.style)
    //             )
    //         }
    //         _ => unreachable!(),
    //     };
    // }fn draw_angle(&mut self, angle: &RenderedAngle) {
    //     let no_arcs = String::from("l"); // Requires a change later! It has to be based on info from the script
    //
    //     self.content += &match &angle.expr.kind {
    //         ScalarExpr::AnglePoint(AnglePoint { arm1, origin, arm2 }) => {
    //             format!(
    //                 r#"
    //                 \begin{{scope}}
    //                     \coordinate (A) at {};
    //                     \coordinate (B) at {};
    //                     \coordinate (C) at {};
    //                         \tkzMarkAngle[size = 0.5,mark = none,arc={no_arcs},mkcolor = black, {}](A,B,C)
    //                 \end{{scope}}
    //                 "#,
    //                 get_point_name(arm1, output, angle.points.0, self.scale),
    //                 get_point_name(origin, output, angle.points.1, self.scale),
    //                 get_point_name(arm2, output, angle.points.2, self.scale),
    //                 styling(angle.style)
    //             )
    //         }
    //         // There are hard coded values in \coordinate, it is intentional, every point has it's label marked by Rendered::Point sequence above
    //         ScalarExpr::AngleLine(_) => {
    //             format!(
    //                 r#"
    //                 \begin{{scope}}
    //                     \coordinate (A) at ({}, {});
    //                     \coordinate (B) at ({}, {});
    //                     \coordinate (C) at ({}, {});
    //                         \tkzMarkAngle[size = 2,mark = none,arc={no_arcs},mkcolor = black, {}](A,B,C)
    //                 \end{{scope}}
    //                 "#,
    //                 angle.points.0.real,
    //                 angle.points.0.imaginary,
    //                 angle.points.1.real,
    //                 angle.points.1.imaginary,
    //                 angle.points.2.real,
    //                 angle.points.2.imaginary,
    //                 styling(angle.style)
    //             )
    //         }
    //         _ => unreachable!(),
    //     };
    // }

    fn draw_circle(&mut self, circle: &CircleItem) {
        let pos1 = circle.center;
        let pos2 = circle.center
            + Position {
                x: circle.radius,
                y: 0.0,
            };

        self.content += &format!(
            r#"
            \begin{{scope}}
                \coordinate (A) at ({}, {});
                \coordinate (B) at ({}, {});
                    \tkzDrawCircle[{}](A,B)
            \end{{scope}}
            "#,
            pos1.x,
            pos1.y,
            pos2.x,
            pos2.y,
            Self::get_style_name(circle.style)
        );
    }

    fn end(&mut self) -> &str {
        self.content += "\\end{tikzpicture} \\end{document}";
        &self.content
    }
}
