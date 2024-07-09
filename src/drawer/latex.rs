use std::string::String;
use num_traits::ToPrimitive;
use geo_aid_figure::math_string::{MathChar, MathIndex, MathSpecial, MathString, SPECIAL_MATH};
use geo_aid_figure::{CircleItem, Figure, LineItem, PointItem, Position, Style, TwoPointItem};
use crate::drawer::Draw;

#[derive(Debug, Default)]
pub struct Latex {
    content: String,
    scale: f64,
}

impl Latex {
    fn math_to_latex(math: &MathString) -> String {
        let mut s = String::new();

        for c in math.iter().copied() {
            match c {
                MathChar::Ascii(c) => s.push(c),
                MathChar::Special(special) => match special {
                    MathSpecial::Quote => s += "\"",
                    special => s += &format!("\\{}", SPECIAL_MATH[special.to_usize().unwrap()])
                },
                MathChar::SetIndex(i) => s += match i {
                    MathIndex::Normal => "}",
                    MathIndex::Lower => "_{"
                },
                MathChar::Prime => s += "^{\\prime}"
            }
        }

        s
    }

    fn get_style_name(style: Style) -> &'static str {
        match style {
            Style::Dotted => "dotted",
            Style::Dashed => "dashed",
            Style::Bold => "ultra thick",
            Style::Solid => "thin",
        }
    }

    fn draw_simple_segment(&mut self, points: &(Position, Position), style: Style) {
        let pos1 = points.0 * self.scale;
        let pos2 = points.1 * self.scale;

        self.content += &format!(
            r#"
                \begin{{scope}}
                    \coordinate (A) at ({},{});
                    \coordinate (B) at ({},{});
                    \tkzDrawSegment[{}](A,B)
                \end{{scope}}
            "#,
            pos1.x,
            pos1.y,
            pos2.x,
            pos2.y,
            Self::get_style_name(style)
        );
    }
}

impl Draw for Latex {
    fn begin(&mut self, figure: &Figure) {
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
        #[allow(clippy::cast_precision_loss)]
        let size = (figure.width.get() as f64, figure.height.get() as f64);

        self.scale = f64::min(10.0/size.0, 10.0/size.1);
    }

    fn draw_point(&mut self, point: &PointItem) {
        let pos = point.position * self.scale;
        let id = format!("expr{}", point.id);

        self.content += &format!(
            r#"
                \coordinate ({}) at ({}, {}); \fill[black] ({}) circle (1pt);
            "#,
            id, pos.x, pos.y, id
        );

        if let Some(label) =& point.label {
            let label_pos = label.position * self.scale;

            self.content += &format!(
                r#"
                \node at ({}, {}) {{${}$}};
            "#,
                label_pos.x, label_pos.y, Self::math_to_latex(&label.content)
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
        let pos1 = circle.center * self.scale;
        let pos2 = (circle.center + Position { x: circle.radius, y: 0.0 }) * self.scale;
        
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
