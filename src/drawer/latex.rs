use std::string::String;
use num_traits::ToPrimitive;

use crate::projector::{
    Output, RenderedCircle, RenderedLine, RenderedPoint, RenderedRay,
    RenderedSegment,
};

use crate::script::figure::Style::{self, Bold, Dashed, Dotted, Solid};
use crate::script::figure::{self, MathChar, MathIndex, MathSpecial, MathString};
use crate::drawer::Draw;
use crate::geometry::Complex;

#[derive(Debug, Default)]
pub struct Latex {
    content: String,
    scale: f64,
}

impl Latex {
    fn math_to_latex(math: &MathString) -> String {
        let mut s = String::new();

        for c in &math.chars {
            match c {
                MathChar::Ascii(c) => s.push(*c),
                MathChar::Special(special) => match special {
                    MathSpecial::Quote => s += "\"",
                    special => s += &format!("\\{}", figure::SPECIAL_MATH[special.to_usize().unwrap()])
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
            Dotted => "dotted",
            Dashed => "dashed",
            Bold => "ultra thick",
            Solid => "thin",
        }
    }

    fn draw_simple_segment(&mut self, points: &(Complex, Complex), style: Style) {
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
            pos1.real,
            pos1.imaginary,
            pos2.real,
            pos2.imaginary,
            Self::get_style_name(style)
        );
    }
}

impl Draw for Latex {
    fn begin(&mut self, output: &Output) {
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
        let size = (output.canvas_size.0 as f64, output.canvas_size.1 as f64);

        self.scale = f64::min(10.0/size.0, 10.0/size.1);
    }

    fn draw_point(&mut self, point: &RenderedPoint) {
        let pos = point.position * self.scale;
        let label_pos = point.label_position * self.scale + pos;
        let id = format!("expr{}", point.item.id);

        let label = Self::math_to_latex(&point.item.label);
        self.content += &format!(
            r#"
                \coordinate ({}) at ({}, {}); \fill[black] ({}) circle (1pt);
                \node at ({}, {}) {{${}$}}; 
            "#,
            id, pos.real, pos.imaginary, id, label_pos.real, label_pos.imaginary, label
        );
    }

    fn draw_line(&mut self, line: &RenderedLine) {
        self.draw_simple_segment(&line.points, line.item.style);
    }

    fn draw_ray(&mut self, ray: &RenderedRay) {
        self.draw_simple_segment(&ray.points, ray.item.style);
    }

    fn draw_segment(&mut self, segment: &RenderedSegment) {
        self.draw_simple_segment(&segment.points, segment.item.style);
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

    fn draw_circle(&mut self, circle: &RenderedCircle) {
        let pos1 = circle.center * self.scale;
        let pos2 = (circle.center + circle.radius) * self.scale;
        
        self.content += &format!(
            r#"
            \begin{{scope}}
                \coordinate (A) at ({}, {});
                \coordinate (B) at ({}, {});
                    \tkzDrawCircle[{}](A,B)
            \end{{scope}}
            "#,
            pos1.real,
            pos1.imaginary,
            pos2.real,
            pos2.imaginary,
            Self::get_style_name(circle.item.style)
        );
    }

    fn end(&mut self) -> &str {
        self.content += "\\end{tikzpicture} \\end{document}";
        &self.content
    }
}
