use geo_aid_figure::{CircleItem, Figure, LineItem, PointItem, Position, Style, TwoPointItem};

use crate::drawer::Draw;

#[derive(Debug, Default)]
pub struct Svg {
    content: String
}

impl Svg {
    fn get_style_width(style: Style) -> &'static str {
        match style {
            Style::Dashed | Style::Dotted => "0.5",
            Style::Bold => "2",
            Style::Solid => "1",
        }
    }

    fn get_style_dashing(style: Style) -> &'static str {
        match style {
            Style::Dotted => "0.8,1",
            Style::Dashed => "2,2",
            Style::Bold | Style::Solid => "1,0",
        }
    }

    fn draw_simple_segment(&mut self, (p1, p2): (Position, Position), style: Style) {
        self.content += &format!(
            r#"
                <line stroke-width="{}" stroke-dasharray="{}" stroke="black" x1="{}" x2="{}" y1="{}" y2="{}"/>
            "#,
            Self::get_style_width(style),
            Self::get_style_dashing(style),
            p1.x,
            p2.x,
            p1.y,
            p2.y
        );
    }
}

impl Draw for Svg {
    fn begin(&mut self, figure: &Figure) {
        self.content += &format!(
            r#"
                <svg height="{}" width="{}" xmlns="http://www.w3.org/2000/svg">
                    <font>
                        <font-face font-family="New Computer Modern">
                        </font-face>
                    </font>
                <g transform="translate(0,{})">
                <g transform="scale(1,-1)">
            "#,
            figure.width, figure.height, figure.width,
        );
    }

    fn draw_point(&mut self, point: &PointItem) {
        let pos = point.position;
        self.content += &format!(
            r#"<circle cx="{}" cy="{}" fill="black" r="1"/>"#,
            pos.x, pos.y
        );

        if let Some(label) = &point.label {
            self.content += &format!(
                r#"
                <text transform="scale(1,-1)"
                    text-anchor="middle" dominant-baseline="middle"
                    style="font-family: 'Computer Modern'" font-size="10px"
                    stroke="black" stroke-width="0" x="{}" y="-{}">{}
                </text>
            "#,
                label.position.x,
                label.position.y,
                label.content
            );
        }
    }

    fn draw_line(&mut self, line: &LineItem) {
        self.draw_simple_segment(line.points, line.style);
    }

    fn draw_ray(&mut self, ray: &TwoPointItem) {
        self.draw_simple_segment(ray.points, ray.style);
    }

    fn draw_segment(&mut self, segment: &TwoPointItem) {
        self.draw_simple_segment(segment.points, segment.style);
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
        self.content += &format!(
            r#"
                <circle cx="{}" cy="{}" r="{}" stroke="black" stroke-width="{}" stroke-dasharray="{}" fill="transparent"/>
            "#,
            circle.center.x,
            circle.center.y,
            circle.radius,
            Self::get_style_width(circle.style),
            Self::get_style_dashing(circle.style),
        );
    }

    fn end(&mut self) -> &str {
        self.content += "</g> </g> </svg>";
        &self.content
    }
}