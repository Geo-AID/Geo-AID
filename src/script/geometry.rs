use crate::generator::Complex;

#[derive(Debug)]
pub struct Point {
    pub position: Option<Complex>,
    pub name: String,
    pub index: usize
}