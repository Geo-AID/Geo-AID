pub mod script;
pub mod generator;
pub mod projector;
pub mod drawer;

#[derive(Debug)]
pub enum Error {
    TypeMismatch(String, String)
}