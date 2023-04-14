#![warn(clippy::pedantic)]

pub mod cli;
pub mod drawer;
pub mod generator;
pub mod projector;
pub mod script;

#[derive(Debug)]
pub enum Error {
    TypeMismatch(String, String),
}
