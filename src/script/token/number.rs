#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
/// Unsigned integer for parsing
pub struct ParsedUint(u64);

/// Signed integer for parsing.
pub struct ParsedInt(i64);

/// Integer for computing
pub type CompUint = i64;

/// Integer for computing
pub type CompInteger = i64;

/// Number for computing
pub type CompNumber = f64;