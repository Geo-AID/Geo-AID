//! Flag-related definitions and functionality.

use std::collections::HashMap;

use crate::{
    parser::{FlagStatement, Parse},
    token::{NumberLit, Span},
    Error,
};

use super::{context::CompileContext, most_similar};

/// A compiler flag.
#[derive(Debug)]
pub struct Flag {
    /// The flag's name
    pub name: &'static str,
    /// The flag kind.
    pub kind: FlagKind,
    /// The card type.
    pub ty: FlagType,
}

impl Flag {
    #[must_use]
    pub fn as_set(&self) -> Option<&FlagSet> {
        match &self.kind {
            FlagKind::Setting(_) => None,
            FlagKind::Set(set) => Some(set),
        }
    }

    #[must_use]
    pub fn as_bool(&self) -> Option<bool> {
        match &self.kind {
            FlagKind::Setting(setting) => setting.get_value().and_then(FlagValue::as_bool),
            FlagKind::Set(_) => None,
        }
    }

    #[must_use]
    pub fn as_ident(&self) -> Option<&String> {
        match &self.kind {
            FlagKind::Setting(setting) => setting.get_value().and_then(FlagValue::as_string),
            FlagKind::Set(_) => None,
        }
    }

    /// Get the span of where the flag is set. Only works with non-flagset flags.
    #[must_use]
    pub fn get_span(&self) -> Option<Span> {
        match &self.kind {
            FlagKind::Setting(setting) => setting.get_span(),
            FlagKind::Set(_) => None,
        }
    }
}

/// The type of this flag
#[derive(Debug)]
pub enum FlagType {
    /// A nested flag set.
    Set,
    /// True or false
    Boolean,
    String,
}

/// The kind of a flag.
#[derive(Debug)]
pub enum FlagKind {
    /// A specific setting for a value.
    Setting(FlagSetting),
    /// A nested flag set
    Set(FlagSet),
}

impl FlagKind {
    #[must_use]
    pub fn as_setting(&self) -> Option<&FlagSetting> {
        if let Self::Setting(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_set(&self) -> Option<&FlagSet> {
        if let Self::Set(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

/// A flag setting.
#[derive(Debug)]
pub enum FlagSetting {
    /// A default value that has not been overriden (even if with the same value)
    Default(FlagValue),
    /// An unset flag, for ones without default values.
    Unset,
    /// An explicitly set value for a flag. NOT A FLAG SET.
    Set(FlagValue, Span),
}

impl FlagSetting {
    /// Get the flag's value, if there's any.
    #[must_use]
    pub fn get_value(&self) -> Option<&FlagValue> {
        match self {
            FlagSetting::Default(v) | FlagSetting::Set(v, _) => Some(v),
            FlagSetting::Unset => None,
        }
    }

    /// Get the span where the flag is set, if it is.
    #[must_use]
    pub fn get_span(&self) -> Option<Span> {
        match self {
            FlagSetting::Default(_) | FlagSetting::Unset => None,
            FlagSetting::Set(_, sp) => Some(*sp),
        }
    }
}

/// A compiler flag value.
#[derive(Debug)]
pub enum FlagValue {
    String(String),
    Bool(bool),
}

impl FlagValue {
    #[must_use]
    pub fn as_bool(&self) -> Option<bool> {
        if let Self::Bool(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_string(&self) -> Option<&String> {
        if let Self::String(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

/// A set of flags, also referenced to as a flag group.
pub type FlagSet = HashMap<&'static str, Flag>;

/// A helper struct for constructing a flag set.
pub struct FlagSetConstructor {
    pub flags: Vec<(&'static str, Flag)>,
}

impl FlagSetConstructor {
    /// Create a new, empty flag set.
    #[must_use]
    pub fn new() -> Self {
        Self { flags: Vec::new() }
    }

    /// Add a string flag.
    #[must_use]
    pub fn add_ident(mut self, name: &'static str) -> Self {
        self.flags.push((
            name,
            Flag {
                name,
                kind: FlagKind::Setting(FlagSetting::Unset),
                ty: FlagType::String,
            },
        ));

        self
    }

    /// Add a string flag with a default
    #[must_use]
    pub fn add_ident_def(mut self, name: &'static str, default: impl ToString) -> Self {
        self.flags.push((
            name,
            Flag {
                name,
                kind: FlagKind::Setting(FlagSetting::Default(FlagValue::String(
                    default.to_string(),
                ))),
                ty: FlagType::String,
            },
        ));

        self
    }

    /// Add a boolean flag
    #[must_use]
    pub fn add_bool(mut self, name: &'static str) -> Self {
        self.flags.push((
            name,
            Flag {
                name,
                kind: FlagKind::Setting(FlagSetting::Unset),
                ty: FlagType::Boolean,
            },
        ));

        self
    }

    /// Add a boolean flag with a default
    #[must_use]
    pub fn add_bool_def(mut self, name: &'static str, default: bool) -> Self {
        self.flags.push((
            name,
            Flag {
                name,
                kind: FlagKind::Setting(FlagSetting::Default(FlagValue::Bool(default))),
                ty: FlagType::Boolean,
            },
        ));

        self
    }

    /// Add a nested flag set
    #[must_use]
    pub fn add_set(mut self, name: &'static str, set: FlagSetConstructor) -> Self {
        self.flags.push((
            name,
            Flag {
                name,
                kind: FlagKind::Set(set.finish()),
                ty: FlagType::Set,
            },
        ));

        self
    }

    /// Finish creating a flag set.
    #[must_use]
    pub fn finish(self) -> FlagSet {
        self.flags.into_iter().collect()
    }
}

impl Default for FlagSetConstructor {
    fn default() -> Self {
        Self::new()
    }
}

/// Set a flag's value.
pub fn set_flag(set: &mut FlagSet, flag: &FlagStatement, context: &CompileContext) {
    set_flag_recursive(set, flag, context, 0);
}

/// Set a flag's value.
fn set_flag_recursive(
    set: &mut FlagSet,
    flag: &FlagStatement,
    context: &CompileContext,
    depth: usize,
) {
    let Some(flag_ref) = set.get_mut(flag.name.name.get(depth).unwrap().ident.as_str()) else {
        let flag_name = flag.name.name.get(depth).unwrap().ident.clone();

        let suggested = most_similar(set.keys(), &flag_name).cloned();

        context.push_error(Error::FlagDoesNotExist {
            flag_name,
            flag_span: flag.name.get_span(),
            error_span: flag.get_span(),
            suggested,
        });

        // Pretend the flag doesn't exist.
        return;
    };

    if flag.name.name.len() - 1 == depth {
        match flag_ref.ty {
            FlagType::Set => match &flag.value {
                crate::parser::FlagValue::Set(set) => match &mut flag_ref.kind {
                    FlagKind::Setting(_) => unreachable!(),
                    FlagKind::Set(s) => {
                        for stat in &set.flags {
                            set_flag(s, stat, context);
                        }
                    }
                },
                crate::parser::FlagValue::Ident(_) | crate::parser::FlagValue::Number(_) => {
                    context.push_error(Error::FlagSetExpected {
                        error_span: flag.get_span(),
                    });
                }
            },
            FlagType::Boolean => {
                context.ok(set_flag_bool(flag_ref, flag));
            }
            FlagType::String => match &flag.value {
                crate::parser::FlagValue::Number(_) | crate::parser::FlagValue::Set(_) => {
                    context.push_error(Error::StringExpected {
                        error_span: flag.get_span(),
                    });
                }
                crate::parser::FlagValue::Ident(ident) => match &mut flag_ref.kind {
                    FlagKind::Setting(s) => match s {
                        FlagSetting::Default(_) | FlagSetting::Unset => {
                            *s = FlagSetting::Set(
                                FlagValue::String(ident.ident.clone()),
                                flag.get_span(),
                            );
                        }
                        FlagSetting::Set(_, sp) => {
                            context.push_error(Error::RedefinedFlag {
                                error_span: flag.get_span(),
                                first_defined: *sp,
                                flag_name: flag_ref.name,
                            });
                        }
                    },
                    FlagKind::Set(_) => unreachable!(),
                },
            },
        }
    } else if let FlagKind::Set(set) = &mut flag_ref.kind {
        set_flag_recursive(set, flag, context, depth + 1);
    } else {
        context.push_error(Error::FlagSetExpected {
            error_span: flag.get_span(),
        });
    }
}

/// Set a boolean flag.
fn set_flag_bool(flag: &mut Flag, stmt: &FlagStatement) -> Result<(), Error> {
    match &stmt.value {
        crate::parser::FlagValue::Set(_) => {
            return Err(Error::BooleanExpected {
                error_span: stmt.get_span(),
            })
        }
        crate::parser::FlagValue::Ident(ident) => match &mut flag.kind {
            FlagKind::Setting(s) => match s {
                FlagSetting::Default(_) | FlagSetting::Unset => {
                    *s = FlagSetting::Set(
                        FlagValue::Bool(match ident.ident.as_str() {
                            "enabled" | "on" | "true" => true,
                            "disabled" | "off" | "false" => false,
                            _ => {
                                return Err(Error::BooleanExpected {
                                    error_span: stmt.get_span(),
                                })
                            }
                        }),
                        stmt.get_span(),
                    );
                }
                FlagSetting::Set(_, sp) => {
                    return Err(Error::RedefinedFlag {
                        error_span: stmt.get_span(),
                        first_defined: *sp,
                        flag_name: flag.name,
                    })
                }
            },
            FlagKind::Set(_) => unreachable!(),
        },
        crate::parser::FlagValue::Number(num) => match &mut flag.kind {
            FlagKind::Setting(s) => match s {
                FlagSetting::Default(_) | FlagSetting::Unset => {
                    *s = FlagSetting::Set(
                        FlagValue::Bool(match num {
                            NumberLit::Integer(i) => match i.parsed.parse::<u8>() {
                                Ok(0) => false,
                                Ok(1) => true,
                                _ => {
                                    return Err(Error::BooleanExpected {
                                        error_span: stmt.get_span(),
                                    })
                                }
                            },
                            NumberLit::Float(_) => {
                                return Err(Error::BooleanExpected {
                                    error_span: stmt.get_span(),
                                })
                            }
                        }),
                        stmt.get_span(),
                    );
                }
                FlagSetting::Set(_, sp) => {
                    return Err(Error::RedefinedFlag {
                        error_span: stmt.get_span(),
                        first_defined: *sp,
                        flag_name: flag.name,
                    })
                }
            },
            FlagKind::Set(_) => unreachable!(),
        },
    }

    Ok(())
}
