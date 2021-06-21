use crate::{context::Context, termtype::Type};
use std::{borrow::Borrow, collections::HashMap, fmt::{Display, Formatter, Result as FormatterResult}};

#[derive(Debug, PartialEq)]
pub enum Term {
    True,
    False,
    Int(i32),
    Record(HashMap<String, Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
    Proj(Box<Term>, String),
    Var(String),
    Abs(String, Box<Type>, Box<Term>),
    App(Box<Term>, Box<Term>),
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatterResult {
        match self {
            Term::True => write!(f, "true"),
            Term::False => write!(f, "false"),
            Term::Int(value) => write!(f, "{}", value),
            Term::Record(fields) => {
                write!(f, "{} ", '{')?;
                for (index, (field, field_value)) in fields.iter().enumerate() {
                    write!(f, "{}: {}", field, field_value)?;
                    if index + 1 < fields.len() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, " {}", '}')
            }
            Term::If(test, consequent, alternate) => {
                write!(f, "if {} then {} else {}", test, consequent, alternate)
            }
            Term::Proj(target, field) => write!(f, "{}.{}", target, field),
            Term::Var(name) => write!(f, "{}", name),
            Term::Abs(param, param_type, body) => {
                write!(f, "({}: {:?}) => {}", param, param_type, body)
            }
            Term::App(callee, argument) => {
                write!(f, "({})({})", callee, argument)
            }
        }
    }
}

impl Term {
    pub fn get_type(&self, context: &mut Context) -> Result<Type, String> {
        let result = match self {
            Term::True => Ok(Type::Bool),
            Term::False => Ok(Type::Bool),
            Term::Int(_) => Ok(Type::Int),
            Term::Record(fields) => {
                let mut type_fields = HashMap::new();
                for (field, field_value) in fields {
                    let field_type = field_value.get_type(context)?;
                    type_fields.entry(field.clone()).or_insert(field_type);
                }
                Ok(Type::Record(type_fields))
            }
            Term::If(test, consequent, alternate) => match test.get_type(context)? {
                Type::Bool => {
                    let consequent_type = consequent.get_type(context)?;
                    let alternate_type = alternate.get_type(context)?;
                    if consequent_type == alternate_type {
                        Ok(alternate_type)
                    } else {
                        Err(format!(
                            "arms of conditional have different types: {} {}",
                            consequent_type, alternate_type
                        ))
                    }
                }
                ty => Err(format!("condition should be bool rather than {}", ty)),
            },
            Term::Proj(target, field) => {
                let target_type = target.get_type(context)?;
                if let Type::Record(entries) = &target_type {
                    if let Some(field_type) = entries.get(field) {
                        Ok(field_type.clone())
                    } else {
                        Err(format!("type {} has no field \"{}\"", target_type, field))
                    }
                } else {
                    Err(format!("expect a record type"))
                }
            }
            Term::Var(name) => context.get_type(name.clone()),
            Term::Abs(param, param_type, body_term) => {
                context.add_binding(param.clone(), *param_type.clone());
                let result = Ok(Type::Arrow(
                    param_type.clone(),
                    Box::new(body_term.get_type(context)?),
                ));
                context.remove_binding(param);
                result
            }
            Term::App(callee, argument) => {
                let argument_type = argument.get_type(context)?;
                match callee.get_type(context)? {
                    Type::Arrow(param_type, body_type) => {
                        if argument_type.is_subtype(param_type.borrow()) {
                            Ok(*body_type.clone())
                        } else {
                            Err(format!(
                                "expect an argument of type {} rather than {}",
                                argument_type, param_type
                            ))
                        }
                    }
                    _ => Err("callee is not callable".into()),
                }
            }
        };
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::TermParser;

    #[test]
    fn test_parse() {
        assert_eq!(TermParser::new().parse("true"), Ok(Term::True));
        assert_eq!(TermParser::new().parse("false"), Ok(Term::False));
        assert_eq!(TermParser::new().parse("x"), Ok(Term::Var("x".into())));
        assert_eq!(
            TermParser::new().parse("cool"),
            Ok(Term::Var("cool".into()))
        );
        assert_eq!(
            TermParser::new().parse("[x y]"),
            Ok(Term::App(
                Box::new(Term::Var("x".into())),
                Box::new(Term::Var("y".into()))
            ))
        );
        assert_eq!(
            TermParser::new().parse("(x: Bool) => y"),
            Ok(Term::Abs(
                "x".into(),
                Box::new(Type::Bool),
                Box::new(Term::Var("y".into()))
            ))
        );
        assert_eq!(
            TermParser::new().parse("if x then y else z"),
            Ok(Term::If(
                Box::new(Term::Var("x".into())),
                Box::new(Term::Var("y".into())),
                Box::new(Term::Var("z".into()))
            ))
        );
        assert_eq!(
            TermParser::new().parse("if x then (x: Bool) => true else (x: Bool) => false"),
            Ok(Term::If(
                Box::new(Term::Var("x".into())),
                Box::new(Term::Abs(
                    "x".into(),
                    Box::new(Type::Bool),
                    Box::new(Term::True)
                )),
                Box::new(Term::Abs(
                    "x".into(),
                    Box::new(Type::Bool),
                    Box::new(Term::False)
                ))
            ))
        );
    }
}
