use std::fmt::{Display, Formatter, Result};

#[derive(Clone, Debug)]
pub enum Type {
    Bool,
    Int,
    Arrow(Box<Type>, Box<Type>),
}

impl Type {
    fn is_arrow(&self) -> bool {
        match self {
            Type::Arrow(..) => true,
            _ => false,
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Type::Bool => matches!(other, Type::Bool),
            Type::Int => matches!(other, Type::Int),
            Type::Arrow(t1, t2) => match other {
                Type::Arrow(s1, s2) => t1.eq(s1) && t2.eq(s2),
                _ => false,
            },
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Type::Bool => write!(f, "Bool"),
            Type::Int => write!(f, "Int"),
            Type::Arrow(left, right) => {
                if right.is_arrow() {
                    write!(f, "{} -> ({})", left, right)
                } else {
                    write!(f, "{} -> {}", left, right)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::TermTypeParser;

    #[test]
    fn test_parse() {
        assert_eq!(TermTypeParser::new().parse("Bool"), Ok(Type::Bool));
        assert_eq!(
            TermTypeParser::new().parse("Bool -> Bool"),
            Ok(Type::Arrow(Box::new(Type::Bool), Box::new(Type::Bool)))
        );
        assert_eq!(
            TermTypeParser::new().parse("(Int -> Bool) -> Bool"),
            Ok(Type::Arrow(
                Box::new(Type::Arrow(Box::new(Type::Int), Box::new(Type::Bool))),
                Box::new(Type::Bool)
            ))
        );
        assert_eq!(
            TermTypeParser::new().parse("Int -> (Bool -> Bool)"),
            Ok(Type::Arrow(
                Box::new(Type::Int),
                Box::new(Type::Arrow(Box::new(Type::Bool), Box::new(Type::Bool)))
            ))
        );
    }
}
