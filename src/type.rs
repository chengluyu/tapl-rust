use combine::{
    choice,
    parser::char::{char, spaces, string},
    sep_by,
    stream::Stream,
    ParseError, Parser,
};
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

fn type_parser_<Input>() -> impl Parser<Input, Output = Type>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let keyword_bool = string("Bool").map(|_| Type::Bool);
    let keyword_int = string("Int").map(|_| Type::Int);
    let parenthesized_type = (char('('), type_parser(), char(')')).map(|t| t.1);
    let atom = choice((keyword_bool, keyword_int, parenthesized_type));
    sep_by(atom, string("->").silent()).map(|ts: Vec<Type>| {
        *ts.iter()
            .map(|t| Box::new(t.clone()))
            .reduce(|left, right| Box::new(Type::Arrow(left, right)))
            .unwrap()
    })
}

parser! {
    pub fn type_parser[Input]()(Input) -> Type
    where [Input: Stream<Token = char>]
    {
        type_parser_()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_parser() {
        assert_eq!(type_parser().parse("Bool"), Ok((Type::Bool, "")));
        assert_eq!(
            type_parser().parse("Bool->Bool"),
            Ok((Type::Arrow(Box::new(Type::Bool), Box::new(Type::Bool)), ""))
        );
        assert_eq!(
            type_parser().parse("(Bool->Bool)->Bool"),
            Ok((
                Type::Arrow(
                    Box::new(Type::Arrow(Box::new(Type::Bool), Box::new(Type::Bool))),
                    Box::new(Type::Bool)
                ),
                ""
            ))
        );
        assert_eq!(
            type_parser().parse("Bool->(Bool->Bool)"),
            Ok((
                Type::Arrow(
                    Box::new(Type::Bool),
                    Box::new(Type::Arrow(Box::new(Type::Bool), Box::new(Type::Bool)))
                ),
                ""
            ))
        );
    }
}