use crate::{context::Context, r#type::{Type, type_parser}};
use combine::{
    choice, many1,
    parser::char::{char, digit, letter, spaces, string},
    stream::Stream,
    ParseError, Parser,
};
use std::fmt::{Display, Formatter, Result as FormatterResult};

#[derive(Debug, PartialEq)]
pub enum Term {
    True,
    False,
    Int(i32),
    If(Box<Term>, Box<Term>, Box<Term>),
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
            Term::If(test, consequent, alternate) => {
                write!(f, "if {} then {} else {}", test, consequent, alternate)
            }
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
                        if argument_type == *param_type {
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

fn term_parser_<Input>() -> impl Parser<Input, Output = Term>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let word = || many1(letter());
    let integer = many1(digit()).map(|s: String| Term::Int(s.parse::<i32>().unwrap()));
    let skip_spaces = || spaces().silent();
    // let lex_char = |c| char(c).skip(skip_spaces());

    let keyword_if = string("if").skip(skip_spaces());
    let keyword_then = string("then").skip(skip_spaces());
    let keyword_else = string("else").skip(skip_spaces());

    let eif = (
        keyword_if,
        term_parser(),
        skip_spaces(),
        keyword_then,
        term_parser(),
        skip_spaces(),
        keyword_else,
        term_parser(),
    )
        .map(|t| Term::If(Box::new(t.1), Box::new(t.4), Box::new(t.7)));

    let abs = (
        char('('),
        word().map(String::from),
        skip_spaces(),
        char(':'),
        skip_spaces(),
        type_parser(),
        char(')'),
        skip_spaces(),
        string("=>"),
        skip_spaces(),
        term_parser(),
    )
        .map(|t| Term::Abs(t.1, Box::new(t.5), Box::new(t.10)));

    let app = (char('['), term_parser(), spaces(), term_parser(), char(']'))
        .map(|t| Term::App(Box::new(t.1), Box::new(t.3)));

    choice((
        string("true").map(|_| Term::True),
        string("false").map(|_| Term::False),
        integer,
        eif,
        word().map(|name| Term::Var(name)),
        abs,
        app,
    ))
}

parser! {
    pub fn term_parser[Input]()(Input) -> Term
    where [Input: Stream<Token = char>]
    {
        term_parser_()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse() {
        assert_eq!(term_parser().parse("true"), Ok((Term::True, "")));
        assert_eq!(term_parser().parse("false"), Ok((Term::False, "")));
        assert_eq!(term_parser().parse("x"), Ok((Term::Var("x".into()), "")));
        assert_eq!(term_parser().parse("cool"), Ok((Term::Var("cool".into()), "")));
        assert_eq!(
            term_parser().parse("[x y]"),
            Ok((
                Term::App(
                    Box::new(Term::Var("x".into())),
                    Box::new(Term::Var("y".into()))
                ),
                ""
            ))
        );
        assert_eq!(
            term_parser().parse("(x: Bool) => y"),
            Ok((
                Term::Abs(
                    "x".into(),
                    Box::new(Type::Bool),
                    Box::new(Term::Var("y".into()))
                ),
                ""
            ))
        );
        assert_eq!(
            term_parser().parse("if x then y else z"),
            Ok((
                Term::If(
                    Box::new(Term::Var("x".into())),
                    Box::new(Term::Var("y".into())),
                    Box::new(Term::Var("z".into()))
                ),
                ""
            ))
        );
        assert_eq!(
            term_parser().parse("if x then (x: Bool) => true else (x: Bool) => false"),
            Ok((
                Term::If(
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
                ),
                ""
            ))
        );
    }
}