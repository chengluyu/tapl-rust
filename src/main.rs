#[macro_use]
extern crate combine;
use combine::{
    choice, many1,
    parser::char::{char, digit, letter, spaces, string},
    sep_by,
    stream::Stream,
    ParseError, Parser,
};
use rustyline::{error::ReadlineError, Editor};
use std::{
    collections::VecDeque,
    fmt::{Display, Formatter, Result as DisplayResult},
};

enum Binding {
    // NameBind,
    VarBind(Type),
}

struct Context(pub VecDeque<(String, Binding)>);

impl Context {
    fn new() -> Context {
        Context(VecDeque::new())
    }

    fn get_type(&self, name: String) -> Result<Type, String> {
        match self.0.iter().find(|(t, _)| t.eq(&name)) {
            Some((_, Binding::VarBind(ty))) => Ok(ty.clone()),
            _ => Err(format!("cannot find binding {:?}", name)),
        }
    }

    fn add_binding(&mut self, name: String, typ: Type) {
        self.0.push_front((name, Binding::VarBind(typ)));
    }

    fn remove_binding(&mut self, name: &String) {
        let mut at = None;
        for (index, (item, _)) in self.0.iter().enumerate() {
            if item == name {
                at = Some(index)
            }
        }
        if let Some(at) = at {
            self.0.remove(at);
        }
    }
}

#[derive(Clone, Debug)]
enum Type {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> DisplayResult {
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

#[derive(Debug, PartialEq)]
enum Term {
    True,
    False,
    Int(i32),
    If(Box<Term>, Box<Term>, Box<Term>),
    Var(String),
    Abs(String, Box<Type>, Box<Term>),
    App(Box<Term>, Box<Term>),
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> DisplayResult {
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
    fn type_parser[Input]()(Input) -> Type
    where [Input: Stream<Token = char>]
    {
        type_parser_()
    }
}

fn term_<Input>() -> impl Parser<Input, Output = Term>
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
        term(),
        skip_spaces(),
        keyword_then,
        term(),
        skip_spaces(),
        keyword_else,
        term(),
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
        term(),
    )
        .map(|t| Term::Abs(t.1, Box::new(t.5), Box::new(t.10)));

    let app = (char('['), term(), spaces(), term(), char(']'))
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
    fn term[Input]()(Input) -> Term
    where [Input: Stream<Token = char>]
    {
        term_()
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

    #[test]
    fn parse() {
        assert_eq!(term().parse("true"), Ok((Term::True, "")));
        assert_eq!(term().parse("false"), Ok((Term::False, "")));
        assert_eq!(term().parse("x"), Ok((Term::Var("x".into()), "")));
        assert_eq!(term().parse("cool"), Ok((Term::Var("cool".into()), "")));
        assert_eq!(
            term().parse("[x y]"),
            Ok((
                Term::App(
                    Box::new(Term::Var("x".into())),
                    Box::new(Term::Var("y".into()))
                ),
                ""
            ))
        );
        assert_eq!(
            term().parse("(x: Bool) => y"),
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
            term().parse("if x then y else z"),
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
            term().parse("if x then (x: Bool) => true else (x: Bool) => false"),
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

    #[test]
    fn check() {
        let mut context = Context::new();
        let identity = Term::Abs(
            "x".into(),
            Box::new(Type::Bool),
            Box::new(Term::Var("x".into())),
        );
        let t = Term::App(Box::new(identity), Box::new(Term::True));
        assert_eq!(t.get_type(&mut context), Ok(Type::Bool));
    }
}

fn main() {
    println!("Welcome to NotSimplyTyped.");
    let mut rl = Editor::<()>::new();
    let mut context = Context::new();
    let mut last_type: Option<Type> = None;
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                if line.starts_with(".") {
                    if line == ".q" || line == ".quit" || line == ".exit" {
                        break;
                    } else if line.starts_with(".save ") {
                        let name = &line[6..];
                        match &last_type {
                            Some(ty) => context.add_binding(name.into(), ty.clone()),
                            None => println!("No last type"),
                        }
                    } else {
                        println!("Unknown command, ignore.")
                    }
                } else {
                    match term().parse(line.as_str()) {
                        Ok((term, _)) => match term.get_type(&mut context) {
                            Ok(ty) => {
                                println!("Type: {}", ty);
                                last_type = Some(ty);
                            }
                            Err(e) => println!("Type error: {}", e),
                        },
                        Err(error) => {
                            println!("Parsing error: {:?}", error);
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C Bye.");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("^D Bye.");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
