#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub parser);

mod context;
mod term;
mod termtype;

use context::Context;
use parser::TermParser;
use rustyline::{error::ReadlineError, Editor};
use termtype::TermType;

fn main() {
    println!("Welcome to NotSimplyTyped.");
    let mut rl = Editor::<()>::new();
    let mut context = Context::new();
    let mut last_type: Option<TermType> = None;
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
                    match TermParser::new().parse(line.as_str()) {
                        Ok(term) => match term.get_type(&mut context) {
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
