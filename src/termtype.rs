use std::{
    collections::HashMap,
    fmt::{Display, Formatter, Result},
};

#[derive(Clone, Debug)]
pub enum Type {
    Bool,
    Int,
    Arrow(Box<Type>, Box<Type>),
    Record(HashMap<String, Type>),
    Top,
}

impl Type {
    fn is_arrow(&self) -> bool {
        match self {
            Type::Arrow(..) => true,
            _ => false,
        }
    }

    pub fn is_subtype(&self, other: &Type) -> bool {
        self == other
            || match (self, other) {
                (_, Type::Top) => true,
                (Type::Record(these_fields), Type::Record(those_fields)) => {
                    those_fields.iter().all(|(that_field, that_field_type)| {
                        if let Some(this_field_type) = these_fields.get(that_field) {
                            this_field_type.is_subtype(that_field_type)
                        } else {
                            false
                        }
                    })
                }
                (Type::Arrow(self_left, self_right), Type::Arrow(other_left, other_right)) => {
                    self_left.is_subtype(other_left) && self_right.is_subtype(other_right)
                }
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
            Type::Record(this_fields) => {
                if let Type::Record(that_fields) = other {
                    this_fields.len() == that_fields.len()
                        && this_fields.iter().all(|(field, this_field_type)| {
                            that_fields.get(field).map_or_else(
                                || false,
                                |that_field_type| this_field_type == that_field_type,
                            )
                        })
                } else {
                    false
                }
            }
            Type::Top => matches!(other, Type::Top),
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
            Type::Record(fields) => {
                write!(f, "{} ", '{')?;
                for (index, (field, field_type)) in fields.iter().enumerate() {
                    write!(f, "{}: {}", field, field_type)?;
                    if index + 1 < fields.len() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, " {}", '}')
            }
            Type::Top => write!(f, "⊤"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::TermTypeParser;

    #[test]
    fn test_subtype_top() {
        assert!(Type::Bool.is_subtype(&Type::Top));
        assert!(Type::Int.is_subtype(&Type::Top));
        assert!(Type::Arrow(Box::new(Type::Int), Box::new(Type::Int)).is_subtype(&Type::Top));
        let mut entries = HashMap::new();
        entries.entry("x".into()).or_insert(Type::Int);
        entries.entry("y".into()).or_insert(Type::Int);
        assert!(Type::Record(entries).is_subtype(&Type::Top));
    }

    #[test]
    fn test_subtype_record() {
        // { x: Int, y: Int } <: { x: Int, y: Int, z: Int }
        let mut entries = HashMap::new();
        entries.entry("x".into()).or_insert(Type::Int);
        entries.entry("y".into()).or_insert(Type::Int);
        let xiyi = Type::Record(entries);
        let mut entries = HashMap::new();
        entries.entry("x".into()).or_insert(Type::Int);
        entries.entry("y".into()).or_insert(Type::Int);
        entries.entry("z".into()).or_insert(Type::Int);
        let xiyizi = Type::Record(entries);
        assert!(xiyizi.is_subtype(&xiyi));
        assert!(!xiyi.is_subtype(&xiyizi));
        // { x: Int, y: Int } <: {}
        // { x: Int, y: Int, z: Int } <: {}
        let entries = HashMap::new();
        let empty = Type::Record(entries);
        assert!(xiyi.is_subtype(&empty));
        assert!(xiyizi.is_subtype(&empty));
        // { x: Int, y: Int } <: { x: Top, y: Top }
        let mut entries = HashMap::new();
        entries.entry("x".into()).or_insert(Type::Top);
        entries.entry("y".into()).or_insert(Type::Top);
        let xtyt = Type::Record(entries);
        assert!(xiyi.is_subtype(&xtyt));
        // { x: Int, y: Int } does nothing with { x: Top, y: Top, z: Top }
        let mut entries = HashMap::new();
        entries.entry("x".into()).or_insert(Type::Top);
        entries.entry("y".into()).or_insert(Type::Top);
        entries.entry("z".into()).or_insert(Type::Top);
        let xtytzt = Type::Record(entries);
        assert!(!xiyi.is_subtype(&xtytzt));
    }

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
