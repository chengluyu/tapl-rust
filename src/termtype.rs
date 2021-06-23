use std::{
    collections::HashMap,
    fmt::{Display, Formatter, Result},
};

#[derive(Clone, Debug)]
pub enum TermType {
    Bool,
    Int,
    Arrow(Box<TermType>, Box<TermType>),
    Record(HashMap<String, TermType>),
    Top,
}

impl TermType {
    fn is_arrow(&self) -> bool {
        match self {
            TermType::Arrow(..) => true,
            _ => false,
        }
    }

    pub fn is_subtype(&self, other: &TermType) -> bool {
        self == other
            || match (self, other) {
                (_, TermType::Top) => true,
                (TermType::Record(these_fields), TermType::Record(those_fields)) => {
                    those_fields.iter().all(|(that_field, that_field_type)| {
                        if let Some(this_field_type) = these_fields.get(that_field) {
                            this_field_type.is_subtype(that_field_type)
                        } else {
                            false
                        }
                    })
                }
                (TermType::Arrow(self_left, self_right), TermType::Arrow(other_left, other_right)) => {
                    self_left.is_subtype(other_left) && self_right.is_subtype(other_right)
                }
                _ => false,
            }
    }
}

impl PartialEq for TermType {
    fn eq(&self, other: &Self) -> bool {
        match self {
            TermType::Bool => matches!(other, TermType::Bool),
            TermType::Int => matches!(other, TermType::Int),
            TermType::Arrow(t1, t2) => match other {
                TermType::Arrow(s1, s2) => t1.eq(s1) && t2.eq(s2),
                _ => false,
            },
            TermType::Record(this_fields) => {
                if let TermType::Record(that_fields) = other {
                    this_fields.len() == that_fields.len()
                        && this_fields.iter().all(|(field, this_field_type)| {
                            if let Some(that_field_type) = that_fields.get(field) {
                                this_field_type == that_field_type
                            } else {
                                false
                            }
                        })
                } else {
                    false
                }
            }
            TermType::Top => matches!(other, TermType::Top),
        }
    }
}

impl Display for TermType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            TermType::Bool => write!(f, "Bool"),
            TermType::Int => write!(f, "Int"),
            TermType::Arrow(left, right) => {
                if right.is_arrow() {
                    write!(f, "{} -> ({})", left, right)
                } else {
                    write!(f, "{} -> {}", left, right)
                }
            }
            TermType::Record(fields) => {
                write!(f, "{} ", '{')?;
                for (index, (field, field_type)) in fields.iter().enumerate() {
                    write!(f, "{}: {}", field, field_type)?;
                    if index + 1 < fields.len() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, " {}", '}')
            }
            TermType::Top => write!(f, "⊤"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::TermTypeParser;

    #[test]
    fn test_subtype_top() {
        assert!(TermType::Bool.is_subtype(&TermType::Top));
        assert!(TermType::Int.is_subtype(&TermType::Top));
        assert!(TermType::Arrow(Box::new(TermType::Int), Box::new(TermType::Int)).is_subtype(&TermType::Top));
        let mut entries = HashMap::new();
        entries.entry("x".into()).or_insert(TermType::Int);
        entries.entry("y".into()).or_insert(TermType::Int);
        assert!(TermType::Record(entries).is_subtype(&TermType::Top));
    }

    #[test]
    fn test_subtype_record() {
        // { x: Int, y: Int } <: { x: Int, y: Int, z: Int }
        let mut entries = HashMap::new();
        entries.entry("x".into()).or_insert(TermType::Int);
        entries.entry("y".into()).or_insert(TermType::Int);
        let xiyi = TermType::Record(entries);
        let mut entries = HashMap::new();
        entries.entry("x".into()).or_insert(TermType::Int);
        entries.entry("y".into()).or_insert(TermType::Int);
        entries.entry("z".into()).or_insert(TermType::Int);
        let xiyizi = TermType::Record(entries);
        assert!(xiyizi.is_subtype(&xiyi));
        assert!(!xiyi.is_subtype(&xiyizi));
        // { x: Int, y: Int } <: {}
        // { x: Int, y: Int, z: Int } <: {}
        let entries = HashMap::new();
        let empty = TermType::Record(entries);
        assert!(xiyi.is_subtype(&empty));
        assert!(xiyizi.is_subtype(&empty));
        // { x: Int, y: Int } <: { x: Top, y: Top }
        let mut entries = HashMap::new();
        entries.entry("x".into()).or_insert(TermType::Top);
        entries.entry("y".into()).or_insert(TermType::Top);
        let xtyt = TermType::Record(entries);
        assert!(xiyi.is_subtype(&xtyt));
        // { x: Int, y: Int } does nothing with { x: Top, y: Top, z: Top }
        let mut entries = HashMap::new();
        entries.entry("x".into()).or_insert(TermType::Top);
        entries.entry("y".into()).or_insert(TermType::Top);
        entries.entry("z".into()).or_insert(TermType::Top);
        let xtytzt = TermType::Record(entries);
        assert!(!xiyi.is_subtype(&xtytzt));
    }

    #[test]
    fn test_parse() {
        assert_eq!(TermTypeParser::new().parse("Bool"), Ok(TermType::Bool));
        assert_eq!(
            TermTypeParser::new().parse("Bool -> Bool"),
            Ok(TermType::Arrow(Box::new(TermType::Bool), Box::new(TermType::Bool)))
        );
        assert_eq!(
            TermTypeParser::new().parse("(Int -> Bool) -> Bool"),
            Ok(TermType::Arrow(
                Box::new(TermType::Arrow(Box::new(TermType::Int), Box::new(TermType::Bool))),
                Box::new(TermType::Bool)
            ))
        );
        assert_eq!(
            TermTypeParser::new().parse("Int -> (Bool -> Bool)"),
            Ok(TermType::Arrow(
                Box::new(TermType::Int),
                Box::new(TermType::Arrow(Box::new(TermType::Bool), Box::new(TermType::Bool)))
            ))
        );
    }
}
