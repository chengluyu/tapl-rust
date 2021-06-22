use std::collections::VecDeque;

use crate::termtype::TermType;

pub enum Binding {
    // NameBind,
    VarBind(TermType),
}

pub struct Context(pub VecDeque<(String, Binding)>);

impl Context {
    pub fn new() -> Context {
        Context(VecDeque::new())
    }

    pub fn get_type(&self, name: String) -> Result<TermType, String> {
        match self.0.iter().find(|(t, _)| t.eq(&name)) {
            Some((_, Binding::VarBind(ty))) => Ok(ty.clone()),
            _ => Err(format!("cannot find binding {:?}", name)),
        }
    }

    pub fn add_binding(&mut self, name: String, typ: TermType) {
        self.0.push_front((name, Binding::VarBind(typ)));
    }

    pub fn remove_binding(&mut self, name: &String) {
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
