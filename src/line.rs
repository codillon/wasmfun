use crate::utils::*;

#[derive(Debug)]
pub struct EditLine {
    id: usize,
    text: String,
    //div_ref: DivRef,
    info: InstrInfo,
    activated: bool,
    op: Option<usize>,
}

impl EditLine {
    pub fn new(id: usize, start_text: String) -> Self {
        let info = parse_instr(&start_text);
        Self {
            id,
            text: start_text,
            //div_ref: DivRef::new(),
            info,
            activated: true,
            op: None,
        }
    }

    pub fn id(&self) -> &usize {
        &self.id
    }

    pub fn set_text(&mut self, new_text: &str) {
        self.text = new_text.to_string();
        self.info = parse_instr(new_text);
    }

    pub fn logical_text(&self) -> &str {
        &self.text
    }

    pub fn display_text(&self) -> &str {
        const COSMETIC_SPACE: &str = "\u{FEFF}";

        if self.text.is_empty() {
            COSMETIC_SPACE
        } else {
            &self.text
        }
    }

    pub fn char_count(&self) -> usize {
        self.text.chars().count() // TODO: memoize
    }

    pub fn display_char_count(&self) -> usize {
        self.display_text().chars().count() // TODO: memoize
    }

    /*pub fn div_ref(&self) -> DivRef {
        self.div_ref
    }*/

    pub fn info(&self) -> &InstrInfo {
        &self.info
    }

    pub fn activated(&self) -> bool {
        self.activated
    }

    pub fn set_activated(&mut self, status: bool) {
        self.activated = status;
    }

    pub fn op(&self) -> &Option<usize> {
        &self.op
    }

    pub fn set_op(&mut self, idx: Option<usize>) {
        self.op = idx;
    }
}
