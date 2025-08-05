use crate::utils::*;

#[derive(Debug)]
pub struct Editor {
    next_id: usize,
    lines: Vec<EditLine>,
    synthetic_ends: usize,
    module: OkModule,
}

impl Editor {
    pub fn new() -> Self {
        let mut editor = Self {
            next_id: 9,
            lines: vec![
                EditLine::new(1, String::from("(func $add (param i32)")),
                EditLine::new(2, String::from("i32.const 0")),
                EditLine::new(3, String::from("drop")),
                EditLine::new(4, String::from(")")),
                EditLine::new(5, String::from("(func")),
                EditLine::new(6, String::from("i32.const 1")),
                EditLine::new(7, String::from("call $add")),
                EditLine::new(8, String::from(")")),
            ],
            synthetic_ends: 0,
            module: OkModule::build(text_to_binary("").expect("wasm_bin"), &Vec::new())
                .expect("OkModule"),
        };
        editor.fix_frames();
        editor
    }

    pub fn lines(&self) -> &Vec<EditLine> {
        &self.lines
    }

    pub fn synthetic_ends(&self) -> usize {
        self.synthetic_ends
    }

    pub fn module(&self) -> &OkModule {
        &self.module
    }

    pub fn text(&self) -> String {
        let mut joined = self
            .lines
            .iter()
            .filter(|line| *(line.info()) != InstrInfo::EmptyorMalformed && line.activated())
            .map(|line| line.logical_text())
            .collect::<Vec<_>>()
            .join("\n");
        for _ in 0..self.synthetic_ends {
            joined.push_str("\nend");
        }
        joined
    }

    pub fn fix_frames(&mut self) {
        let instrs: Vec<InstrInfo> = self.lines.iter().map(|line| line.info()).copied().collect();
        let (deactivate_indices, num_synthetic_end) = fix_frames(&instrs);
        for (i, line) in self.lines.iter_mut().enumerate() {
            if deactivate_indices.contains(&i) {
                line.set_activated(false);
            } else {
                line.set_activated(true);
            }
        }
        self.synthetic_ends = num_synthetic_end;
        self.module = OkModule::build(
            text_to_binary(&self.text()).expect("wasm binary"),
            &self.lines,
        )
        .expect("OkModule");
    }

    pub fn update_operators(&mut self) {
        let ops_len = self.module.ops_len(0).expect("length of operators");
        let mut op_index = 0;

        for line in self.lines.iter_mut() {
            if line.activated() && line.can_have_op() {
                if op_index < ops_len {
                    line.set_op(Some(op_index));
                    op_index += 1;
                } else {
                    line.set_op(None);
                }
            } else {
                line.set_op(None);
            }
        }
    }

    pub fn input(&mut self, idx: usize, new_text: &str) {
        self.lines[idx].set_text(new_text);
        self.fix_frames();
        self.update_operators();
    }

    pub fn insert_line(&mut self, idx: usize) {
        let editline = EditLine::new(self.next_id, String::from(""));
        self.lines.insert(idx, editline);
        self.next_id += 1;
    }

    pub fn run_test(&self) {
        test(&self.text(), &self.module.ops().as_ref().unwrap());
    }
}

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

    pub fn set_info(&mut self, info: InstrInfo) {
        self.info = info
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

    pub fn can_have_op(&self) -> bool {
        self.info != InstrInfo::EmptyorMalformed && self.info != InstrInfo::FuncHeader
    }
}
