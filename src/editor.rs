use std::collections::LinkedList;

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
            next_id: 10,
            lines: vec![
                EditLine::new(
                    1,
                    String::from("(func $add (param $hello i32) (param $hello i32)"),
                    InstrKind::Activated,
                ),
                EditLine::new(2, String::from("i32.const 0"), InstrKind::Activated),
                EditLine::new(3, String::from("drop"), InstrKind::Activated),
                EditLine::new(4, String::from(")"), InstrKind::Activated),
                EditLine::new(5, String::from("(func"), InstrKind::Activated),
                EditLine::new(6, String::from("i32.const 1"), InstrKind::Activated),
                EditLine::new(7, String::from("if (result i32 i32)"), InstrKind::Activated),
                EditLine::new(8, String::from("i32.const 1"), InstrKind::Activated),
                EditLine::new(9, String::from("i32.const 1"), InstrKind::Activated),
                EditLine::new(10, String::from("else"), InstrKind::Activated),
                EditLine::new(11, String::from("i32.const 2"), InstrKind::Activated),
                EditLine::new(12, String::from("i32.const 2"), InstrKind::Activated),
                EditLine::new(13, String::from("end"), InstrKind::Activated),
                EditLine::new(14, String::from("drop"), InstrKind::Activated),
                EditLine::new(15, String::from("drop"), InstrKind::Activated),
                EditLine::new(16, String::from(")"), InstrKind::Activated),
            ],
            synthetic_ends: 0,
            module: OkModule::build(text_to_binary("").expect("wasm_bin"), &Vec::new())
                .expect("OkModule"),
        };
        editor.fix_frames();
        editor.update_module();
        editor.update_operators();
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
            .filter(|line| {
                *(line.info()) != InstrInfo::EmptyorMalformed
                    && line.kind() != &InstrKind::Deactivated
            })
            .map(|line| line.logical_text())
            .collect::<Vec<_>>()
            .join("\n");
        for _ in 0..self.synthetic_ends {
            joined.push_str("\nend");
        }
        joined
    }

    pub fn delete_synthetic(&mut self) {
        self.lines
            .retain(|line| !matches!(line.kind(), InstrKind::Synthetic(_)));
    }

    pub fn fix_frames(&mut self) {
        let instrs: Vec<InstrInfo> = self.lines.iter().map(|line| line.info()).copied().collect();
        let (deactivate_indices, num_synthetic_end) = fix_frames(&instrs);
        for (i, line) in self.lines.iter_mut().enumerate() {
            if deactivate_indices.contains(&i) {
                line.set_kind(InstrKind::Deactivated);
            } else {
                line.set_kind(InstrKind::Activated);
            }
        }
        self.synthetic_ends = num_synthetic_end;
    }

    pub fn update_module(&mut self) {
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
            if line.can_have_op() {
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
        self.update_module();
        self.update_operators();
    }

    pub fn insert_line(&mut self, idx: usize, kind: InstrKind) {
        let editline = EditLine::new(self.next_id, String::from(""), kind);
        self.lines.insert(idx, editline);
        self.next_id += 1;
    }

    pub fn insert_line_with_text(&mut self, idx: usize, text: String, kind: InstrKind) {
        let editline = EditLine::new(self.next_id, text, kind);
        self.lines.insert(idx, editline);
        self.next_id += 1;
    }

    /*pub fn fix_validation(&mut self) {
        while let Some(((idx, instr), parent_idx)) = test(self.module.binary(), &self.module.ops().as_ref().unwrap()) {
            self.insert_line_with_text(idx, instr, InstrKind::Synthetic(parent_idx));
            self.update_module();
        }
    }*/

    pub fn add_ui_elements(&mut self) {
        println!(
            "Printing Operands: {:?}",
            print_operands(self.module.binary(), &self.module.ops().as_ref().unwrap())
        );
        let operands = print_operands(self.module.binary(), &self.module.ops().as_ref().unwrap())
            .expect("operands");
        let mut operands_iter = operands.iter();
        for line in self.lines.iter_mut() {
            if line.can_have_op() {
                let (pops, pushes) = operands_iter.next().expect("ops");
                let mut params = Vec::new();
                let mut results = Vec::new();
                for (valtype, idx) in pops {
                    params.push(Param {
                        valtype: valtype.clone(),
                        idx: Some(*idx),
                    })
                }
                for push in pushes {
                    results.push(Result {
                        valtype: push.clone(),
                    });
                }
                line.set_params(params);
                line.set_results(results);
            }
        }
    }

    /*pub fn run_test(&mut self) {
        let result = test(self.module.binary(), &self.module.ops().as_ref().unwrap());
        println!("{result:?}");
    }*/
}

#[derive(Debug, Clone, PartialEq)]
pub enum InstrKind {
    Synthetic(usize),
    Activated,
    Deactivated,
}

#[derive(Debug, Clone)]
pub struct Param {
    valtype: wasmparser::ValType,
    idx: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct Result {
    valtype: wasmparser::ValType,
}

#[derive(Debug, Clone)]
pub struct EditLine {
    id: usize,
    text: String,
    //div_ref: DivRef,
    info: InstrInfo,
    kind: InstrKind,
    params: Vec<Param>,
    results: Vec<Result>,
    op: Option<usize>,
}

impl EditLine {
    pub fn new(id: usize, start_text: String, kind: InstrKind) -> Self {
        let info = parse_instr(&start_text);
        Self {
            id,
            text: start_text,
            //div_ref: DivRef::new(),
            info,
            kind,
            params: Vec::new(),
            results: Vec::new(),
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

    pub fn kind(&self) -> &InstrKind {
        &self.kind
    }

    pub fn set_kind(&mut self, kind: InstrKind) {
        self.kind = kind;
    }

    pub fn params(&self) -> &Vec<Param> {
        &self.params
    }

    pub fn set_params(&mut self, params: Vec<Param>) {
        self.params = params;
    }

    pub fn results(&self) -> &Vec<Result> {
        &self.results
    }

    pub fn set_results(&mut self, results: Vec<Result>) {
        self.results = results;
    }

    pub fn op(&self) -> &Option<usize> {
        &self.op
    }

    pub fn set_op(&mut self, idx: Option<usize>) {
        self.op = idx;
    }

    pub fn can_have_op(&self) -> bool {
        self.info != InstrInfo::EmptyorMalformed
            && self.info != InstrInfo::FuncHeader
            && self.kind != InstrKind::Deactivated
    }
}
