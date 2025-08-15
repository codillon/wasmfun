use crate::editor::*;
use anyhow::{Result, anyhow};
use self_cell::self_cell;
use std::ops::RangeInclusive;
use wasm_tools::parse_binary_wasm;
use wasmparser::{Parser, ValType};
use wast::core::{Instruction, Module};
use wast::parser::{self, ParseBuffer};

// (Operator, EditLine idx)
type Ops<'a> = Result<Vec<Vec<(wasmparser::Operator<'a>, usize)>>>;

self_cell!(
    pub struct OkModule {
        owner: Vec<u8>,
        #[covariant]
        dependent: Ops,
        // Reverse mapping maps op index (wihtin a function) to a global edit line index
    }

    impl {Debug}
);

impl OkModule {
    pub fn build(wasm_bin: Vec<u8>, lines: &Vec<EditLine>) -> Result<Self> {
        Ok(OkModule::new(wasm_bin, |wasm_bin| {
            let parser = wasmparser::Parser::new(0);
            let mut functions = Vec::new();
            let mut line_idx = 0;

            for payload in parser.parse_all(wasm_bin) {
                if let wasmparser::Payload::CodeSectionEntry(body) = payload? {
                    let mut func_ops = Vec::new();
                    for op in body.get_operators_reader()? {
                        if op.is_err() {
                            continue;
                        };
                        // Advance our line idx until we encounter a line that should have an op.
                        while !lines[line_idx].can_have_op() {
                            line_idx += 1;
                        }
                        debug_assert!(!(line_idx > lines.len()));

                        func_ops.push((op?, line_idx));
                        line_idx += 1;
                    }
                    functions.push(func_ops);
                }
            }
            Ok(functions)
        }))
    }

    pub fn binary(&self) -> &[u8] {
        self.borrow_owner()
    }

    pub fn ops(&self) -> &Ops {
        self.borrow_dependent()
    }

    pub fn ops_len(&self, func_idx: usize) -> Result<usize, &anyhow::Error> {
        self.with_dependent(|_binary, ops| ops.as_ref().map(|ops| ops[func_idx].len()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstrInfo {
    FuncHeader,
    FuncEnd,
    If,
    Else,
    End,
    OtherStructured, // block, loop, or try_table
    Other,           // any other instruction
    EmptyorMalformed,
}

impl From<Instruction<'_>> for InstrInfo {
    fn from(instr: Instruction<'_>) -> Self {
        match instr {
            Instruction::If(_) => InstrInfo::If,
            Instruction::Else(_) => InstrInfo::Else,
            Instruction::End(_) => InstrInfo::End,
            Instruction::Block(_) | Instruction::Loop(_) | Instruction::TryTable(_) => {
                InstrInfo::OtherStructured
            }
            Instruction::Try(_) | Instruction::Catch(_) | Instruction::CatchAll => {
                panic!("legacy-exceptions not supported");
            }
            _ => InstrInfo::Other,
        }
    }
}

/// Parse one code line as instruction
/// (only accepts plain instructions)
///
/// Uses wast ParseBuffer to convert string into buffer and wast parser to parse buffer as Instruction
///
/// # Parameters
/// s: A string slice representing a Wasm instruction
///
/// # Returns
/// Err: Malformed
/// Ok(None): empty
/// Ok(Some(InstrInfo)): instruction of given category
pub fn parse_instr(s: &str) -> InstrInfo {
    //get rid of comments and spaces (clippy says not to use nth...)
    let s = s
        .split(";;")
        .next()
        .expect("split produced empty iterator")
        .trim();
    if s.is_empty() {
        // is there an instruction on this line?
        InstrInfo::EmptyorMalformed
    } else if s == ")" {
        InstrInfo::FuncEnd
    } else if s.starts_with("(func") {
        let module = format!("module {s})");
        if let Ok(buf) = ParseBuffer::new(&module) {
            if parser::parse::<Module>(&buf).is_ok() {
                InstrInfo::FuncHeader
            } else {
                InstrInfo::EmptyorMalformed
            }
        } else {
            InstrInfo::EmptyorMalformed
        }
    } else if let Ok(buf) = ParseBuffer::new(s) {
        if let Ok(instr) = parser::parse::<Instruction>(&buf) {
            instr.into()
        } else {
            InstrInfo::EmptyorMalformed
        }
    } else {
        InstrInfo::EmptyorMalformed
    }
}

/// Convert from WebAssembly text format to binary format. Doesn't comprehensively enforce well-formedness.
pub fn text_to_binary(lines: &str) -> Result<Vec<u8>> {
    let txt = format!("module {lines}");
    Ok(parser::parse::<Module>(&ParseBuffer::new(&txt)?)?.encode()?)
}

/// Decides if a given string is a well-formed text-format Wasm function
///
/// Uses wast ParseBuffer to convert string into buffer and wast parser to parse buffer as Module
/// Encodes Module to binary Wasm and wasmparser parses binary Wasm
///
/// # Parameters
/// lines: A string slice representing a Wasm function
///
/// # Returns
/// true: if the function is syntactically well-formed; false otherwise
///
/// # Assumptions
/// Each instruction is plain
pub fn is_well_formed_func(lines: &str) -> bool {
    let parse_text = || {
        let text = format!("module (func {lines})");
        let binary = parser::parse::<Module>(&ParseBuffer::new(&text)?)?.encode()?;
        parse_binary_wasm(Parser::new(0), &binary)
    };
    parse_text().is_ok()
}

/// Returns input and output types for each instruction in a binary Wasm module
///
/// # Parameters
/// wasm_bin: a binary Wasm module
///
/// # Returns
/// A vector of strings where each string represents the input and output types for one instruction
///
/// # Assumptions
/// The function is valid
pub fn print_operands<'a>(
    wasm_bin: &[u8],
    ops: &Vec<Vec<(wasmparser::Operator<'a>, usize)>>,
) -> Result<Vec<(Vec<(ValType, usize)>, Vec<ValType>)>> {
    let mut validator = wasmparser::Validator::new();
    let parser = wasmparser::Parser::new(0);
    let mut result = Vec::new();
    let dummy_offset = 1; // no need to track offsets, but validator has safety checks against 0
    let mut ops_iter = ops.iter();

    for payload in parser.parse_all(wasm_bin) {
        if let wasmparser::ValidPayload::Func(func, _) = validator.payload(&payload?)? {
            let mut func_validator: wasmparser::FuncValidator<wasmparser::ValidatorResources> =
                func.into_validator(wasmparser::FuncValidatorAllocations::default());
            let Some(func_ops) = ops_iter.next() else {
                println!("Warning: more functions than ops provided");
                break;
            };
            let mut idx_stack: Vec<usize> = Vec::new();
            for (op, idx) in func_ops.iter() {
                let (pop_count, push_count) = op
                    .operator_arity(&func_validator.visitor(dummy_offset))
                    .ok_or(anyhow!("could not determine operator arity"))?;
                let prev_height = func_validator.operand_stack_height();
                let mut inputs = (prev_height - pop_count..prev_height)
                    .filter_map(|i| {
                        let valtype = func_validator.get_operand_type(i as usize).flatten();
                        let idx = idx_stack.pop();
                        match (valtype, idx) {
                            (Some(val), Some(idx)) => Some((val, idx)),
                            _ => None,
                        }
                    })
                    .collect::<Vec<_>>();
                for _ in &inputs {
                    idx_stack.pop();
                }
                println!("Validating operator: {op:?} with inputs: {inputs:?}");
                func_validator.op(dummy_offset, &op)?;
                let new_height = func_validator.operand_stack_height();
                let mut outputs = (new_height - push_count..new_height)
                    .filter_map(|i| func_validator.get_operand_type(i as usize).flatten())
                    .collect::<Vec<_>>();
                for _ in &outputs {
                    idx_stack.push(*idx);
                }
                inputs.reverse();
                outputs.reverse();
                result.push((inputs, outputs));
            }
        }
    }

    Ok(result)
}

fn valtype_to_str(ty: ValType) -> &'static str {
    match ty {
        ValType::I32 => "i32",
        ValType::I64 => "i64",
        ValType::F32 => "f32",
        ValType::F64 => "f64",
        ValType::V128 => "v128",
        ValType::Ref(_) => "ref",
    }
}

fn str_type_to_instr(s: &str) -> &str {
    match s {
        "i32" => "i32.const 0",
        "i64" => "i64.const 0",
        "f32" => "f32.const 0",
        "f64" => "f64.const 0",
        "v128" => "v128.const i32x4 0 0 0 0",
        _ => "ref",
    }
}

pub fn get_operators<'a>(wasm_bin: &'a [u8]) -> Vec<wasmparser::Operator<'a>> {
    let parser = wasmparser::Parser::new(0);
    let mut ops = Vec::new();
    for payload in parser.parse_all(wasm_bin) {
        if let wasmparser::Payload::CodeSectionEntry(body) = payload.unwrap() {
            for op in body.get_operators_reader().unwrap() {
                ops.push(op.unwrap());
            }
        }
    }
    ops
}

/// Represents a frame entry (like "block end" pair, etc.), with range recorded.
/// The range is inclusive, containing both start instr number and end instr number.
/// The start number begins at 0.
pub type Frame = RangeInclusive<usize>;

/// A list of occurances that require synthetic changes to appease the validator.
/// The first usize is the line where operators need to be inserted, followed
/// by a list of operators that need to be added in-order.
/// Finally, the trailing usize refers to the number of synthetic "drops" that
/// need to be added to balance out the additions.
pub type FixOperandsResult<'a> = Vec<((usize, Vec<wasmparser::Operator<'a>>), usize)>;

//
// Reconstruct the operators afterwards
// On input, fix the frames, then update the ops
// Returns a new OkModule (can we do that? We need to move ownership)
pub fn fix_operands<'a>(
    wasm_binary: &Vec<u8>,
    ops: &Vec<(wasmparser::Operator<'a>, usize)>,
) -> Result<FixOperandsResult<'a>> {
    let mut validator = wasmparser::Validator::new();
    let parser = wasmparser::Parser::new(0);

    for payload in parser.parse_all(wasm_binary) {
        if let wasmparser::ValidPayload::Func(func, body) = validator.payload(&payload?)? {
            let mut func_validator: wasmparser::FuncValidator<wasmparser::ValidatorResources> =
                func.into_validator(wasmparser::FuncValidatorAllocations::default());

            for (idx, func) in ops.iter().enumerate() {}
        }
    }

    return Ok(Vec::new());
}

pub fn test<'a>(wasm_bin: &[u8], ops: &Vec<Vec<(wasmparser::Operator<'a>)>>) -> Option<String> {
    let mut validator = wasmparser::Validator::new();
    let parser = wasmparser::Parser::new(0);
    let dummy_offset = 1; // no need to track offsets, but validator has safety checks against 0
    let mut ops_iter = ops.iter();

    for payload in parser.parse_all(wasm_bin) {
        println!("New payload: {payload:?}");
        if let wasmparser::ValidPayload::Func(func, _) =
            validator.payload(&payload.unwrap()).unwrap()
        {
            let mut func_validator: wasmparser::FuncValidator<wasmparser::ValidatorResources> =
                func.into_validator(wasmparser::FuncValidatorAllocations::default());
            let Some(func_ops) = ops_iter.next() else {
                println!("Warning: more functions than ops provided");
                break;
            };
            for op in func_ops.iter() {
                println!("Validating operator: {op:?}");
                let result = func_validator.op(dummy_offset, op);
                match result {
                    Err(e) => {
                        println!("Error validating operator: {e}");
                        let error = e.to_string();
                        if error.starts_with("type mismatch:") {
                            let ty = error
                                .trim_start_matches("type mismatch: expected ")
                                .split_once(' ')
                                .map(|(first, _)| first)
                                .unwrap_or(&error);
                            let instr = str_type_to_instr(ty);
                            return Some(instr.to_string());
                        }
                    }
                    Ok(_) => (),
                };
            }
        }
    }
    None
}

/// Fix frames by deactivated unmatched instrs and append **end** after the last instruction
///
/// ### Returns
/// A tuple of a vector containing the deactivated indices, and the number of **end** appended at the end.
pub fn fix_frames(instrs: &[InstrInfo]) -> (Vec<usize>, usize) {
    let mut deactivated = Vec::new();
    // Use stacks as memory of frame begining borders.
    let mut frame_border_stack = Vec::new();

    for (idx, instr) in instrs.iter().enumerate() {
        match instr {
            InstrInfo::If | InstrInfo::OtherStructured => frame_border_stack.push((instr, idx)),
            InstrInfo::Else => {
                if let Some(prev) = frame_border_stack.last().cloned()
                    && matches!(prev.0, InstrInfo::If)
                {
                    frame_border_stack.pop();
                    frame_border_stack.push((instr, idx));
                } else {
                    deactivated.push(idx);
                }
            }
            InstrInfo::End => {
                if !frame_border_stack.is_empty() {
                    frame_border_stack.pop();
                } else {
                    deactivated.push(idx);
                }
            }
            _ => (),
        }
    }

    (deactivated, frame_border_stack.len())
}

/// Match frames for (Instruction or empty).
///
/// ### Panics
/// When the input has unmatched frames
pub fn match_frames(instrs: &[InstrInfo]) -> Vec<Frame> {
    let mut frames = Vec::new();
    // Use stacks as memory of frame begining borders.
    let mut frame_border_stack = Vec::new();

    for (idx, instr) in instrs.iter().enumerate() {
        match instr {
            InstrInfo::If | InstrInfo::OtherStructured => {
                frame_border_stack.push((instr, idx));
            }
            InstrInfo::Else => {
                if let Some((prev, last_span_start)) = frame_border_stack.pop()
                    && matches!(prev, InstrInfo::If)
                {
                    let last_span_end = idx - 1;
                    frames.push(last_span_start..=last_span_end);
                } else {
                    panic!("Unmatched else");
                }
                frame_border_stack.push((instr, idx));
            }
            InstrInfo::End => {
                if let Some((_, last_span_start)) = frame_border_stack.pop() {
                    frames.push(last_span_start..=idx);
                } else {
                    panic!("Unmatched end");
                }
            }
            _ => (), // Ignore other instructions
        }
    }
    if !frame_border_stack.is_empty() {
        panic!("Unmatched block: {frame_border_stack:?}");
    }
    frames
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;
    use anyhow::Result;
    /*
    #[test]
    fn test_module_operator_map() -> Result<()> {
        let mut edit_lines = Vec::new();
        edit_lines.push(EditLine::new(0, String::from("(func"), InstrKind::Activated));
        edit_lines.push(EditLine::new(1, String::from("i32.const 2"), InstrKind::Activated));
        edit_lines.push(EditLine::new(2, String::from("i32.const 5"), InstrKind::Activated));
        edit_lines.push(EditLine::new(3, String::from("i32.add"), InstrKind::Activated));
        edit_lines.push(EditLine::new(4, String::from("\n"), InstrKind::Activated));
        edit_lines.push(EditLine::new(5, String::from("drop"), InstrKind::Activated));
        edit_lines.push(EditLine::new(6, String::from(""), InstrKind::Activated));
        edit_lines.push(EditLine::new(7, String::from(")"), InstrKind::Activated));

        edit_lines[4].set_kind(InstrKind::Deactivated);
        edit_lines[6].set_info(InstrInfo::EmptyorMalformed);

        let bin =
            text_to_binary("(func i32.const 2\ni32.const 5\ni32.add\ndrop\n)").expect("wasm_bin");
        let module = OkModule::build(bin, &edit_lines);
        assert!(module.is_ok());
        let mut op_indices = HashSet::new();
        if let Ok(all_ops) = module.unwrap().borrow_dependent() {
            for func in all_ops {
                for op in func {
                    // Ensure that line indices are in-bounds and unique.
                    assert!(op.1 < edit_lines.len());
                    assert!(op_indices.insert(op.1));
                }
            }
        }
        Ok(())
    }*/
}
