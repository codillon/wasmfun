use anyhow::{Result, anyhow};
use std::ops::RangeInclusive;
use wasm_tools::parse_binary_wasm;
use wasmparser::{Parser, ValType};
use wast::core::{Instruction, Module};
use wast::parser::{self, ParseBuffer};
use self_cell::self_cell;

type Ops<'a> = Result<Vec<Vec<wasmparser::Operator<'a>>>>;

self_cell!(
    pub struct OkModule {
        owner: Vec<u8>,
        #[covariant]
        dependent: Ops,
    }

    impl {Debug}
);

impl OkModule {
    pub fn build(wasm_bin: Vec<u8>) -> Result<Self> {
        

        Ok(OkModule::new(wasm_bin, |wasm_bin| {
            let parser = wasmparser::Parser::new(0);
            let mut functions = Vec::new();

            for payload in parser.parse_all(&wasm_bin) {
                if let wasmparser::Payload::CodeSectionEntry(body) = payload? {
                    functions.push(
                        body.get_operators_reader()?
                            .into_iter()
                            .map(|x| Ok(x?))
                            .collect::<Result<Vec<_>>>()?,
                    );
                }
            }
            Ok(functions)
        }))
    }

    pub fn binary(&self) -> &[u8] {
        self.borrow_owner()
    }

    pub fn ops(&self) -> &Ops{
        self.borrow_dependent()
    }

    pub fn ops_len(&self, func_idx: usize) -> Result<usize, &anyhow::Error> {
        self.with_dependent(|_binary, ops| ops.as_ref().map(|ops| ops[func_idx].len()))
    }
}




#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstrInfo {
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
    } else {
        if let Ok(buf) = ParseBuffer::new(s) {
            if let Ok(instr) = parser::parse::<Instruction>(&buf) {
                instr.into()
            } else {
                InstrInfo::EmptyorMalformed
            }
        } else {
            InstrInfo::EmptyorMalformed
        }
    }
}

/// Convert from WebAssembly text format to binary format. Doesn't comprehensively enforce well-formedness.
pub fn text_to_binary(lines: &str) -> Result<Vec<u8>> {
    let txt = format!("module (func {lines})");
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
pub fn print_operands(wasm_bin: &[u8]) -> Result<Vec<String>> {
    let mut validator = wasmparser::Validator::new();
    let parser = wasmparser::Parser::new(0);
    let mut result = Vec::new();
    let dummy_offset = 1; // no need to track offsets, but validator has safety checks against 0

    for payload in parser.parse_all(wasm_bin) {
        if let wasmparser::ValidPayload::Func(func, body) = validator.payload(&payload?)? {
            let mut func_validator =
                func.into_validator(wasmparser::FuncValidatorAllocations::default());
            for op in body.get_operators_reader()? {
                let op = op?;
                let (pop_count, push_count) = op
                    .operator_arity(&func_validator.visitor(dummy_offset))
                    .ok_or(anyhow!("could not determine operator arity"))?;
                let prev_height = func_validator.operand_stack_height();
                let inputs = (prev_height - pop_count..prev_height)
                    .filter_map(|i| func_validator.get_operand_type(i as usize).flatten())
                    .map(valtype_to_str)
                    .collect::<Vec<_>>()
                    .join(" ");
                func_validator.op(dummy_offset, &op)?;
                let new_height = func_validator.operand_stack_height();
                let outputs = (new_height - push_count..new_height)
                    .filter_map(|i| func_validator.get_operand_type(i as usize).flatten())
                    .map(valtype_to_str)
                    .collect::<Vec<_>>()
                    .join(" ");
                result.push(format!("Inputs: [{inputs}] Returns: [{outputs}]"));
            }
        }
    }
    //remove the entry associated with the `end` at end of function body
    result.pop();

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
        panic!("Unmatched block: {:?}", frame_border_stack);
    }
    frames
}
