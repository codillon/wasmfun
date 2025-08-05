use pipeline::editor::Editor;
pub fn main() {
    let mut editor = Editor::new();
    editor.input(0, "i32.const 1");
    println!("{editor:?}");
}
