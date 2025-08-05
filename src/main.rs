use pipeline::editor::Editor;
use pipeline::utils::*;
pub fn main() {
    let mut editor = Editor::new();
    editor.input(0, "(func");
    editor.run_test();

/*
    let mut editor = Editor::new();
    editor.input(0, "i32.const 1");
    println!("{editor:?}");

*/

}
