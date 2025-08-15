use pipeline::editor::Editor;
use pipeline::utils::*;
pub fn main() {
    let mut editor = Editor::new();
    println!("Printing Editor: {editor:?}");
    editor.add_ui_elements();
    println!("Printing Editor: {editor:?}");
    /*
        let mut editor = Editor::new();
        editor.input(0, "i32.const 1");
        println!("{editor:?}");

    */
}
