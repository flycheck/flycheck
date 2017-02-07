// Doesn't compile, but since lib.rs is an implicit dependency of main.rs, this
// will cause an error when using `cargo rustc -- --error-format=json`.
fn foo() { zorglub }
