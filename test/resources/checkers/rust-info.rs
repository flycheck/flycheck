// a note/info about lifetimes

// a non-POD type
struct NonPOD;
impl Drop for NonPOD {
    fn drop(&mut self) {}
}

fn main() {
    let x = NonPOD;
    let y = x;          // note: x's lifetime ends here
    let z = x;          // error: can no longer move x
}
