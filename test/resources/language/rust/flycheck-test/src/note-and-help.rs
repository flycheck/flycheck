// a note/info about lifetimes

// a non-POD type
struct NonPOD;
impl Drop for NonPOD {
    fn drop(&mut self) {}
}

fn main() {
    let _x = NonPOD;
    let _y = _x;          // note: x's lifetime ends here
    let _z = _x;          // error: can no longer move x
}
