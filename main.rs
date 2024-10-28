// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run
use bitfield::*;

type A = B1;
type B = B3;
type C = B4;
type D = B24;

#[bitfield]
pub struct MyFourBytes {
    a: A,
    b: B,
    c: C,
    d: D,
}

fn main() {
    assert_eq!(<B24 as Specifier>::BITS, 24);
}
