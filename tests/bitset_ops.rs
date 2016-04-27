extern crate socrates;


#[test]
fn test_ls1b_functions() {
    use socrates::utils::{ls1b, clear_ls1b};
    assert_eq!(ls1b(0x100100), 0x100);
    let mut x = 0x100100i64;
    clear_ls1b(&mut x);
    assert_eq!(x, 0x100000);
}

