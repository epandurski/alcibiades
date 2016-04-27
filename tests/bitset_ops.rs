extern crate socrates;


#[test]
#[allow(overflowing_literals)]
fn test_ls1b_functions() {
    use socrates::utils::{ls1b, clear_ls1b, above_ls1b_mask, below_lsb1_mask_including,
                          below_lsb1_mask, smeared_ls1b_up, smeared_ls1b_down};
    assert_eq!(ls1b(0x100100), 0x100);
    let mut x = 0x100100i64;
    clear_ls1b(&mut x);
    assert_eq!(x, 0x100000);
    let y = 0b11101000;
    assert_eq!(above_ls1b_mask(y), 0xfffffffffffffff0);
    assert_eq!(below_lsb1_mask_including(y), 0b1111);
    assert_eq!(below_lsb1_mask(y), 0b111);
    assert_eq!(smeared_ls1b_up(0b1010000), 0xfffffffffffffff0);
    assert_eq!(smeared_ls1b_down(0b1010000), 0b1011111);
}

#[test]
#[allow(overflowing_literals)]
fn test_bitscan() {
    use socrates::utils::{bitscan_forward};
    assert_eq!(bitscan_forward(0b1001101), 0);
    assert_eq!(bitscan_forward(0b1001000), 3);
    assert_eq!(bitscan_forward(0xf000000000000000), 60);
}
