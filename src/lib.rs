pub mod utils;

#[cfg(test)]
mod test {
    #[test]
    fn it_works() {
    }
    #[test]
    fn test_attack_sets() {
        use utils::{generate_attack_and_blockers_arrays};
        let (att_sets, bl_sets) = generate_attack_and_blockers_arrays();
        assert_eq!(att_sets[0][0], 0b11<<8 | 0b10);
        assert_eq!(bl_sets[0][0], 0);
        assert_eq!(att_sets[2][0], 0b11111110 | 1<<8 | 1<<16 | 1<<24 | 1<<32 | 1<<40 | 1<<48 | 1<<56);
        assert_eq!(bl_sets[2][0], 0b01111110 | 1<<8 | 1<<16 | 1<<24 | 1<<32 | 1<<40 | 1<<48 | 0<<56);
    }
}
