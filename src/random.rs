pub trait Random {
    fn random() -> Self;
}

impl Random for u32 {
    #[inline]
    fn random() -> u32 {
        let mut x = 0;
        unsafe { syscall!(GETRANDOM, &mut x as *mut u32, 4, 0) };
        x
    }
}

impl Random for u64 {
    #[inline]
    fn random() -> u64 {
        let mut x = 0;
        unsafe { syscall!(GETRANDOM, &mut x as *mut u64, 8, 0) };
        x
    }
}
