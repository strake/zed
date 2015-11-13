extern crate core;
extern crate libreal;
extern crate std;

use std::io;
use std::io::{ Write };

pub struct File { pub fd : isize }

impl Drop for File {
    #[inline]
    fn drop(&mut self) { unsafe { syscall!(CLOSE, self.fd) }; }
}

impl Write for File {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let m = unsafe { syscall!(WRITE, self.fd, &buf[0] as *const u8, buf.len()) as isize };
        if m < 0 { Err(io::Error::from_raw_os_error(-m as i32)) } else { Ok(m as usize) }
    }

    #[inline]
    fn flush(&mut self) -> io::Result<()> { Ok(()) }
}
