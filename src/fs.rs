use core::ops::*;
use file::*;
use libc;
use random::*;
use std::io;

fn randname(cs : &mut [u8]) {
    let base = 'Z' as u64 - 'A' as u64 + 1;
    let mut n : u64 = Random::random();
    for p in cs.iter_mut() {
        *p = (n % base + 'A' as u64) as u8;
        n /= base;
    }
}

fn mktempat<'a, R : Clone>(dirfd : isize, template : &mut [u8], range : R, flags : usize) -> io::Result<File> where [u8] : IndexMut<R, Output = [u8]> {
    let tries = 65536;
    let mut fd = -libc::EEXIST as isize;
    for _ in 1..tries {
        randname(&mut template[range.clone()]);
        fd = unsafe { syscall!(OPENAT, dirfd, &template[0] as *const u8, flags | (libc::O_RDWR | libc::O_CREAT | libc::O_EXCL) as usize, 0o600) as isize };
        if fd >= 0 { return Ok(File { fd : fd }) };
        if fd < 0 && fd != -libc::EEXIST as isize { break };
    }
    return Err(io::Error::from_raw_os_error(-fd as i32));
}

pub fn atomicWriteFileAt<F : FnMut(File) -> io::Result<T>, T>(dirfd : isize, path : *const u8, overwrite : bool, mut writer : F) -> io::Result<T> {
    let mut tmpPath = [0; 13];
    let f = try!(mktempat(dirfd, &mut tmpPath, 0..12, 0));
    let m = try!(writer(f));
    unsafe {
        if overwrite {
            try!(::sys::fromSysret(syscall!(RENAMEAT, dirfd, &tmpPath[0] as *const u8, dirfd, path) as isize));
        } else {
            try!(::sys::fromSysret(syscall!(LINKAT,   dirfd, &tmpPath[0] as *const u8, dirfd, path) as isize));
            syscall!(UNLINKAT, dirfd, &tmpPath[0] as *const u8, 0);
        }
    }
    Ok(m)
}
