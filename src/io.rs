extern crate core;

use std::io;
use std::io::{ Write };

pub fn writeCode<Encode, I, T, W>(mut encode: Encode, w: &mut W, xs : I) -> io::Result<usize>
  where Encode: FnMut(T, &mut [u8]) -> Option<usize>, I: Iterator<Item = T>, T: Copy, W: Write {
    let mut buf = [0; 4096];
    let mut pos = 0;
    let mut nBytesWritten = 0;
    for x in xs {
        'retry: loop {
            match encode(x, &mut buf[pos..]) {
                Some(n) => { pos += n; break 'retry; },
                None => {
                    w.write_all(&buf[0..pos]).map_err(fst2)?;
                    nBytesWritten += pos;
                    pos = 0;
                }
            }
        }
    }
    try!(w.write_all(&buf[0..pos]));
    nBytesWritten += pos;
    Ok(nBytesWritten)
}
