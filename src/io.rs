use core::mem;
pub use io::*;

fn fst2<S, T>((x, _): (S, T)) -> S { x }

pub fn writeCode<Codon, Encode, I, T, W>(mut encode: Encode, w: &mut W, xs : I) -> Result<usize, W::Err>
  where Codon: Copy, Encode: FnMut(T, &mut [Codon]) -> Option<usize>, I: Iterator<Item = T>, T: Copy, W: Write<Codon>, W::Err: From<EndOfFile> {
    let mut buf: [Codon; 4096] = unsafe { mem::MaybeUninit::uninit().assume_init() };
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
    w.write_all(&buf[0..pos]).map_err(fst2)?;
    nBytesWritten += pos;
    Ok(nBytesWritten)
}
