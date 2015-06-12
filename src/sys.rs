use std::io;

pub fn fromSysret(m: isize) -> Result<usize, io::Error> {
    if m < 0 { Err(io::Error::from_raw_os_error(-m as i32)) } else { Ok(m as usize) }
}
