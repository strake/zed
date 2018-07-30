#![no_std]

#![feature(core_intrinsics)]
#![feature(lang_items)]
#![feature(panic_implementation)]
#![feature(panic_info_message)]
#![feature(start)]

extern crate containers;
extern crate cursebox;
extern crate default_allocator;
extern crate fmt;
extern crate libc;
extern crate loca;
extern crate null_terminated;
#[macro_use]
extern crate syscall;
extern crate unix;
extern crate utf;

mod actLog;
mod io;

use actLog::{ Act, ActLog };
use containers::collections::*;
use core::cmp::*;
use core::mem;
use cursebox::*;
use io::*;
use loca::Alloc;
use null_terminated::Nul;
use unix::err::OsErr;
use unix::file::*;
use unix::str::OsStr;
use utf::{UtfExt, decode_utf8};

use Attitude::*;
use Reach::*;

struct Buffer {
    xss: Vec<Vec<char>>,
    pt: (usize, usize),
}

impl Buffer {
    fn insert(&mut self, x: char) -> bool {
        if !match x {
            '\n' => match self.xss[self.pt.1 - 1].split_off(self.pt.0) {
                None => false,
                Some(xs) => self.xss.insert(self.pt.1, xs).is_ok(),
            },
            _ => self.xss[self.pt.1 - 1].insert(self.pt.0, x).is_ok()
        } { return false };
        match x {
            '\n' => { self.pt.1 += 1; self.pt.0 = 0; },
            _ => { self.pt.0 += 1; }
        };
        true
    }

    fn deleteBack(&mut self) -> Option<Option<char>> {
        if self.pt.0 > 0 {
            self.pt.0 -= 1;
            Some(Some(self.xss[self.pt.1 - 1].delete(self.pt.0)))
        } else if self.pt.1 > 1 {
            self.pt.1 -= 1;
            self.mv(Right, End);
            let n = self.pt.1;
            if self.joinRow(n) { Some(Some('\n')) } else { None }
        } else { Some(None) }
    }

    fn deleteForth(&mut self) -> Option<Option<char>> {
        if self.pt.0 < self.xss[self.pt.1 - 1].len() {
            Some(Some(self.xss[self.pt.1 - 1].delete(self.pt.0)))
        } else if self.pt.1 <= self.xss.len() {
            let n = self.pt.1;
            if self.joinRow(n) { Some(Some('\n')) } else { None }
        } else { Some(None) }
    }

    fn joinRow(&mut self, n: usize) -> bool {
        let l = self.xss[n].len();
        self.xss[self.pt.1 - 1].reserve(l) && {
            let xs = self.xss.delete(n);
            let _ = self.xss[n-1].append(xs);
            true
        }
    }

    #[inline]
    fn mv(&mut self, a: Attitude, r: Reach) {
        match (a, r) {
            (Left,  End)  => { self.pt.0 = 0; },
            (Right, End)  => { self.pt.0 = self.xss[self.pt.1 - 1].len(); },
            (Up,    End)  => { self.pt.1 = 1; },
            (Down,  End)  => { self.pt.1 = self.xss.len() + 1; },
            (Left,  Unit) => if self.pt.0 > 0 { self.pt.0 -= 1; }
                             else if self.pt.1 > 1 { self.pt.1 -= 1; self.mv(Right, End); },
            (Right, Unit) => if self.pt.0 < self.xss[self.pt.1 - 1].len() { self.pt.0 += 1 }
                             else if self.pt.1 < self.xss.len() { self.pt.1 += 1; self.mv(Left, End); },
            (Up,    Unit) => if self.pt.1 > 1 {
                                 self.pt.1 -= 1;
                                 self.pt.0 = min(self.pt.0, self.xss[self.pt.1 - 1].len());
                             },
            (Down,  Unit) => if self.pt.1 < self.xss.len() {
                                 self.pt.1 += 1;
                                 self.pt.0 = min(self.pt.0, self.xss[self.pt.1 - 1].len());
                             },
        }
    }

    #[inline]
    fn ag(&mut self, pt: (usize, usize), insert: &Vec<char>, delete: &Vec<char>) -> bool {
        self.pt = pt;
        for &_ in delete.iter() { match self.deleteForth() { None => return false, _ => () } };
        for &x in insert.iter() { if !self.insert(x) { return false } };
        true
    }
}

enum Attitude { Left, Right, Up, Down }

enum Reach { End, Unit }

fn nextTabStop(logTS: usize, pos: usize) -> usize { (pos + (1 << logTS)) & (!0 << logTS) }

fn draw<A: Alloc>(ui: &mut cursebox::UI<A>, b: &EditBuffer, topRow: usize) {
    assert!(topRow >= 1);
    let logTabStop = 3;
    let curse = |curs_x, p: char| match p {
        '\t' => nextTabStop(logTabStop, curs_x),
        _ => curs_x + 1
    };
    ui.clear();
    for (curs_y, xs) in b.buffer.xss.iter().skip(topRow - 1).enumerate().take(ui.height() - 1) {
        let mut curs_x = 0;
        for &x in xs.iter() {
            if let Some(p) = ui.cells_mut().at_mut(curs_x, curs_y) { *p = Cell { ch: x as _, fg: cursebox::Attr::White, bg: cursebox::Attr::Black } }
            curs_x = curse(curs_x, x);
        }
    }
    drawStatus(ui, b.status, b.buffer.pt, b.buffer.xss.len());
    ui.set_cursor(b.buffer.xss[b.buffer.pt.1 - 1].iter().cloned().take(b.buffer.pt.0).fold(0, curse), b.buffer.pt.1 - topRow);
    ui.present();
}

fn drawStatus<A: Alloc>(ui: &mut cursebox::UI<A>, stat: Status, pt: (usize, usize), n: usize) {
    let fg = cursebox::Attr::Black;
    let bg = if stat.failure { cursebox::Attr::Red } else { cursebox::Attr::White };
    let y = ui.height() - 1;
    for x in 0..ui.width() {
        if let Some(p) = ui.cells_mut().at_mut(x, y) { *p = Cell { ch: ' ' as _, fg, bg } }
    }
    let mut pr = ScreenPrinter { pt: (0, y), fg, bg, term: ui };
    use core::fmt::Write;
    if stat.failure {
        pr.write_str("OPERATION FAILED")
    } else if stat.unsavedWork == UnsavedWorkFlag::Warned {
        pr.write_str("WARNING: File modified; work will be lost! (once more to quit)")
    } else {
        write!(&mut pr, "{} [{}] ({}/{}),{}",
               ::fmt::Items(decode_utf8(stat.filePath.iter().cloned()).map(|x| x.unwrap_or('\u{FFFD}'))),
               match stat.unsavedWork { UnsavedWorkFlag::Saved => '-', _ => '*' }, pt.1, n, pt.0)
    }.unwrap_or(())
}

#[derive(Debug)]
struct ScreenPrinter<'a, A: 'a + Alloc> {
    pt: (usize, usize),
    fg: cursebox::Attr,
    bg: cursebox::Attr,
    term: &'a mut cursebox::UI<A>,
}

impl<'a, A: Alloc> ScreenPrinter<'a, A> {
    #[inline]
    fn print_chars<I: Iterator<Item = char>>(&mut self, xs: I) -> usize {
        let mut n = 0;
        for (i, x) in xs.enumerate() {
            if let Some(p) = self.term.cells_mut().at_mut(self.pt.0+i, self.pt.1) { *p = Cell { ch: x as _, fg: self.fg, bg: self.bg } }
            n += 1;
        }
        n
    }
}

impl<'a, A: Alloc> ::core::fmt::Write for ScreenPrinter<'a, A> {
    #[inline]
    fn write_str(&mut self, s: &str) -> ::core::fmt::Result {
        self.pt.0 += self.print_chars(s.chars());
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq)]
struct Status<'a> {
    filePath: &'a OsStr,
    unsavedWork: UnsavedWorkFlag,
    failure: bool,
}

impl<'a> Status<'a> {
    fn unwarn(&mut self) {
        if self.unsavedWork == UnsavedWorkFlag::Warned { self.unsavedWork = UnsavedWorkFlag::Modified };
    }
}

#[derive(Clone, Copy, PartialEq)]
enum UnsavedWorkFlag {
    Saved,
    Modified,
    Warned,
}

struct EditBuffer<'a> {
    buffer: Buffer,
    actLog: ActLog,
    status: Status<'a>,
}

impl<'a> EditBuffer<'a> {
    #[inline] fn insert(&mut self, x: char) -> bool {
        let pt = self.buffer.pt;
        let xs = match Vec::from_iter([x].into_iter().map(|p|*p)) { Err(_) => return false, Ok(xs) => xs };
        self.actLog.reserve(1) && self.buffer.insert(x) &&
        self.actLog.ag(Act { pt: pt, insert: xs, delete: Vec::new() }) &&
        { self.status.unsavedWork = UnsavedWorkFlag::Modified; true }
    }

    #[inline] fn deleteBack(&mut self) -> bool {
        if self.buffer.pt.0 == 0 && self.buffer.pt.1 == 1 { return true };
        let mut xs = match Vec::with_capacity(1) { None => return false, Some(xs) => xs };
        self.actLog.reserve(1) && match self.buffer.deleteBack() {
            None => false,
            Some(None) => true,
            Some(Some(x)) => {
                let _ = xs.push(x);
                self.status.unsavedWork = UnsavedWorkFlag::Modified;
                self.actLog.ag(Act { pt: self.buffer.pt, insert: Vec::new(), delete: xs })
            },
        }
    }

    #[inline] fn mv(&mut self, a: Attitude, r: Reach) {
        self.buffer.mv(a, r);
        self.status.unwarn();
    }

    #[inline] fn unag(&mut self) -> bool { match self.actLog.unag() { None => true, Some(act) => self.buffer.ag(act.pt, &act.delete, &act.insert) } }
    #[inline] fn reag(&mut self) -> bool { match self.actLog.reag() { None => true, Some(act) => self.buffer.ag(act.pt, &act.insert, &act.delete) } }
}

#[inline]
fn encode_utf8_raw(x: char, b: &mut [u8]) -> Option<usize> {
    x.try_encode_utf8(b).map(|s| s.len())
}

#[start]
fn start(_: isize, c_argv: *const *const u8) -> isize {
    extern { static environ: *const *const u8; }
    unsafe { main(mem::transmute(c_argv), mem::transmute(environ)) }
}

fn main(args: &'static Nul<&'static Nul<u8>>,
        _env: &'static Nul<&'static Nul<u8>>) -> isize {
    let path: &'static Nul<u8> = args.iter().skip(1).next().expect("no file given");
    let mut b = EditBuffer {
        buffer: Buffer {
            xss: match open_at(None, path, OpenMode::RdOnly, FileMode::empty()) {
                Ok(f) => Vec::from_iter(f.split(|x| x == b'\n', false).map(Result::<_, OsErr>::unwrap)
                                         .map(|s| Vec::from_iter(decode_utf8(s.into_iter())
                                                                     .map(|r| r.unwrap_or('\u{FFFD}'))).ok().expect("alloc failed"))).ok().expect("alloc failed"),
                Err(::unix::err::ENOENT) => { let mut xss = Vec::new(); xss.push(Vec::new()).unwrap(); xss },
                Err(e) => panic!("failed to open file: {:?}", e),
            },
            pt: (0, 1),
        },
        actLog: ActLog::new(),
        status: Status {
            filePath: path,
            unsavedWork: UnsavedWorkFlag::Saved,
            failure: false,
        },
    };
    let mut ui = cursebox::UI::new_in(default_allocator::Heap).unwrap();
    let mut topRow = 1;

    loop {
        {
            let h = ui.height();
            if b.buffer.pt.1 <  topRow         { topRow = b.buffer.pt.1; }
            if b.buffer.pt.1 >= topRow + h - 1 { topRow = b.buffer.pt.1 + 1 - h; }
        }
        draw(&mut ui, &b, topRow);
        b.status.failure = !match ui.fetch_event(None) {
            Ok(Some(cursebox::Event::Key(mod_, key))) => match key {
                Key::Left  => { b.mv(Left,  Unit); true },
                Key::Right => { b.mv(Right, Unit); true },
                Key::Up    => { b.mv(Up,    Unit); true },
                Key::Down  => { b.mv(Down,  Unit); true },
                Key::Home  => { b.mv(Left,  End);  true },
                Key::End   => { b.mv(Right, End);  true },
                Key::PgUp  => { for _ in 0..ui.height() { b.mv(Up,    Unit); } true },
                Key::PgDn  => { for _ in 0..ui.height() { b.mv(Down,  Unit); } true },
                Key::Char('\x03' /* ^C */) => match b.status.unsavedWork {
                    UnsavedWorkFlag::Saved | UnsavedWorkFlag::Warned => { return 0 },
                    UnsavedWorkFlag::Modified => { b.status.unsavedWork = UnsavedWorkFlag::Warned; true },
                },
                Key::Char('\x18' /* ^X */) => {
                    let c = atomic_write_file_at(
                        None, path, Clobber, (FilePermission::Read | FilePermission::Write) << USR,
                        |mut f| {
                            for (k, xs) in b.buffer.xss.iter().enumerate() {
                                try!(io::writeCode(
                                         encode_utf8_raw, &mut f,
                                         if k > 0 { "\n" } else { "" }.chars().chain(xs.iter().map(|p|*p))
                                ));
                            }
                            f.flush()
                        }
                    );
                    if c.is_ok() { b.status.unsavedWork = UnsavedWorkFlag::Saved };
                    c.is_ok()
                },
                Key::Char('\x1A' /* ^Z */) => unsafe {
                    ui.stop();
                    let c = syscall!(KILL, syscall!(GETPID), ::libc::SIGSTOP);
                    ui.start();
                    0 == c
                },
                Key::Char('\x08') => b.deleteBack(),
                Key::Char('\r') => b.insert('\n'),
                Key::Char(x) => b.insert(x),
                _ => true,
            },
            Err(_) => false,
            _ => true
        };
    }
}

#[cold]
#[inline(never)]
#[panic_implementation]
#[no_mangle]
pub fn panic(info: &::core::panic::PanicInfo) -> ! {
    #[cfg(debug_assertions)]
    {
        use core::fmt::Write;
        let mut pr = File::new_unchecked(2);
        match (info.location(), info.message()) {
            (None, None) => writeln!(pr, "PANIC!"),
            (Some(loc), None) => writeln!(pr, "PANIC at {}!", loc),
            (None, Some(msg)) => writeln!(pr, "PANIC: {}", msg),
            (Some(loc), Some(msg)) => writeln!(pr, "PANIC at {}: {}", loc, msg),
        }.unwrap_or(())
    }

    let _ = info;

    unsafe {
        syscall!(KILL, syscall!(GETPID), ::libc::SIGABRT);
        ::core::intrinsics::abort()
    }
}

#[lang = "eh_personality"]
extern "C" fn eh_personality() {}
