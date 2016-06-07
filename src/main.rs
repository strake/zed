#![feature(decode_utf8)]
#![feature(start)]
#![feature(unicode)]

extern crate std as core;

extern crate containers;
extern crate curse;
extern crate libc;
extern crate null_terminated;
extern crate unix;

mod actLog;
mod io;

use actLog::{ Act, ActLog };
use containers::collections::*;
use core::char::decode_utf8;
use core::cmp::*;
use core::mem;
use curse::{ Key };
use io::*;
use null_terminated::Nul;
use unix::err::OsErr;
use unix::file::*;
use unix::str::OsStr;

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

fn draw(ui: &mut curse::Term, b: &EditBuffer, topRow: usize) {
    assert!(topRow >= 1);
    let logTabStop = 3;
    let curse = |curs_x, p: &char| match *p {
        '\t' => nextTabStop(logTabStop, curs_x),
        _ => curs_x + 1
    };
    ui.clear();
    for (curs_y, xs) in b.buffer.xss.iter().skip(topRow - 1).enumerate().take(ui.height() - 1) {
        let mut curs_x = 0;
        for x in xs.iter() {
            ui.print_char(curs_x, curs_y, curse::Face::empty(), curse::Color::White, curse::Color::Black, *x);
            curs_x = curse(curs_x, x);
        }
    }
    drawStatus(ui, b.status);
    ui.set_cursor(b.buffer.xss[b.buffer.pt.1 - 1].iter().take(b.buffer.pt.0).fold(0, curse), b.buffer.pt.1 - topRow);
    ui.freshen();
}

fn print_chars<I: Iterator<Item = char>>(ui: &mut curse::Term, x_pos: usize, y_pos: usize, face: curse::Face, fg: curse::Color, bg: curse::Color, xs: I) {
    for (i, x) in xs.enumerate() { ui.print_char(x_pos+i, y_pos, face, fg, bg, x) }
}

fn drawStatus(ui: &mut curse::Term, stat: Status) {
    let fgcolor = if stat.failure { curse::Color::Red } else { curse::Color::White };
    let bgcolor = curse::Color::Black;
    let curs_y = ui.height() - 1;
    for curs_x in 0..ui.width() {
        ui.print_char(curs_x, curs_y, curse::REVERSE, fgcolor, bgcolor, ' ');
    }
    if stat.failure {
        print_chars(ui, 0, curs_y, curse::REVERSE | curse::BOLD, fgcolor, bgcolor, "OPERATION FAILED".chars());
    } else if stat.unsavedWork == UnsavedWorkFlag::Warned {
        print_chars(ui, 0, curs_y, curse::REVERSE, fgcolor, bgcolor, "WARNING: File modified; work will be lost! (once more to quit)".chars());
    } else {
        print_chars(ui, 0, curs_y, curse::REVERSE, fgcolor, bgcolor,
                    decode_utf8(stat.filePath.iter().map(|&b|b))
                        .map(|r| r.unwrap_or('\u{FFFD}')));
        for (i, &x) in ['[', match stat.unsavedWork { UnsavedWorkFlag::Saved => '-', _ => '*' }, ']'].into_iter().enumerate() {
            ui.print_char(stat.filePath.len() + i + 1, curs_y, curse::REVERSE, fgcolor, bgcolor, x);
        }
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

fn encode_utf8_raw(x: char, b: &mut [u8]) -> Option<usize> {
    let l = x.len_utf8();
    if b.len() < l { None } else { x.encode_utf8(b); Some(l) }
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
            xss: Vec::from_iter(open_at(None, path, OpenMode::RdOnly, OpenFlags::empty(), FileMode::empty())
                                    .unwrap().split(|x| x == b'\n', false).map(Result::<_, OsErr>::unwrap)
                                    .map(|s| Vec::from_iter(decode_utf8(s.into_iter())
                                                                .map(|r| r.unwrap_or('\u{FFFD}'))).ok()
                                                 .expect("alloc failed"))).ok().expect("alloc failed"),
            pt: (0, 1),
        },
        actLog: ActLog::new(),
        status: Status {
            filePath: path,
            unsavedWork: UnsavedWorkFlag::Saved,
            failure: false,
        },
    };
    let mut ui = curse::Term::init().unwrap();

    loop {
        draw(&mut ui, &b, 1);
        b.status.failure = !match ui.next_event(None) {
            Ok(Some(curse::Event::Key(key))) => match key {
                Key::Left  => { b.mv(Left,  Unit); true },
                Key::Right => { b.mv(Right, Unit); true },
                Key::Up    => { b.mv(Up,    Unit); true },
                Key::Down  => { b.mv(Down,  Unit); true },
                Key::Home  => { b.mv(Left,  End);  true },
                Key::End   => { b.mv(Right, End);  true },
                Key::Ctrl('c') => match b.status.unsavedWork {
                    UnsavedWorkFlag::Saved | UnsavedWorkFlag::Warned => { return 0 },
                    UnsavedWorkFlag::Modified => { b.status.unsavedWork = UnsavedWorkFlag::Warned; true },
                },
                Key::Ctrl('x') => {
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
                Key::Backspace => b.deleteBack(),
                Key::Tab     => b.insert('\t'),
                Key::Enter   => b.insert('\n'),
                Key::Char(x) => b.insert(x),
                _ => true,
            },
            Err(_) => false,
            _ => true
        };
    }
}
