#![feature(char_internals)]
#![feature(convert)]
#![feature(core)]

extern crate core;
extern crate libc;
extern crate libreal;
#[macro_use]
extern crate syscall;
extern crate rustbox;

mod actLog;
mod file;
mod fs;
mod io;
mod posix;
mod random;
mod sys;

use actLog::{ Act, ActLog };
use core::default::Default;
use core::cmp::*;
use fs::*;
use libreal::vec::*;
use rustbox::{ RustBox, Key, RB_NORMAL, RB_REVERSE, RB_BOLD };
use std::io::{ BufRead, Write };

use Attitude::*;
use Reach::*;

struct Buffer {
    xss: Vec<Vec<char>>,
    pt: (usize, usize),
}

impl Buffer {
    fn insert(&mut self, x: char) -> bool {
        if !match x {
            '\n' => match self.xss[self.pt.1 - 1].splitOff(self.pt.0) {
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
        if self.pt.0 < self.xss[self.pt.1 - 1].length() {
            Some(Some(self.xss[self.pt.1 - 1].delete(self.pt.0)))
        } else if self.pt.1 <= self.xss.length() {
            let n = self.pt.1;
            if self.joinRow(n) { Some(Some('\n')) } else { None }
        } else { Some(None) }
    }

    fn joinRow(&mut self, n: usize) -> bool {
        let l = self.xss[n].length();
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
            (Right, End)  => { self.pt.0 = self.xss[self.pt.1 - 1].length(); },
            (Up,    End)  => { self.pt.1 = 1; },
            (Down,  End)  => { self.pt.1 = self.xss.length() + 1; },
            (Left,  Unit) => if self.pt.0 > 0 { self.pt.0 -= 1; }
                             else if self.pt.1 > 1 { self.pt.1 -= 1; self.mv(Right, End); },
            (Right, Unit) => if self.pt.0 < self.xss[self.pt.1 - 1].length() { self.pt.0 += 1 }
                             else if self.pt.1 < self.xss.length() { self.pt.1 += 1; self.mv(Left, End); },
            (Up,    Unit) => if self.pt.1 > 1 {
                                 self.pt.1 -= 1;
                                 self.pt.0 = min(self.pt.0, self.xss[self.pt.1 - 1].length());
                             },
            (Down,  Unit) => if self.pt.1 < self.xss.length() {
                                 self.pt.1 += 1;
                                 self.pt.0 = min(self.pt.0, self.xss[self.pt.1 - 1].length());
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

fn draw(ui: &RustBox, b: &EditBuffer, topRow: usize) {
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
            ui.print_char(curs_x, curs_y, RB_NORMAL, rustbox::Color::White, rustbox::Color::Black, *x);
            curs_x = curse(curs_x, x);
        }
    }
    drawStatus(ui, b.status);
    ui.set_cursor(b.buffer.xss[b.buffer.pt.1 - 1].iter().take(b.buffer.pt.0).fold(0, curse) as isize, (b.buffer.pt.1 - topRow) as isize);
    ui.present();
}

fn drawStatus(ui: &RustBox, stat: Status) {
    let fgcolor = if stat.failure { rustbox::Color::Red } else { rustbox::Color::White };
    let bgcolor = rustbox::Color::Black;
    let curs_y = ui.height() - 1;
    for curs_x in 0..ui.width() {
        ui.print_char(curs_x, curs_y, RB_REVERSE, fgcolor, bgcolor, ' ');
    }
    if stat.failure {
        ui.print(0, curs_y, RB_REVERSE | RB_BOLD, fgcolor, bgcolor, "OPERATION FAILED");
    } else if stat.unsavedWork == UnsavedWorkFlag::Warned {
        ui.print(0, curs_y, RB_REVERSE, fgcolor, bgcolor, "WARNING: File modified; work will be lost! (once more to quit)");
    } else {
        ui.print(0, curs_y, RB_REVERSE, fgcolor, bgcolor, stat.filePath);
        for (i, &x) in ['[', match stat.unsavedWork { UnsavedWorkFlag::Saved => '-', _ => '*' }, ']'].into_iter().enumerate() {
            ui.print_char(stat.filePath.len() + i + 1, curs_y, RB_REVERSE, fgcolor, bgcolor, x);
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
struct Status<'a> {
    filePath: &'a str,
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
        let mut xs = match Vec::withCapacity(1) { None => return false, Some(xs) => xs };
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

fn main() {
    let (mut b, path_string) = match std::env::args().skip(1).next() {
        None => panic!("no file given"),
        Some(path) =>
            (EditBuffer {
                 buffer: Buffer {
                     xss: match std::fs::File::open(&path) {
                         Err(_) => Vec::new(),
                         Ok(f) => Vec::from_iter(std::io::BufReader::new(&f).
                                                 lines().filter_map(Result::ok).
                                                 map(|s| Vec::from_iter(s.chars()).ok().
                                                         expect("alloc failed")).
                                                 chain(Some(Vec::new()))).ok().
                                  expect("alloc failed"),
                     },
                     pt: (0, 1),
                 },
                 actLog: ActLog::new(),
                 status: Status {
                     filePath: "",
                     unsavedWork: UnsavedWorkFlag::Saved,
                     failure: false,
                 },
             }, path),
    };
    let path_bytes = { let mut p = path_string.clone().into_bytes(); p.push(0); p };
    b.status.filePath = path_string.as_str();
    let ui = RustBox::init(Default::default()).unwrap();

    loop {
        draw(&ui, &b, 1);
        b.status.failure = !match ui.poll_event(false) {
            Ok(rustbox::Event::KeyEvent(Some(key))) => match key {
                Key::Left  => { b.mv(Left,  Unit); true },
                Key::Right => { b.mv(Right, Unit); true },
                Key::Up    => { b.mv(Up,    Unit); true },
                Key::Down  => { b.mv(Down,  Unit); true },
                Key::Home  => { b.mv(Left,  End);  true },
                Key::End   => { b.mv(Right, End);  true },
                Key::Ctrl('c') => match b.status.unsavedWork {
                    UnsavedWorkFlag::Saved | UnsavedWorkFlag::Warned => { return },
                    UnsavedWorkFlag::Modified => { b.status.unsavedWork = UnsavedWorkFlag::Warned; true },
                },
                Key::Ctrl('x') => {
                    let c = atomicWriteFileAt(
                        posix::fs::AT_FDCWD, &path_bytes[0] as *const u8, true,
                        |mut f| {
                            for (k, xs) in b.buffer.xss.iter().enumerate() {
                                try!(io::writeCode(
                                         |x, b| core::char::encode_utf8_raw(x as u32, b),
                                         &mut f,
                                         if k > 0 { "\n" } else { "" }.chars().chain(xs.iter().map(|p|*p))
                                ));
                            }
                            f.flush()
                        }
                    );
                    if c.is_ok() { b.status.unsavedWork = UnsavedWorkFlag::Saved };
                    c.is_ok()
                },
                Key::Ctrl('h') |
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
