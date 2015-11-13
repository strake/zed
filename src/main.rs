#![feature(char_internals)]
#![feature(convert)]
#![feature(core)]

extern crate core;
extern crate libc;
#[macro_use]
extern crate syscall;
extern crate real;
extern crate rustbox;

mod file;
mod fs;
mod io;
mod posix;
mod random;
mod sys;

use core::default::Default;
use core::cmp::*;
use fs::*;
use real::vec::*;
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
                Some(xs) => self.xss.insert(self.pt.1, xs),
            },
            _ => self.xss[self.pt.1 - 1].insert(self.pt.0, x)
        } { return false };
        match x {
            '\n' => { self.pt.1 += 1; self.pt.0 = 0; },
            _ => { self.pt.0 += 1; }
        };
        true
    }

    fn deleteBack(&mut self) -> bool {
        if self.pt.0 > 0 {
            self.pt.0 -= 1;
            self.xss[self.pt.1 - 1].delete(self.pt.0);
            true
        } else if self.pt.1 > 1 {
            self.pt.1 -= 1;
            self.mv(Right, End);
            let xs = self.xss.delete(self.pt.1);
            self.xss[self.pt.1 - 1].append(xs)
        } else { true }
    }

    #[inline]
    fn mv(&mut self, a: Attitude, r: Reach) {
        match (a, r) {
            (Left,  End)  => { self.pt.0 = 0; },
            (Right, End)  => { self.pt.0 = self.xss[self.pt.1 - 1].length(); },
            (Up,    End)  => { self.pt.1 = 1; },
            (Down,  End)  => { self.pt.1 = self.xss.length() + 1; },
            (Left,  Unit) => if self.pt.0 > 0 { self.pt.0 -= 1; }
                             else { self.mv(Up,    Unit); self.mv(Right, End); },
            (Right, Unit) => if self.pt.0 < self.xss[self.pt.1 - 1].length() { self.pt.0 += 1 }
                             else { self.mv(Down,  Unit); self.mv(Left,  End); },
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
}

enum Attitude { Left, Right, Up, Down }

enum Reach { End, Unit }

fn nextTabStop(logTS: usize, pos: usize) -> usize { (pos + (1 << logTS)) & (!0 << logTS) }

fn draw(ui: &RustBox, b: &Buffer, topRow: usize, stat: Status) {
    assert!(topRow >= 1);
    let logTabStop = 3;
    let curse = |curs_x, p: &char| match *p {
        '\t' => nextTabStop(logTabStop, curs_x),
        _ => curs_x + 1
    };
    ui.clear();
    for (curs_y, xs) in b.xss.iter().skip(topRow - 1).enumerate().take(ui.height() - 1) {
        let mut curs_x = 0;
        for x in xs.iter() {
            ui.print_char(curs_x, curs_y, RB_NORMAL, rustbox::Color::White, rustbox::Color::Black, *x);
            curs_x = curse(curs_x, x);
        }
    }
    drawStatus(ui, stat);
    ui.set_cursor(b.xss[b.pt.1 - 1].iter().take(b.pt.0).fold(0, curse) as isize, (b.pt.1 - topRow) as isize);
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

fn main() {
    let (mut b, path_string) = match std::env::args().skip(1).next() {
        None => panic!("no file given"),
        Some(path) =>
            (Buffer {
                 xss: match std::fs::File::open(&path) {
                     Err(_) => Vec::new(),
                     Ok(f) => Vec::from_iter(std::io::BufReader::new(&f).
                                             lines().filter_map(Result::ok).
                                             map(|s| Vec::from_iter(s.chars()).
                                                     expect("alloc failed")).
                                             chain(Some(Vec::new()))).
                              expect("alloc failed"),
                 },
                 pt: (0, 1),
             }, path),
    };
    let path_bytes = { let mut p = path_string.clone().into_bytes(); p.push(0); p };
    let mut stat = Status {
        filePath: path_string.as_str(),
        unsavedWork: UnsavedWorkFlag::Saved,
        failure: false,
    };
    let ui = RustBox::init(Default::default()).unwrap();

    loop {
        draw(&ui, &b, 1, stat);
        stat.failure = !match ui.poll_event(false) {
            Ok(rustbox::Event::KeyEvent(Some(key))) => match key {
                Key::Left  => { b.mv(Left,  Unit); stat.unwarn(); true },
                Key::Right => { b.mv(Right, Unit); stat.unwarn(); true },
                Key::Up    => { b.mv(Up,    Unit); stat.unwarn(); true },
                Key::Down  => { b.mv(Down,  Unit); stat.unwarn(); true },
                Key::Home  => { b.mv(Left,  End);  stat.unwarn(); true },
                Key::End   => { b.mv(Right, End);  stat.unwarn(); true },
                Key::Ctrl('c') => match stat.unsavedWork {
                    UnsavedWorkFlag::Saved | UnsavedWorkFlag::Warned => { return },
                    UnsavedWorkFlag::Modified => { stat.unsavedWork = UnsavedWorkFlag::Warned; true },
                },
                Key::Ctrl('x') => {
                    let c = atomicWriteFileAt(
                        posix::fs::AT_FDCWD, &path_bytes[0] as *const u8, true,
                        |mut f| {
                            for (k, xs) in b.xss.iter().enumerate() {
                                try!(io::writeCode(
                                         |x, b| core::char::encode_utf8_raw(x as u32, b),
                                         &mut f,
                                         if k > 0 { "\n" } else { "" }.chars().chain(xs.iter().map(|p|*p))
                                ));
                            }
                            f.flush()
                        }
                    );
                    if c.is_ok() { stat.unsavedWork = UnsavedWorkFlag::Saved };
                    c.is_ok()
                },
                Key::Ctrl('h') |
                Key::Backspace => { stat.unsavedWork = UnsavedWorkFlag::Modified; b.deleteBack() },
                Key::Tab     => { stat.unsavedWork = UnsavedWorkFlag::Modified; b.insert('\t') },
                Key::Enter   => { stat.unsavedWork = UnsavedWorkFlag::Modified; b.insert('\n') },
                Key::Char(x) => { stat.unsavedWork = UnsavedWorkFlag::Modified; b.insert(x) },
                _ => true,
            },
            Err(_) => false,
            _ => true
        };
    }
}
