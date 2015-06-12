#![feature(core)]
#![feature(libc)]

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
use rustbox::{ RustBox, Key, RB_NORMAL };
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

fn draw(ui: &RustBox, b: &Buffer, topRow: usize) {
    assert!(topRow >= 1);
    let logTabStop = 3;
    let curse = |curs_x, p: &char| match *p {
        '\t' => nextTabStop(logTabStop, curs_x),
        _ => curs_x + 1
    };
    ui.clear();
    for (curs_y, xs) in b.xss.iter().skip(topRow - 1).enumerate() {
        let mut curs_x = 0;
        for x in xs.iter() {
            ui.print_char(curs_x, curs_y, RB_NORMAL, rustbox::Color::White, rustbox::Color::Black, *x);
            curs_x = curse(curs_x, x);
        }
    }
    ui.set_cursor(b.xss[b.pt.1 - 1].iter().take(b.pt.0).fold(0, curse) as isize, (b.pt.1 - topRow) as isize);
    ui.present();
}

fn main() {
    let (mut b, path) = match std::env::args().skip(1).next() {
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
             }, { let mut p = path.into_bytes(); p.push(0); p }),
    };
    let ui = RustBox::init(Default::default()).unwrap();

    loop {
        draw(&ui, &b, 1);
        match ui.poll_event(false) {
            Ok(rustbox::Event::KeyEvent(Some(key))) => match key {
                Key::Left  => { b.mv(Left,  Unit); true },
                Key::Right => { b.mv(Right, Unit); true },
                Key::Up    => { b.mv(Up,    Unit); true },
                Key::Down  => { b.mv(Down,  Unit); true },
                Key::Home  => { b.mv(Left,  End);  true },
                Key::End   => { b.mv(Right, End);  true },
                Key::Ctrl('c') => { return },
                Key::Ctrl('x') => atomicWriteFileAt(
                                      posix::fs::AT_FDCWD, &path[0] as *const u8, true,
                                      |mut f| {
                                          for (k, xs) in b.xss.iter().enumerate() {
                                              try!(io::writeCode(
                                                       |x, b| core::char::encode_utf8_raw(x as u32, b),
                                                       &mut f,
                                                       if k > 0 { "\n" } else { "" }.chars().
                                                       chain(xs.iter().map(|p|*p))
                                                   ));
                                          }
                                          f.flush()
                                      }
                                  ).is_ok(),
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
