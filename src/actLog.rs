use libreal::vec::*;

pub struct Act {
    pub pt: (usize, usize),
    pub insert: Vec<char>,
    pub delete: Vec<char>,
}

pub struct ActLog {
    acta: Vec<Act>,
    pos: usize,
}

impl ActLog {
    #[inline]
    pub fn new() -> ActLog {
        ActLog {
            acta: Vec::new(),
            pos: 0,
        }
    }

    #[inline]
    pub fn reserve(&mut self, n: usize) -> bool {
        self.acta.reserve(n)
    }

    #[inline]
    pub fn ag(&mut self, act: Act) -> bool {
        self.acta.truncate(self.pos);
        let c = self.acta.push(act).is_ok();
        if c { self.pos += 1 };
        c
    }

    #[inline]
    pub fn unag(&mut self) -> Option<&Act> {
        if self.pos == 0 { None }
        else {
            self.pos -= 1;
            Some(&self.acta[self.pos])
        }
    }

    #[inline]
    pub fn reag(&mut self) -> Option<&Act> {
        if self.pos >= self.acta.length() { None }
        else {
            self.pos += 1;
            Some(&self.acta[self.pos - 1])
        }
    }
}
