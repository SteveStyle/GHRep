use std::cmp::Ord;

pub mod position {
    use std::ops::{Add, Sub, AddAssign};
    use std::fmt;
    use std::num::*;

    #[derive(PartialEq,Copy,Clone,Debug,Ord,PartialOrd,Eq)]
    pub struct Pos {
        pub x: i32,
        pub y: i32,
    }

    impl fmt::Display for Pos {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "({}, {})", self.x, self.y)
        }
    }
    
    impl Add for Pos {
        type Output = Self;
    
        fn add(self, other: Self) -> Self {
            Self {
                x: self.x + other.x,
                y: self.y + other.y,
            }
        }
    }

    impl AddAssign for Pos {
        fn add_assign(&mut self, other: Self) {
            *self = Self {
                x: self.x + other.x,
                y: self.y + other.y,
            }
        }
    }
    
    impl Sub for Pos {
        type Output = Self;
    
        fn sub(self, other: Self) -> Self {
            Self {
                x: self.x - other.x,
                y: self.y - other.y,
            }
        }
    }

    impl Pos {
        fn abs(self) -> Self {
            Self {
                x: self.x.abs(),
                y: self.y.abs(),
            }
        }
    }
    impl Pos {
        fn signum(self) -> Self {
            Self {
                x: self.x.signum(),
                y: self.y.signum(),
            }
        }
    }

    impl Pos {

        pub fn is_touching(head: &Pos, tail: &Pos ) -> bool {
            let absdiff = Pos::abs(*head - *tail);
            return absdiff.x <2 && absdiff.y <2;
        }
        
        pub fn move_tail(head: &Pos, tail: &Pos) -> Pos {
            if ! Pos::is_touching(head,tail) {
                return *tail + Pos::signum(* head - *tail );
            }
            return *tail;
        }
    }
    
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Pos;
    #[test]
    fn test_pos() {
        let pos1 = &Pos{x:0,y:0};
        let pos2 = &Pos{x:1,y:0};
        let pos3 = &Pos{x:-1,y:0};
        let pos4 = &Pos{x:-1,y:-1};
        let pos5 = &Pos{x:1,y:1};
        let pos6 = &Pos{x:-1,y:1};

        assert_eq!( Pos::is_touching(pos1,pos1), true);
        assert_eq!( Pos::is_touching(pos2,pos2), true);
        assert_eq!( Pos::is_touching(pos1,pos3), true);
        assert_eq!( Pos::is_touching(pos1,pos4), true);
        assert_eq!( Pos::is_touching(pos1,pos2), true);
        assert_eq!( Pos::is_touching(pos3,pos4), true);
        assert_eq!( Pos::is_touching(pos2,pos4), false);
        assert_eq!( Pos::is_touching(pos2,pos3), false);
        assert_eq!( Pos::is_touching(pos4,pos5), false);
        assert_eq!( Pos::is_touching(pos4,pos6), false);

        assert_eq!( &Pos::move_tail(pos1,pos1), pos1);
        assert_eq!( &Pos::move_tail(pos2,pos2), pos2);
        assert_eq!( &Pos::move_tail(pos1,pos3), pos3);
        assert_eq!( &Pos::move_tail(pos1,pos4), pos4);
        assert_eq!( &Pos::move_tail(pos1,pos2), pos2);
        assert_eq!( &Pos::move_tail(pos3,pos4), pos4);
        assert_eq!( &Pos::move_tail(pos2,pos4), pos1);
        assert_eq!( &Pos::move_tail(pos2,pos3), pos1);
        assert_eq!( &Pos::move_tail(pos4,pos5), pos1);
        assert_eq!( &Pos::move_tail(pos4,pos6), pos3);
        
    }
}