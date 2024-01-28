//extern crate num_traits;

//use num_traits::Num;
use std::fmt::Debug;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct Pos {
    pub x: usize,
    pub y: usize,
}
// implement add and subtract for position
impl std::ops::Add<Pos> for Pos {
    type Output = Pos;
    fn add(self, other: Pos) -> Pos {
        Pos {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}
impl std::ops::Add<Direction> for Pos {
    type Output = Pos;
    fn add(self, other: Direction) -> Pos {
        self.add(other.to_position())
    }
}
impl std::ops::Sub<Pos> for Pos {
    type Output = Pos;
    fn sub(self, other: Pos) -> Pos {
        Pos {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}
impl std::ops::Mul<isize> for Pos {
    type Output = Pos;
    fn mul(self, other: isize) -> Pos {
        Pos {
            x: self.x * other,
            y: self.y * other,
        }
    }
}

// implement the new function for Position
impl Pos {
    pub fn new(x: isize, y: isize) -> Pos {
        Pos { x, y }
    }

    pub fn is_adjacent(&self, other: &Pos) -> bool {
        let diff = *self - *other;
        diff.x.abs() <= 1 && diff.y.abs() <= 1
    }
    pub fn is_orthogonal(&self, other: &Pos) -> bool {
        let diff = *self - *other;
        diff.x.abs() <= 1 && diff.y.abs() <= 1 && diff.x.abs() != diff.y.abs()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    North = 0,
    East,
    South,
    West,
}

impl std::ops::Add<Direction> for Direction {
    type Output = Direction;
    fn add(self, rhs: Direction) -> Self::Output {
        let result: usize = (self.as_number::<usize>() + rhs.as_number::<usize>()) % 4;
        let result = Direction::from_number(result);
        return result;
    }
}

impl Direction {
    pub fn from_number<T>(n: T) -> Direction
    where
        T: Into<usize>,
    {
        let n = n.into();
        match n {
            0 => Direction::North,
            1 => Direction::East,
            2 => Direction::South,
            3 => Direction::West,
            _ => panic!("Invalid direction number: {}", n),
        }
    }
    pub fn to_position(&self) -> Pos {
        match self {
            Direction::East => Pos::new(1, 0),
            Direction::South => Pos::new(0, 1),
            Direction::West => Pos::new(-1, 0),
            Direction::North => Pos::new(0, -1),
        }
    }
}
