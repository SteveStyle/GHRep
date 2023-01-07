
//extern crate num_traits;

//use num_traits::Num;
use std::str::FromStr;
use std::fmt::Debug;

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct Position {
    pub x: isize,
    pub y: isize,
}
// implement add and subtract for position
impl std::ops::Add<Position> for Position {
    type Output = Position;
    fn add(self, other: Position) -> Position {
        Position {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}
impl std::ops::Add<Direction> for Position {
    type Output = Position;
    fn add(self, other: Direction) -> Position {
        self.add(other.to_position())
    }
}
impl std::ops::Sub<Position> for Position {
    type Output = Position;
    fn sub(self, other: Position) -> Position {
        Position {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}
impl std::ops::Mul<isize> for Position {
    type Output = Position;
    fn mul(self, other: isize) -> Position {
        Position {
            x: self.x * other,
            y: self.y * other,
        }
    }
}

// implement the new function for Position
impl Position {
    pub fn new(x: isize, y: isize) -> Position {
        Position { x, y }
    }

    pub fn is_adjacent(&self, other: &Position) -> bool {
        let diff = *self - *other;
        diff.x.abs() <= 1 && diff.y.abs() <= 1
    }
    pub fn is_orthogonal(&self, other: &Position) -> bool {
        let diff = *self - *other;
        diff.x.abs() <= 1 && diff.y.abs() <= 1 && diff.x.abs() != diff.y.abs()
    }
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum Direction {
    Right,
    Down,
    Left,
    Up,
    Wait,
}

impl std::ops::Add<Direction> for Direction {
    type Output = Direction;
    fn add(self, rhs: Direction) -> Self::Output {
        if self == Direction::Wait {
            return rhs;
        }
        if rhs == Direction::Wait {
            return self;
        }
        let result : usize = (self.as_number::<usize>() + rhs.as_number::<usize>()) % 4;
        let result = Direction::from_number(result);
        return result;
    }
}

impl Direction {
    pub fn from_number<T>(n: T) -> Direction 
    where
        T: Into<usize> 
    {
        let n = n.into();
        match n {
            0 => Direction::Right,
            1 => Direction::Down,
            2 => Direction::Left,
            3 => Direction::Up,
            _ => Direction::Wait,
        }
    }
    pub fn to_position(&self) -> Position {
        match self {
            Direction::Right => Position::new(1, 0),
            Direction::Down => Position::new(0, 1),
            Direction::Left => Position::new(-1, 0),
            Direction::Up => Position::new(0, -1),
            Direction::Wait => Position::new(0, 0),
        }
    }
    fn as_str(&self) -> &str {
        match self {
            Direction::Right => "Right",
            Direction::Down => "Down",
            Direction::Left => "Left",
            Direction::Up => "Up",
            Direction::Wait => "Wait",
        }
    }
    fn as_number<T>(&self) -> T
    where
        T: std::str::FromStr, <T as FromStr>::Err: Debug
    {
        match self {
            Direction::Right => "0".parse().unwrap(),
            Direction::Down => "1".parse().unwrap(),
            Direction::Left => "2".parse().unwrap(),
            Direction::Up => "3".parse().unwrap(),
            Direction::Wait => "0".parse().unwrap(),
        }
    }
    pub fn from_char(c: char) -> Option<Direction> {
        match c {
            '>' => Some(Direction::Right),
            'v' => Some(Direction::Down),
            '<' => Some(Direction::Left),
            '^' => Some(Direction::Up),
            _ => None,
        }
    }
    pub fn is_horizontal(&self) -> bool {
        match self {
            Direction::Right => true,
            Direction::Left => true,
            _ => false,
        }
    }


    
}
// have constants for the four directions as Position objects
pub const RIGHT : Position = Position { x:  1 , y:  0 };
pub const DOWN  : Position = Position { x:  0 , y:  1 };
pub const LEFT  : Position = Position { x: -1 , y:  0 };
pub const UP    : Position = Position { x:  0 , y: -1 };
pub const WAIT  : Position = Position { x:  0 , y:  0 };

pub const DIRECTIONS : [Position; 5] = [RIGHT, DOWN, LEFT, UP, WAIT];
