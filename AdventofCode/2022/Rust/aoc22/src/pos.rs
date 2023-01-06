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
impl std::ops::Sub<Position> for Position {
    type Output = Position;
    fn sub(self, other: Position) -> Position {
        Position {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}
// have constants for the four directions as Position objects
pub const RIGHT : Position = Position { x:  1 , y:  0 };
pub const DOWN  : Position = Position { x:  0 , y:  1 };
pub const LEFT  : Position = Position { x: -1 , y:  0 };
pub const UP    : Position = Position { x:  0 , y: -1 };
