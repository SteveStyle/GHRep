
use std::collections::HashMap;

use Instruction::*;
use Direction::*;

#[derive(Debug, PartialEq)]
enum Instruction {
    Move(usize),
    TurnLeft,
    TurnRight,
}

#[derive(Eq, Debug, Hash, PartialEq)]
enum Direction {
    Right = 0,
    Down  = 1,
    Left  = 2,
    Up    = 3,
}

impl Direction {
    fn turn_left(&self) -> Direction {
        match self {
            Right => Up,
            Down  => Right,
            Left  => Down,
            Up    => Left,
        }
    }
    fn turn_right(&self) -> Direction {
        match self {
            Right => Down,
            Down  => Left,
            Left  => Up,
            Up    => Right,
        }
    }
    fn get_step(&self) -> Position {
        match self {
            Right => RIGHT,
            Down  => DOWN,
            Left  => LEFT,
            Up    => UP,
        }
    }
}
#[derive(Debug, PartialEq)]
struct Position {
    x: isize,
    y: isize,
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
const RIGHT : Position = Position { x:  1 , y:  0 };
const DOWN  : Position = Position { x:  0 , y:  1 };
const LEFT  : Position = Position { x: -1 , y:  0 };
const UP    : Position = Position { x:  0 , y: -1 };

const DIRECTIONS: [Position; 4] = [RIGHT, DOWN, LEFT, UP];

type DirectionLimit = HashMap<Direction, Vec<isize>>;

#[derive(Debug, PartialEq)]
pub struct Map {
    pub map: Vec<Vec<char>>,
    pub width: usize,
    pub height: usize,
    instructions: Vec<Instruction>,
    position: Position,
    direction: Direction,
    // add lookup vectors for the first and last non-space in each row and column, indexed by direction and column number or row number
    first_non_space: DirectionLimit,
    last_non_space: DirectionLimit,
}

//implement display for Map
impl std::fmt::Display for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let result = String::new();
        result += &self.format_map();
        result += format!("width: {}, height: {}, position: {:?}, direction: {:?}\ninstructions: {:?}\nfirst non space {:?}\nlast non space {:?}", 
                        self.width, self.height, self.position, self.direction, self.instructions, self.first_non_space, self.last_non_space).as_str();
        write!(f, "{}", result)
    }
}
        

impl Map {
    /* new function which takes a string in the form below.  The first secion is the map, the second section is the instructions.
"        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"
    */

    pub fn new(content: &str) -> Map {
        let mut sections = content.split("\n\n");
        let map_text = sections.next().unwrap();
        let instructions_text = sections.next().unwrap();

        // Split the input string into lines
        let lines = map_text.lines().collect::<Vec<_>>();
    
        // Convert each line into a Vec of chars
        let map = lines.iter().map(|line| line.chars().collect::<Vec<_>>()).collect::<Vec<_>>();
    
        // Determine the width and height of the map
        let width = map.iter().map(|l| l.len()).max().unwrap();
        let height = map.len();

        // extend each row to the width of the map using spaces
        for row in map.iter() {
            for _ in row.len()..width {
                row.push(' ');
            }
        }

        //calculate the first and last non-space in each row and column, indexed by direction and column number or row number
        let mut first_non_space = DirectionLimit::new();
        let mut last_non_space = DirectionLimit::new();

        // Initialize a new Map object
        let x = map[0].iter().position(|&c| c == '.').unwrap() as isize;
        let mut map = Map {
            map,
            width,
            height,
            instructions: Vec::new(),
            position: Position {
                x,
                y: 0,
            },
            direction: Right,
            first_non_space,
            last_non_space,
        };

        map.calculate_first_last_non_space();
    
            // Extract the instructions from the input string
        let mut distance = 0;
        for c in instructions_text.chars() {
            if c.is_digit(10) {
                distance = distance * 10 + c.to_digit(10).unwrap() as usize;
            } else if c == 'L' || c == 'R' {
                if distance > 0 {
                    map.instructions.push(Move(distance));
                    distance = 0;
                }
                if c == 'L' {
                    map.instructions.push(TurnLeft);
                } else if c == 'R' {
                    map.instructions.push(TurnRight);
                }
            }
        }
        if distance > 0 {
            map.instructions.push(Move(distance));
        }

        // set position to the first . in the first row of the map, facing right


        println!("{}", map);
        return map;
    }

    fn calculate_first_last_non_space(&mut self) {
        //calculate the first and last non-space in each row and column, indexed by direction and column number or row number
        let &first_non_space = &self.first_non_space;
        let &last_non_space  = &self.last_non_space;

        for direction in [Right, Left] {
            first_non_space.insert(direction, vec![-1;self.width]);
            last_non_space.insert(direction, vec![-1;self.width]);
        }
    
        for direction in [Up, Down] {
            first_non_space.insert(direction, vec![-1;self.height]);
            last_non_space.insert(direction, vec![-1;self.height]);
        }
    
        // For each direction, calculate the first and last non-space for each row or column
       for y in 0..self.height {
            for x in 0..self.width {
                let c = self.map[y][x];
                if c != ' ' {
                    if first_non_space[&Right][y] == -1 {
                        first_non_space[&Right][y] = x as isize;
                        last_non_space[&Left][y] = x as isize;
                    }
                    last_non_space[&Right][y] = x as isize;

                    if first_non_space[&Down][x] == -1 {
                        first_non_space[&Down][x] = y as isize;
                        last_non_space[&Up][x] = y as isize;
                    }
                    last_non_space[&Down][x] = y as isize;
                    first_non_space[&Up][x] = y as isize;
                }
            }
       }
        
    }
    
    fn format_map(&self) -> String {
        let mut result = String::new();
        for line in &self.map {
            result += &line.iter().collect::<String>();
        }
        return result;
    }

    // apply the instructions to the map
    pub fn apply_instructions(&mut self) {
        for instruction in &self.instructions {
            match instruction {
                Move(distance) => {
                    for _ in 0..*distance {
                        self.move_forward();
                    }
                },
                TurnLeft => {
                    self.turn_left();
                },
                TurnRight => {
                    self.turn_right();
                },
            }
        }
    }

    // the position in the direction we are moving, either x or y
    fn get_idx_pos(&self) -> ( isize, isize ) {
        if direction in [Right, Left] {
            return (self.position.y, self.position.x);
        } else if direction in [Up, Down] {
            return (self.position.x, self.position.y);
        }
    }

    fn at_last(&self) -> bool {
        let (idx, pos) = self.get_idx_pos();
        let last = self.last_non_space[&self.direction][idx];
        return pos == last;
    }

    fn next_pos(&self) -> Position {
        let (idx, pos) = self.get_idx_pos();
        if self.at_last() {
            let mut result = self.position.clone();
            result.x = self.first_non_space[&self.direction][idx];
            return result;
        } else {
            return self.position + self.direction.get_step();
        }
    }

    // move forward one step in the current direction, as long as the next step is a '.'.  If it is a '#', stop.  If it is a ' ' or the end of the map, wrap around to the first . in that direction.
    fn move_forward(&mut self) {
        let (next_idx, next_pos) = self.next_pos();
        if self.map[next_idx][next_pos] == '.' {
            self.position.x = next_pos;
            self.position.y = next_idx;
        } else if self.map[next_idx][next_pos] == '#' {
            return;
        } else if self.at_last() {
            let first = self.first_non_space[&self.direction][next_idx];
            self.position.x = first;
            self.position.y = next_idx;
        }
    }





}


// a test module and a test function to test the new function
#[cfg(test)]
mod tests {
    use super::*;
    use crate::path::Instruction::*;

    #[test]
    fn test_new() {
        let content = "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5";
        let map = Map::new(content);
        assert_eq!(map.width, 16);
        assert_eq!(map.height, 12);
        assert_eq!(map.map[0][0], ' ');
        assert_eq!(map.map[11][15], '.');
        assert_eq!(map.instructions.len(), 13);
        assert_eq!(map.instructions[0], Move(10));
        assert_eq!(map.instructions[1], TurnRight);
        assert_eq!(map.instructions[2], Move(5));
        assert_eq!(map.instructions[3], TurnLeft);
        assert_eq!(map.instructions[4], Move(5));
        assert_eq!(map.instructions[5], TurnRight);
        assert_eq!(map.instructions[6], Move(10));
        assert_eq!(map.instructions[7], TurnLeft);
        assert_eq!(map.instructions[8], Move(4));
        assert_eq!(map.instructions[9], TurnRight);
        assert_eq!(map.instructions[10], Move(5));
        assert_eq!(map.instructions[11], TurnLeft);
        assert_eq!(map.instructions[12], Move(5));
    }
}