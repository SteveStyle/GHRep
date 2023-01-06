
use std::{collections::HashMap, ops::{Add, Sub}};

use Instruction::*;
use Direction::*;

use crate::pos::*;


#[derive(Debug, PartialEq)]
enum Instruction {
    Move(usize),
    TurnLeft,
    TurnRight,
}

#[derive(Eq, Debug, Hash, Clone, Copy, PartialEq)]
enum Direction {
    Right = 0,
    Down  = 1,
    Left  = 2,
    Up    = 3,
}

const DIRECTIONS: [Direction; 4] = [Right, Down, Left, Up];


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
    // a function which returns the value of Direction as an integer
    fn get_value(&self) -> isize {
        match self {
            Right => 0,
            Down  => 1,
            Left  => 2,
            Up    => 3,
        }
    }

}

impl Add for Direction {
    type Output = Direction;
    fn add(self, other: Direction) -> Direction {
        let mut result = self.get_value() + other.get_value();
        if result > 3 {
            result -= 4;
        }
        match result {
            0 => Right,
            1 => Down,
            2 => Left,
            3 => Up,
            _ => panic!("Invalid direction value"),
        }
    }
}

impl Sub for Direction {
    type Output = Direction;
    fn sub(self, other: Direction) -> Direction {
        let mut result = self.get_value() - other.get_value();
        if result < 0 {
            result += 4;
        }
        match result {
            0 => Right,
            1 => Down,
            2 => Left,
            3 => Up,
            _ => panic!("Invalid direction value"),
        }
    }
}

// implement negate for Direction
impl std::ops::Neg for Direction {
    type Output = Direction;
    fn neg(self) -> Direction {
        match self {
            Right => Left,
            Down  => Up,
            Left  => Right,
            Up    => Down,
        }
    }
}


#[derive(Debug, PartialEq, Clone, Copy)]
enum Face {
    Top,
    Bottom,
    Left,
    Right,
    Front,
    Back,
}

type DirectionLimit = HashMap<Direction, Vec<isize>>;

#[derive(Debug, PartialEq)]
pub struct Map {
    pub map: Vec<Vec<char>>,
    pub width: usize,
    pub height: usize,
    block_size: usize,
    instructions: Vec<Instruction>,
    position: Position,
    direction: Direction,
    // add lookup vectors for the first and last non-space in each row and column, indexed by direction and column number or row number
    first_non_space: DirectionLimit,
    last_non_space: DirectionLimit,
    block_array: Vec<Vec<Option<(Face,Direction)>>>,
    glue: HashMap<(Position, Direction), ( Position, Direction )>,
}

//implement display for Map
impl std::fmt::Display for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut result = String::new();
        result += &self.format_map();
        result += format!("width: {}, height: {}, position: {:?}, direction: {:?}\ninstructions: {:?}\nfirst non space {:?}\nlast non space {:?}\n\nblock array {:?}\nglue {:?}", 
                        self.width, self.height, self.position, self.direction, self.instructions, self.first_non_space, self.last_non_space, self.block_array, self.glue).as_str();
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
        let mut map = lines.iter().map(|line| line.chars().collect::<Vec<_>>()).collect::<Vec<_>>();
    
        // Determine the width and height of the map
        let width = map.iter().map(|l| l.len()).max().unwrap();
        let height = map.len();

        // extend each row to the width of the map using spaces
        for row in map.iter_mut() {
            for _ in row.len()..width {
                row.push(' ');
            }
        }

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
            first_non_space: DirectionLimit::new(),
            last_non_space: DirectionLimit::new(),
            block_size: 0,
            block_array: Vec::new(),
            glue: HashMap::new(),
        };

        map.calculate_first_last_non_space();

        //set the position to the first . in the first row of the map, facing right
        map.position = Position{ x: map.first_non_space[&Right][0], y: 0 };
    
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


        //println!("{}", map);
        return map;
    }

    fn calculate_first_last_non_space(&mut self) {
        //calculate the first and last non-space in each row and column, indexed by direction and column number or row number

        for direction in [Right, Left] {
            self.first_non_space.insert(direction, vec![-1;self.height]);
            self.last_non_space.insert(direction, vec![-1;self.height]);
        }
    
        for direction in [Up, Down] {
            self.first_non_space.insert(direction, vec![-1;self.width]);
            self.last_non_space.insert(direction, vec![-1;self.width]);
        }
    
        // For each direction, calculate the first and last non-space for each row or column
       for y in 0..self.height {
            for x in 0..self.width {
                let c = self.map[y][x];
                if c != ' ' {
                    if self.first_non_space[&Right][y] == -1 {
                        self.first_non_space.get_mut(&Right).unwrap()[y] = x as isize;
                        self.last_non_space.get_mut(&Left).unwrap()[y] = x as isize;
                    }
                    self.last_non_space.get_mut(&Right).unwrap()[y] = x as isize;
                    self.first_non_space.get_mut(&Left).unwrap()[y] = x as isize;

                    if self.first_non_space[&Down][x] == -1 {
                        self.first_non_space.get_mut(&Down).unwrap()[x] = y as isize;
                        self.last_non_space.get_mut(&Up).unwrap()[x] = y as isize;
                    }
                    self.last_non_space.get_mut(&Down).unwrap()[x] = y as isize;
                    self.first_non_space.get_mut(&Up).unwrap()[x] = y as isize;
                }
            }
       }
        
    }
    
    fn format_map(&self) -> String {
        let mut result = String::new();
        for line in &self.map {
            result += &line.iter().collect::<String>();
            result += "\n";
        }
        return result;
    }

    // apply the instructions to the map
    pub fn apply_instructions(&mut self) {
        //println!("{:?} \t{:?}", self.position, self.direction);
        for i in 0..self.instructions.len() {
            match self.instructions[i] {
                Move(distance) => {
                    for _ in 0..distance {
                        if !self.move_forward() {
                            break;
                        };
                    }
                },
                TurnLeft => {
                    self.turn_left();
                },
                TurnRight => {
                    self.turn_right();
                },
            }
//            println!("{:?} \t\t{:?} \t{:?}", self.instructions[i], self.position, self.direction);
        }
    }

    // the position in the direction we are moving, either x or y
    fn get_idx_pos(&self) -> ( isize, isize ) {
        if self.direction == Right || self.direction == Left {
            return (self.position.y, self.position.x);
        } 
        //else if self.direction == Up || self.direction == Down {
        return (self.position.x, self.position.y);        
    }

    fn at_last(&self) -> bool {
        let (idx, pos) = self.get_idx_pos();
        let last = self.last_non_space[&self.direction][idx as usize];
        return pos == last;
    }

    fn next_pos(&self) -> ( Position, Direction ) {
        let (idx, _) = self.get_idx_pos();
        if let Some(glue) = self.glue.get(&(self.position, self.direction)) {
//            println!("Glue at {:?} {:?}, gives {:?}", self.position, self.direction, glue);
            return glue.clone();
        } else if self.at_last() {
            let mut result = self.position.clone();
            if self.direction == Left || self.direction == Right {
                result.x = self.first_non_space[&self.direction][idx as usize];
            } else {
                result.y = self.first_non_space[&self.direction][idx as usize];
            }
//            println!("At last, {:?} {:?} -> {:?}", self.position, self.direction, result);
            return ( result, self.direction );
        } else {
            return ( self.position + self.direction.get_step(), self.direction );
        }
    }

    fn get(&self, pos: Position) -> char {
        return self.map[pos.y as usize][pos.x as usize];
    }

    // move forward one step in the current direction, as long as the next step is a '.'.  If it is a '#', stop.  If it is a ' ' or the end of the map, wrap around to the first . in that direction.
    fn move_forward(&mut self) -> bool {
        let ( next_pos, next_direction ) = self.next_pos();
        let c = self.get(next_pos);
        if c == '.' {
            self.position = next_pos;
            self.direction = next_direction;
            return true;
        } else {
            return false;
        }
    }

    fn turn_left(&mut self) {
        self.direction = self.direction.turn_left();
    }

    fn turn_right(&mut self) {
        self.direction = self.direction.turn_right();
    }

    pub fn get_password(&self) -> isize {
        return 1000 * (self.position.y +1) + 4 * (self.position.x +1) + self.direction.get_value();
    }

    fn calculate_block_size(&mut self)  {
        if self.height > self.width {
            self.block_size = self.width / 3;
            assert_eq!(self.block_size * 4, self.height);
        } else {
            self.block_size = self.height / 3;
            assert_eq!(self.block_size * 4, self.width);
        }
    }

     fn calculate_block_array(&mut self)  {
        self.calculate_block_size();
        let block_width = self.width / self.block_size;
        let block_height = self.height / self.block_size;
        self.block_array = vec![vec![None; block_width]; block_height];
        let mut block_boolean = vec![];

        for y in 0..self.height/self.block_size {
            let mut row = Vec::new();
            for x in 0..self.width/self.block_size {
                row.push( self.map[y*self.block_size][x*self.block_size] != ' ' );
            }
            block_boolean.push(row);
        }

//        println!("block_boolean {:#?}\n block_array {:?}", block_boolean, self.block_array);

        // set pos to the first block which is true in the block_boolean array
        for y in 0..self.height/self.block_size {
            for x in 0..self.width/self.block_size {
                if block_boolean[y][x] {
                    let pos = Position { x: x as isize, y: y as isize };
                    self.set_block(&block_boolean, pos, Face::Top, Right);
                    return;
                }
            }
        }
      
    }

    fn in_bounds_of_block_array(&self, pos: Position) -> bool {
        return pos.x >= 0 && pos.x < self.width as isize / self.block_size as isize && pos.y >= 0 && pos.y < self.height as isize / self.block_size as isize;
    }

    fn set_block(&mut self, block_boolean: &Vec<Vec<bool>>, pos: Position, face: Face, orientation: Direction) {
        self.block_array[pos.y as usize][pos.x as usize] = Some((face, orientation));
        for planer_direction in DIRECTIONS {
            let next_block_pos = pos + planer_direction.get_step();
            if self.in_bounds_of_block_array(next_block_pos) && block_boolean[next_block_pos.y as usize][next_block_pos.x as usize] && self.block_array[next_block_pos.y as usize][next_block_pos.x as usize].is_none() {
                let cubic_direction = planer_direction - orientation;
                match face {
                    Face::Top => match cubic_direction {
                        Direction::Up => self.set_block(block_boolean, next_block_pos, Face::Back, orientation + Right),
                        Direction::Down => self.set_block(block_boolean, next_block_pos, Face::Front, orientation + Right),
                        Direction::Left => self.set_block(block_boolean, next_block_pos, Face::Left, orientation + Right),
                        Direction::Right => self.set_block(block_boolean, next_block_pos, Face::Right, orientation + Right),
                    }
                    Face::Bottom => match cubic_direction {
                        Direction::Up => self.set_block(block_boolean, next_block_pos, Face::Back, orientation + Left),
                        Direction::Down =>self.set_block(block_boolean, next_block_pos, Face::Front, orientation + Left),
                        Direction::Left => self.set_block(block_boolean, next_block_pos, Face::Right, orientation + Right),
                        Direction::Right => self.set_block(block_boolean, next_block_pos, Face::Left, orientation + Right),
                    }
                    Face::Front => match cubic_direction {
                        Direction::Up => self.set_block(block_boolean, next_block_pos, Face::Top, orientation + Right),
                        Direction::Down => self.set_block(block_boolean, next_block_pos, Face::Bottom, orientation + Left),
                        Direction::Left => self.set_block(block_boolean, next_block_pos, Face::Left, orientation + Up),
                        Direction::Right => self.set_block(block_boolean, next_block_pos, Face::Right, orientation + Down),
                    }
                    Face::Back => match cubic_direction {
                        Direction::Up => self.set_block(block_boolean, next_block_pos, Face::Bottom, orientation + Left),
                        Direction::Down => self.set_block(block_boolean, next_block_pos, Face::Top, orientation + Right),
                        Direction::Left => self.set_block(block_boolean, next_block_pos, Face::Left, orientation + Down),
                        Direction::Right => self.set_block(block_boolean, next_block_pos, Face::Right, orientation + Up),
                    }
                    Face::Left => match cubic_direction {
                        Direction::Up => self.set_block(block_boolean, next_block_pos, Face::Back, orientation + Up),
                        Direction::Down => self.set_block(block_boolean, next_block_pos, Face::Front, orientation + Down),
                        Direction::Left => self.set_block(block_boolean, next_block_pos, Face::Bottom, orientation + Right),
                        Direction::Right => self.set_block(block_boolean, next_block_pos, Face::Top, orientation + Right),
                    }
                    Face::Right => match cubic_direction {
                        Direction::Up => self.set_block(block_boolean, next_block_pos, Face::Back, orientation + Down),
                        Direction::Down => self.set_block(block_boolean, next_block_pos, Face::Front, orientation + Up),
                        Direction::Left => self.set_block(block_boolean, next_block_pos, Face::Top, orientation + Right),
                        Direction::Right => self.set_block(block_boolean, next_block_pos, Face::Bottom, orientation + Right),
                    }
                }
            }
        }
    }

    fn get_face_position(&self, face: Face) -> (Position, Direction) {
        for y in 0..self.height / self.block_size {
            for x in 0..self.width / self.block_size {
                if let Some((f, orientation )) = self.block_array[y][x] {
                    if f == face {
                        return ( Position { x: (self.block_size * x) as isize, y: (self.block_size * y) as isize }, orientation );
                    }
                }
            }
        }
        panic!("No face found");
    }

    // returns the edge of the face in the given direction, read clockwise
    fn get_edge( &self, pos: Position, planer_direction: Direction ) -> Vec<Position> {
        let mut edge = Vec::new();
        let block_size = self.block_size as isize;
        for i in 0..self.block_size {
            let i = i as isize;
            match planer_direction {
                Direction::Up => edge.push( Position { x: pos.x + i, y: pos.y } ),
                Direction::Down => edge.push( Position { x: pos.x + i, y: pos.y + block_size - 1 } ),
                Direction::Left => edge.push( Position { x: pos.x, y: pos.y + i } ),
                Direction::Right => edge.push( Position { x: pos.x + block_size - 1, y: pos.y + i } ),
            }
        }
        if planer_direction == Direction::Down || planer_direction == Direction::Left {
            edge.reverse();
        }
        return edge;
    }

    fn glue( &mut self, face1 : Face, cubic_direction1 : Direction, face2 : Face, cubic_direction2 : Direction ) {
        let (pos1, orientation1) = self.get_face_position(face1);
        let (pos2, orientation2) = self.get_face_position(face2);
        let planer_direction1 = cubic_direction1 + orientation1;
        let planer_direction2 = cubic_direction2 + orientation2;
        let edge1 = self.get_edge(pos1, planer_direction1);
        let mut edge2 = self.get_edge(pos2, planer_direction2);
        edge2.reverse();
        for i in 0..self.block_size {
            self.glue.insert((edge1[i], planer_direction1), (edge2[i], -planer_direction2));
            self.glue.insert((edge2[i], planer_direction2), (edge1[i], -planer_direction1));
        }
    }
    pub fn glue_cube(&mut self) {
        self.calculate_block_array();

        self.glue(Face::Top, Direction::Right, Face::Right, Direction::Left);
        self.glue(Face::Top, Direction::Down, Face::Front, Direction::Up);
        self.glue(Face::Top, Direction::Left, Face::Left, Direction::Right);
        self.glue(Face::Top, Direction::Up, Face::Back, Direction::Down);

        self.glue(Face::Bottom, Direction::Right, Face::Left, Direction::Left);
        self.glue(Face::Bottom, Direction::Down, Face::Back, Direction::Down);
        self.glue(Face::Bottom, Direction::Left, Face::Right, Direction::Right);
        self.glue(Face::Bottom, Direction::Up, Face::Front, Direction::Up);

        self.glue(Face::Front, Direction::Right, Face::Right, Direction::Down);
        self.glue(Face::Front, Direction::Down, Face::Bottom, Direction::Down);
        self.glue(Face::Front, Direction::Left, Face::Left, Direction::Down);
        self.glue(Face::Front, Direction::Up, Face::Top, Direction::Down);

        self.glue(Face::Back, Direction::Right, Face::Left, Direction::Up);
        self.glue(Face::Back, Direction::Down, Face::Top, Direction::Up);
        self.glue(Face::Back, Direction::Left, Face::Right, Direction::Up);
        self.glue(Face::Back, Direction::Up, Face::Bottom, Direction::Up);

        self.glue(Face::Left, Direction::Right, Face::Top, Direction::Left);
        self.glue(Face::Left, Direction::Down, Face::Front, Direction::Left);
        self.glue(Face::Left, Direction::Left, Face::Bottom, Direction::Right);
        self.glue(Face::Left, Direction::Up, Face::Back, Direction::Left);

        self.glue(Face::Right, Direction::Right, Face::Bottom, Direction::Left);
        self.glue(Face::Right, Direction::Down, Face::Front, Direction::Right);
        self.glue(Face::Right, Direction::Left, Face::Top, Direction::Right);
        self.glue(Face::Right, Direction::Up, Face::Back, Direction::Right);
        
    }

}


// a test module and a test function to test the new function
#[cfg(test)]
mod tests {
    use super::*;
    

    // a test function for calculate_block_array
    #[test]
    fn 
    test_calculate_block_array() {
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
        let mut map = Map::new(content);
        map.calculate_block_array();
        println!(" test function: block_array: {:?}", map.block_array);
    }

    #[test]
    fn test_2() {
        let content = 
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

10R5L5R10L4R5L5";
        let mut map = Map::new(content);

        map.glue_cube();

        map.apply_instructions();

        println!("{}", map);

        assert_eq!(map.get_password(), 5031);
    }
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
        let mut map = Map::new(content);
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

        println!("{}", map.format_map());

        map.apply_instructions();
        println!("{}", map.get_password());

        map.calculate_block_size();
        println!("{}", map.block_size);
    }
}