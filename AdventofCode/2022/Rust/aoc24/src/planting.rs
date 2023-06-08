use std::collections::HashMap;

use crate::pos::Position;

enum Direction {
    North,
    South,
    East,
    West,
}

impl Direction {
    fn is_east_west(&self) -> bool {
        match self {
            Direction::East => true,
            Direction::West => true,
            _ => false,
        }
    }
    fn is_north_south(&self) -> bool {
        match self {
            Direction::North => true,
            Direction::South => true,
            _ => false,
        }
    }
    fn direction_to_position(&self) -> Position {
        match self {
            Direction::North => Position::new(0, -1),
            Direction::South => Position::new(0, 1),
            Direction::East => Position::new(1, 0),
            Direction::West => Position::new(-1, 0),
        }
    }
}

const NORTH : Position = Position { x:  0 , y: -1 };
const SOUTH : Position = Position { x:  0 , y:  1 };
const EAST  : Position = Position { x:  1 , y:  0 };
const WEST  : Position = Position { x: -1 , y:  0 };

pub struct Planting {
    elves : HashMap< Position, Option<Position> >,
    directions : Vec<Direction>,
}

//implement Display for Planting, using min and max to determine the size of the grid
impl std::fmt::Display for Planting {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut result = String::from("\n");
        let (min,max) = self.calculate_min_max();
        for y in min.y..=max.y {
            for x in min.x..=max.x {
                let pos = Position::new(x,y);
                if self.elves.contains_key(&pos) {
                    result += "#";
                } else {
                    result += ".";
                }
            }
            result += "\n";
        }
        write!(f, "{}", result)
    }   
}


impl Planting {
    /*initialize the planting from a file of this format, where # indicates an elf:
..............
..............
.......#......
.....###.#....
...#...#.#....
....#...##....
...#.###......
...##.#.##....
....#..#......
..............
..............
..............


    */
    pub fn new( content : &str ) -> Planting {
        let mut elves = HashMap::new();
        let mut y = 0;
        for line in content.lines() {
            let mut x = 0;
            for c in line.chars() {
                if c == '#' {
                    elves.insert( Position::new(x,y), None );
                }
                x += 1;
            }
            y += 1;
        }
        let directions = vec![Direction::North,Direction::South,Direction::West,Direction::East];
        Planting { elves, directions }
    }

    // cycle directions, moving the first direction to the end of the vector
    fn cycle_directions(&mut self) {
        let first = self.directions.remove(0);
        self.directions.push(first);
    }


//calculate the min and max positions of the elves, without assuming they start at 0,0
    fn calculate_min_max( &self ) -> (Position,Position) {
        let x_values = self.elves.keys().map(|p| p.x);
        let y_values = self.elves.keys().map(|p| p.y);
        let min_x = x_values.clone().min().unwrap();
        let max_x = x_values.clone().max().unwrap();
        let min_y = y_values.clone().min().unwrap();
        let max_y = y_values.clone().max().unwrap();
        ( Position::new(min_x,min_y), Position::new(max_x,max_y) )    
    }

    // calculate the number of elves that are adjacent to a particular position, adjacent means abs diffence of 0 or 1 in x or y
    fn calculate_adjacent_elves( &self, pos : &Position ) -> usize {
        let mut count = 0;
        for y in -1..=1 {
            for x in -1..=1 {
                if x == 0 && y == 0 {
                    continue;
                }
                let adjacent_pos = Position::new(pos.x + x, pos.y + y);
                if self.elves.contains_key(&adjacent_pos) {
                    count += 1;
                }
            }
        }
//        println!("calculate_adjacent_elves({:?}), count {}", pos, count);
        count
    }



    // iterate over the elves and update the values in the hashmap
    fn calculate_moves(&mut self) {
        let keys : Vec<Position> = self.elves.keys().cloned().collect();
        for pos in &keys {
            let mut next_pos = None;
            // if no other elves are adjacent, then the elf stays in place, set the value to None
            if self.calculate_adjacent_elves(pos) != 0 {
//                println!("adjacent elves for {:?}", pos);
                // if there are adjacent elves, then the elf moves to the first adjacent cell in the direction of the first direction in the directions vector
                // for each direction, test the three adjacent positions in that direction.  If there is no elf in any of those three cells then the elf moves to the first cell in that direction.  If there is an elf in any of those three cells, then the elf stays in place.
                for direction in self.directions.iter() {
                    if direction.is_east_west() {
                        // check if there is an elf in position + direction, position + direction + north, position + direction + south
                        let pos1 = *pos + direction.direction_to_position();
                        let pos2 = pos1 + NORTH;
                        let pos3 = pos1 + SOUTH;
                        if !self.elves.contains_key(&pos1) && !self.elves.contains_key(&pos2) && !self.elves.contains_key(&pos3) {
                            next_pos = Some(pos1);
                            break;
                        }
                    } else if direction.is_north_south() {
                        // check if there is an elf in position + direction, position + direction + east, position + direction + west
                        let pos1 = *pos + direction.direction_to_position();
                        let pos2 = pos1 + EAST;
                        let pos3 = pos1 + WEST;
                        if !self.elves.contains_key(&pos1) && !self.elves.contains_key(&pos2) && !self.elves.contains_key(&pos3) {
                            next_pos = Some(pos1);
                            break;
                        }
                    }
                }
            }
            self.elves.insert(*pos, next_pos);
        }
       // if the next position is duplicated, then the elf stays in place
       // find duplicate values in the next positions
        let mut duplicate_values: HashMap<Position, Vec<Position>> = HashMap::new();
        for (pos, next_pos) in self.elves.iter() {
            if let Some(next_pos) = next_pos {
                if duplicate_values.contains_key(next_pos) {
                    duplicate_values.get_mut(next_pos).unwrap().push(*pos);
                } else {
                    duplicate_values.insert(*next_pos, vec![*pos]);
                }
            }
        }
        // if there are duplicate values, then set the next position to None
        for (next_pos, positions) in duplicate_values.iter() {
            if positions.len() > 1 {
                for pos in positions {
                    self.elves.insert(*pos, None);
                }
            }
        }

    }

    // return false if no elf moves
    fn make_moves(&mut self) -> bool {
        let mut response = false;
        self.calculate_moves();
        let keys : Vec<Position> = self.elves.keys().cloned().collect();
        for pos in &keys {
            if let Some(next_pos) = self.elves.get(pos).unwrap() {
                response = true;
                self.elves.insert(*next_pos, None);
                self.elves.remove(pos);
            }
        }
        response
    }

    pub fn run(&mut self, steps : usize) -> usize {
        let mut no_steps = 0;
        for i in 0..steps {
            if !self.make_moves() {
                no_steps = i;
                break;
            };
            self.cycle_directions();
        }
        no_steps +1
    }

    pub fn get_empty_positions_count(&self) -> usize {
        let (min_pos, max_pos) = self.calculate_min_max();
        let mut count = 0;
        for y in min_pos.y..=max_pos.y {
            for x in min_pos.x..=max_pos.x {
                let pos = Position::new(x,y);
                if !self.elves.contains_key(&pos) {
                    count += 1;
                }
            }
        }
        count
    }
}



// a test module and function
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_planting() {
        let content = "..............
        ..............
        .......#......
        .....###.#....
        ...#...#.#....
        ....#...##....
        ...#.###......
        ...##.#.##....
        ....#..#......
        ..............
        ..............
        ..............
        ";
        let mut planting = Planting::new(content);
        println!("elves: {:}", planting);
        assert_eq!( planting.elves.len(), 22 );
        planting.run(10);
        println!("elves: {:}", planting);
        println!("empty positions: {}", planting.get_empty_positions_count());
    }


    #[test]
    fn test_calculate_adjacent_elves() {
        let content = "..............
        ..............
        .......#......
        .....###.#....
        ...#...#.#....
        ....#...##....
        ...#.###......
        ...##.#.##....
        ....#..#......
        ..............
        ..............
        ..............
        ";
        let mut planting = Planting::new(content);
        println!("no steps: {}", planting.run(10000));

    }


}