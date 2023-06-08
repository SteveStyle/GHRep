use std::collections::HashSet;

use crate::pos::*;

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
struct Wind {
    direction: Direction,
    starting_position: Position,
}

impl Wind {
    fn new(direction: Direction, starting_position: Position) -> Wind {
        Wind {
            direction,
            starting_position,
        }
    }
    fn position_after_time(&self, time: isize, max_height : isize, max_width : isize ) -> Position {
        let mut result = self.starting_position + self.direction.to_position() * time;
        if self.direction.is_horizontal() {
            let x = (result.x -1) % max_width;
            if x < 0 {
                result.x = (x + max_width) % max_width +1;
            } else {
                result.x = x % max_width +1;
            }
        }
        else {
            let y = (result.y -1) % max_height;
            if y < 0 {
                result.y = (y + max_height) % max_height +1;
            } else {
                result.y = y % max_height +1;
            }
        }
        result
    }

}

#[derive(Debug, Clone)]
pub struct Valley {
    winds: Vec<Wind>,
    min : Position, // the minimum position of the valley, not including the entrance and exit
    max : Position, // the maximum position of the valley, not including the entrance and exit
    entrance: Position,
    exit: Position,
}

impl Valley {
    pub fn new(content : &str) -> Valley {
        let mut winds = Vec::new();
        let lines = content.lines().collect::<Vec<&str>>();
        let height = lines.len();
        let width = lines[0].len();

        let min = Position::new(1, 1);
        let max = Position::new(width as isize -2, height as isize -2);

        let mut entrance = Position::new(0, 0);
        for (idx, c) in lines[0].trim().chars().enumerate() {
            if c == '.' {
                entrance = Position::new(idx as isize, 0);
                break;
            }
        }

        let mut exit = Position::new(0, 0);
        for (idx, c) in lines[height-1].trim().chars().enumerate() {
            if c == '.' {
                exit = Position::new(idx as isize, height as isize -1);
                break;
            }
        }

        for y in 1..height-1 {
            let line = lines[y].trim().chars().collect::<Vec<char>>();
            for x in 1..width-1 {
                if let Some(direction) = Direction::from_char(line[x]) {
                    winds.push(Wind::new(direction, Position::new( x as isize, y as isize)));
                }
            }
        }

        
        println!("Valley: min: {:?}, max: {:?}, entrance: {:?}, exit: {:?}", min, max, entrance, exit);

        Valley {
            winds,
            min,
            max,
            entrance,
            exit,
        }

    }

    fn is_valid_position(&self, position : Position) -> bool {
        (position.x >= self.min.x && position.x <= self.max.x && position.y >= self.min.y && position.y <= self.max.y) 
        || position == self.entrance || position == self.exit
    }

    fn get_wind_positions(&self, time : isize) -> HashSet<Position> {
        let mut result = HashSet::new();
        for wind in &self.winds {
            let wind_position = wind.position_after_time(time, self.max.y, self.max.x);
            result.insert(wind_position);
        }
        result
    }
    fn get_valid_reachable_positions(&self, position: Position ) -> HashSet<Position> {
        let mut result = HashSet::new();
        for direction in DIRECTIONS {
            let new_position = position + direction;
            if self.is_valid_position(new_position) {
                result.insert(new_position);
            }
        }
        result
    }
    fn get_reachable_position(&self, position : Position, time : isize, previous_reachable_positions : HashSet<Position>) -> HashSet<Position> {
        let mut result = HashSet::new();
        for p in previous_reachable_positions {
            let valid_reachable_positions = self.get_valid_reachable_positions(p);
            for vrp in valid_reachable_positions {
                result.insert(vrp);
            }
        }
        let mut wind_positions = self.get_wind_positions(time);
        let result = result.difference(&wind_positions).cloned().collect();
//        self.show_valley(&wind_positions, &result);
        
        result
    }
    fn is_rock(&self, position : Position) -> bool {
        if position == self.entrance || position == self.exit {
            return false;
        }
        if position.x == 0 || position.x == self.max.x+1 || position.y == 0 || position.y == self.max.y+1 {
            return true;
        }
        false
    }
    fn show_valley(&self, wind_positions : &HashSet<Position>, reachable_positions : &HashSet<Position>) {
        for y in 0..self.max.y+2 {
            for x in 0..self.max.x+2 {
                let pos = Position::new(x, y);
                if self.is_rock(pos) {
                    print!("#");
                } else if reachable_positions.contains(&pos) && wind_positions.contains(&pos) {
                    print!("X");
                } else if reachable_positions.contains(&pos) {
                    print!("R");
                } else if wind_positions.contains(&pos) {
                    print!("W");
                } else {
                    print!(".");
                }
            }
            println!("");
        }
        println!("");
    }
    pub fn solve(&self, part : isize) -> isize {
        let mut time = 0;
        let mut leg = 1;
        let mut reachable_positions = HashSet::from([self.entrance]);
        loop {
            time += 1;
            reachable_positions = self.get_reachable_position(self.entrance, time, reachable_positions);
            match leg {
                1 => { if reachable_positions.contains(&self.exit) {
                        if part == 1 {
                            break;
                        }
                        leg = 2;
                        reachable_positions = HashSet::from([self.exit]);
                       }
                     }
                2 => { if reachable_positions.contains(&self.entrance) {
                        leg = 3;
                        reachable_positions = HashSet::from([self.entrance]);
                       }
                     }
                3 => { if reachable_positions.contains(&self.exit) {
                        break;
                       }
                     }
                _ => {}
            }                
        }
        time
    }
}

const EXAMPLE_CONTENT: &str = 
"#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#";

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_valley() {
        let content = EXAMPLE_CONTENT;
        let valley = Valley::new(content);
        assert_eq!(valley.solve(1), 18);
    }
    #[test]
    fn test_valley2() {
        let content = EXAMPLE_CONTENT;
        let valley = Valley::new(content);
        assert_eq!(valley.solve(2), 54);
    }
}