#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    North = 0,
    East,
    South,
    West,
}

impl Direction {
    fn opposite(&self) -> Self {
        match self {
            Direction::North => Direction::South,
            Direction::East => Direction::West,
            Direction::South => Direction::North,
            Direction::West => Direction::East,
        }
    }
    fn clockwise_quarter_turns(&self, other: &Self) -> usize {
        match (*other - *self).rem_euclid(4) {
            3 => -1,
            n => n,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CellType {
    Empty,
    Start,
    Pipe([Direction; 2]),
}

struct Cell {
    cell_type: CellType,
    distance: usize,
    distance_calculated: bool,
    previous_direction: Option<Direction>,
    next_direction: Option<Direction>,
    is_loop: bool,
    is_inside_loop: Option<bool>,
}

impl Cell {
    fn set_previous_direction(&mut self, previous_direction: Direction) -> Direction {
        if let CellType::Pipe(directions) = self.cell_type {
            self.previous_direction = Some(previous_direction);
            let next_direction = *(directions
                .iter()
                .find(|d| d != &&previous_direction)
                .unwrap());
            self.next_direction = Some(next_direction);
            next_direction
        } else {
            panic!("Cannot set previous direction on non-pipe cell");
        }
    }

}


pub struct PipeMap {
    cells: Vec<Vec<Cell>>,
    max_x: usize,
    max_y: usize,
    start_x: usize,
    start_y: usize,
}

impl PipeMap {
    pub fn from_string(input: &str) -> Self {
        let mut cells = Vec::new();
        let max_x = input.lines().next().unwrap().len() - 1;
        let max_y = input.lines().count() - 1;
        let mut start_x = usize::MAX;
        let mut start_y = usize::MAX;
        let mut start_found = false;
        for (i, line) in input.lines().enumerate() {
            let mut row = Vec::new();
            for (j, c) in line.chars().enumerate() {
                let cell_type = match c {
                    '.' => CellType::Empty,
                    'S' => {
                        if start_found {
                            panic!("two start cells")
                        };
                        start_x = j;
                        start_y = i;
                        start_found = true;
                        CellType::Start
                    }
                    '-' => CellType::Pipe([Direction::East, Direction::West]),
                    '|' => CellType::Pipe([Direction::North, Direction::South]),
                    'L' => CellType::Pipe([Direction::North, Direction::East]),
                    'J' => CellType::Pipe([Direction::North, Direction::West]),
                    '7' => CellType::Pipe([Direction::South, Direction::West]),
                    'F' => CellType::Pipe([Direction::South, Direction::East]),
                    _ => panic!("Unexpected character in input: {}", c),
                };
                row.push(Cell {
                    cell_type,
                    distance: 0,
                    distance_calculated: false,
                    previous_direction: None,
                    next_direction: None,
                });
            }
            cells.push(row);
        }
        assert!(start_found, "No start cell found");
        PipeMap {
            cells,
            max_x,
            max_y,
            start_x,
            start_y,
        }
    }

    fn get_cell(&self, x: usize, y: usize) -> &Cell {
        &self.cells[y][x]
    }

    fn get_cell_mut(&mut self, x: usize, y: usize) -> &mut Cell {
        &mut self.cells[y][x]
    }

    pub fn calculate_distances(&mut self) -> usize {
        let start_x = self.start_x;
        let start_y = self.start_y;
        let max_x = self.max_x;
        let max_y = self.max_y;

        let mut current_x = start_x;
        let mut current_y = start_y;

        let mut previous_direction: Direction;
        let mut next_direction: Direction;
        let mut distance_travelled = 0;
        let mut clockwise_quarter_turns = 0;

        let start_cell = self.get_cell_mut(start_x, start_y);
        start_cell.distance_calculated = true;
        let mut paths = Vec::new();
        if self.start_x > 0 {
            if let CellType::Pipe(directions) = self.get_cell(start_x - 1, start_y).cell_type {
                if directions.contains(&Direction::East) {
                    current_x = start_x - 1;
                    current_y = start_y;
                    previous_direction = Direction::West;
                    start_cell.next_direction = Some(Direction::West);
                }
            }
        } else if start_x < max_x {
            if let CellType::Pipe(directions) = self.get_cell(start_x + 1, start_y).cell_type {
                if directions.contains(&Direction::West) {
                    current_x = start_x + 1;
                    current_y = start_y;
                    previous_direction = Direction::East;
                    start_cell.next_direction = Some(Direction::East);
                }
            }
        } else if start_y > 0 {
            if let CellType::Pipe(directions) = self.get_cell(start_x, start_y - 1).cell_type {
                if directions.contains(&Direction::South) {
                    current_x = start_x;
                    current_y = start_y - 1;
                    previous_direction = Direction::North;
                    start_cell.next_direction = Some(Direction::North);
                }
            }
        } else if start_y < max_y {
            if let CellType::Pipe(directions) = self.get_cell(start_x, start_y + 1).cell_type {
                if directions.contains(&Direction::North) {
                    current_x = start_x;
                    current_y = start_y + 1;
                    previous_direction = Direction::South;
                    start_cell.next_direction = Some(Direction::South);
                }
            }
        }
        let mut max_distance = 0;
        'outer: loop {
            let current_cell = self.get_cell_mut(current_x, current_y);
            if current_cell.distance_calculated {
                break 'outer;
            }
            distance_travelled += 1;
            current_cell.distance = distance_travelled;
            current_cell.distance_calculated = true;
            next_direction = current_cell.set_previous_direction(previous_direction);
            clockwise_quarter_turns += previous_direction.clockwise_quarter_turns(&next_direction);
            previous_direction = next_direction;
            match next_direction {
                Direction::North => {
                    if current_y > 0 {
                        current_y -= 1;
                    } else {
                        break 'outer;
                    }
                }
                Direction::East => {
                    if current_x < max_x {
                        current_x += 1;
                    } else {
                        break 'outer;
                    }
                }
                Direction::South => {
                    if current_y < max_y {
                        current_y += 1;
                    } else {
                        break 'outer;
                    }
                }
                Direction::West => {
                    if current_x > 0 {
                        current_x -= 1;
                    } else {
                        break 'outer;
                    }
                }
            }
        }
        max_distance = (distance_travelled + 1) / 2;
        println!("clockwise_quarter_turns: {}", clockwise_quarter_turns);
        let clockwise = clockwise_quarter_turns == 4;
        
        }
    }

    pub fn max_distance(&self) -> usize {
        self.cells
            .iter()
            .map(|row| row.iter().map(|cell| cell.distance).max().unwrap())
            .max()
            .unwrap()
    }

    fn next(&mut self, path: &mut Path) -> Option<(usize, usize)> {
        let current_cell = self.get_cell_mut(path.x, path.y);
        if current_cell.distance_calculated {
            return None;
        }
        current_cell.distance_calculated = true;
        current_cell.distance = path.distance;
        path.distance += 1;
        if let CellType::Pipe(directions) = current_cell.cell_type {
            for direction in directions.iter() {
                if direction == &path.came_from {
                    continue;
                }
                match direction {
                    Direction::North => {
                        if path.y > 0 {
                            path.y -= 1;
                            path.came_from = Direction::South;
                            return Some((path.x, path.y));
                        }
                    }
                    Direction::East => {
                        if path.x < self.max_x {
                            path.x += 1;
                            path.came_from = Direction::West;
                            return Some((path.x, path.y));
                        }
                    }
                    Direction::South => {
                        if path.y < self.max_y {
                            path.y += 1;
                            path.came_from = Direction::North;
                            return Some((path.x, path.y));
                        }
                    }
                    Direction::West => {
                        if path.x > 0 {
                            path.x -= 1;
                            path.came_from = Direction::East;
                            return Some((path.x, path.y));
                        }
                    }
                }
            }
        }
        None
    }
}

#[allow(dead_code)]
const EXAMPLE_INPUT: &str = ".....
.S-7.
.|.|.
.L-J.
.....";

const EXAMPLE_INPUT2: &str = "..F7.
.FJ|.
SJ.L7
|F--J
LJ...";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_clockwise_quarter_turns() {
        assert_eq!(
            Direction::North.clockwise_quarter_turns(&Direction::East),
            1
        );
        assert_eq!(
            Direction::North.clockwise_quarter_turns(&Direction::West),
            3
        );
        assert_eq!(
            Direction::East.clockwise_quarter_turns(&Direction::North),
            3
        );
        assert_eq!(
            Direction::East.clockwise_quarter_turns(&Direction::South),
            1
        );
        assert_eq!(
            Direction::South.clockwise_quarter_turns(&Direction::East),
            3
        );
        assert_eq!(
            Direction::South.clockwise_quarter_turns(&Direction::West),
            1
        );
        assert_eq!(
            Direction::West.clockwise_quarter_turns(&Direction::North),
            1
        );
        assert_eq!(
            Direction::West.clockwise_quarter_turns(&Direction::South),
            3
        );
    }
    #[test]
    fn test_from_string() {
        let pipe_map = PipeMap::from_string(EXAMPLE_INPUT);
        assert_eq!(pipe_map.max_x, 4);
        assert_eq!(pipe_map.max_y, 4);
        assert_eq!(pipe_map.start_x, 1);
        assert_eq!(pipe_map.start_y, 1);
    }

    #[test]
    fn test_calculate_distances() {
        let mut pipe_map = PipeMap::from_string(EXAMPLE_INPUT);
        pipe_map.calculate_distances();
        assert_eq!(pipe_map.max_distance(), 6);
    }

    #[test]
    fn test_calculate_distances2() {
        let mut pipe_map = PipeMap::from_string(EXAMPLE_INPUT2);
        pipe_map.calculate_distances();
        assert_eq!(pipe_map.max_distance(), 24);
    }
}
