#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    North = 0,
    East,
    South,
    West,
}

enum CellType {
    Empty,
    Start,
    Pipe([Direction; 2]),
}

struct Cell {
    cell_type: CellType,
    distance: usize,
    distance_calculated: bool,
    from_direction: Option<Direction>,
    to_direction: Option<Direction>,
}

struct Path {
    x: usize,
    y: usize,
    distance: usize,
    came_from: Direction,
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
                    from_direction: None,
                    to_direction: None,
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

        let start_cell = self.get_cell_mut(start_x, start_y);
        start_cell.distance_calculated = true;
        let mut paths = Vec::new();
        if self.start_x > 0 {
            if let CellType::Pipe(directions) = self.get_cell(start_x - 1, start_y).cell_type {
                if directions.contains(&Direction::East) {
                    paths.push(Path {
                        x: start_x - 1,
                        y: start_y,
                        distance: 1,
                        came_from: Direction::East,
                    });
                }
            }
        }
        if start_x < max_x {
            if let CellType::Pipe(directions) = self.get_cell(start_x + 1, start_y).cell_type {
                if directions.contains(&Direction::West) {
                    paths.push(Path {
                        x: start_x + 1,
                        y: start_y,
                        distance: 1,
                        came_from: Direction::West,
                    });
                }
            }
        }

        if start_y > 0 {
            if let CellType::Pipe(directions) = self.get_cell(start_x, start_y - 1).cell_type {
                if directions.contains(&Direction::South) {
                    paths.push(Path {
                        x: start_x,
                        y: start_y - 1,
                        distance: 1,
                        came_from: Direction::South,
                    });
                }
            }
        }
        if start_y < max_y {
            if let CellType::Pipe(directions) = self.get_cell(start_x, start_y + 1).cell_type {
                if directions.contains(&Direction::North) {
                    paths.push(Path {
                        x: start_x,
                        y: start_y + 1,
                        distance: 1,
                        came_from: Direction::North,
                    });
                }
            }
        }
        let mut max_distance = 0;
        'outer: loop {
            for path in &mut paths {
                if self.next(path).is_none() {
                    max_distance = path.distance;
                    break 'outer;
                }
            }
        }
        max_distance
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
