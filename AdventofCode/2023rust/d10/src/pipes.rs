use std::cell;
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
    calculated: bool,
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
        let mut max_x = input.lines().next().unwrap().len() - 1;
        let mut max_y = input.lines().count() - 1;
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
                    calculated: false,
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
        let start_cell = self.get_cell_mut(self.start_x, self.start_y);
        start_cell.calculated = true;
        let mut paths = Vec::new();
        if self.start_x > 0 {
            if let CellType::Pipe(directions) =
                self.get_cell(self.start_x - 1, self.start_y).cell_type
            {
                if directions.contains(&Direction::East) {
                    paths.push(Path {
                        pipe_map: self,
                        x: self.start_x - 1,
                        y: self.start_y,
                        distance: 1,
                        came_from: Direction::East,
                    });
                }
            }
        }
        if self.start_x < self.max_x {
            if let CellType::Pipe(directions) =
                self.get_cell(self.start_x + 1, self.start_y).cell_type
            {
                if directions.contains(&Direction::West) {
                    paths.push(Path {
                        pipe_map: self,
                        x: self.start_x + 1,
                        y: self.start_y,
                        distance: 1,
                        came_from: Direction::West,
                    });
                }
            }
        }

        if self.start_y > 0 {
            if let CellType::Pipe(directions) =
                self.get_cell(self.start_x, self.start_y - 1).cell_type
            {
                if directions.contains(&Direction::South) {
                    paths.push(Path {
                        pipe_map: self,
                        x: self.start_x,
                        y: self.start_y - 1,
                        distance: 1,
                        came_from: Direction::South,
                    });
                }
            }
        }
        if self.start_y < self.max_y {
            if let CellType::Pipe(directions) =
                self.get_cell(self.start_x, self.start_y + 1).cell_type
            {
                if directions.contains(&Direction::North) {
                    paths.push(Path {
                        pipe_map: self,
                        x: self.start_x,
                        y: self.start_y + 1,
                        distance: 1,
                        came_from: Direction::North,
                    });
                }
            }
        }
        let mut max_distance = 0;
        'outer: loop {
            for path in &mut paths {
                if path.next().is_none() {
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
}

struct Path {
    pipe_map: &PipeMap,
    x: usize,
    y: usize,
    distance: usize,
    came_from: Direction,
}

impl Iterator for Path {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        let current_cell = self.pipe_map.get_cell_mut(self.x, self.y);
        if current_cell.calculated {
            return None;
        }
        current_cell.calculated = true;
        current_cell.distance = self.distance;
        self.distance += 1;
        for direction in current_cell.directions.iter() {
            if direction == &self.came_from && self.cell_type != CellType::Start {
                continue;
            }
            match direction {
                Direction::North => {
                    if self.y > 0 {
                        self.y -= 1;
                        self.came_from = Direction::South;
                        return Some((self.x, self.y));
                    }
                }
                Direction::East => {
                    if self.x < self.max_x {
                        self.x += 1;
                        self.came_from = Direction::West;
                        return Some((self.x, self.y));
                    }
                }
                Direction::South => {
                    if self.y < self.max_y {
                        self.y += 1;
                        self.came_from = Direction::North;
                        return Some((self.x, self.y));
                    }
                }
                Direction::West => {
                    if self.x > 0 {
                        self.x -= 1;
                        self.came_from = Direction::East;
                        return Some((self.x, self.y));
                    }
                }
            }
        }
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
