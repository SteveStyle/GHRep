enum Direction {
    North = 0,
    East,
    South,
    West,
}

enum CellType {
    Empty,
    Start,
    Pipe { exit1: Direction, exit2: Direction },
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
        let mut max_x = input.lines().next().unwrap().len();
        let mut max_y = input.lines().count();
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
                    '-' => CellType::Pipe {
                        exit1: Direction::West,
                        exit2: Direction::East,
                    },
                    '|' => CellType::Pipe {
                        exit1: Direction::North,
                        exit2: Direction::South,
                    },
                    'L' => CellType::Pipe {
                        exit1: Direction::North,
                        exit2: Direction::East,
                    },
                    'J' => CellType::Pipe {
                        exit1: Direction::North,
                        exit2: Direction::West,
                    },
                    '7' => CellType::Pipe {
                        exit1: Direction::South,
                        exit2: Direction::West,
                    },
                    'F' => CellType::Pipe {
                        exit1: Direction::South,
                        exit2: Direction::East,
                    },
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

    fn calculate_distances(&mut self) {
        let start_cell = self.get_cell_mut(self.start_x, self.start_y);
        start_cell.calculated = true;
    }

    pub fn max_distance(&self) -> usize {
        self.cells
            .iter()
            .map(|row| row.iter().map(|cell| cell.distance).max().unwrap())
            .max()
            .unwrap()
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
