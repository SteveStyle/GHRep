// this is a library
use std::fs;
//use std::num::integer::lcm;

pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input12.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The part 1 total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}

struct Grid {
    grid : Vec<Vec<i32>>,
    least_moves : Vec<Vec<Option<usize>>>,
    start : (usize,usize),
    end : (usize,usize),
    no_rows : usize,
    no_cols : usize,
}

impl Grid {
    fn new( contents : &str ) -> Grid {
        let mut grid = Grid{ grid: vec![], least_moves: vec![], start: (0,0), end: (0,0), no_rows: 0, no_cols: 0 };
        let mut lines: Vec<&str> = vec![];
        for line in contents.lines() {
            lines.push(line.trim());
        }
        grid.no_rows = lines.len();
        grid.no_cols = lines[0].len();
        for r in 0..grid.no_rows {
            let mut row : Vec<i32> = vec![];
            let mut row_least_moves : Vec<Option<usize>> = vec![];
            let chars: Vec<char> = lines[r].chars().collect();
            for c in 0..grid.no_cols {
                match chars[c] {
                    'S' => {
                        grid.start = (r,c);
                        row.push('a' as i32);
                        row_least_moves.push(Some(0));
                    },
                    'E' => {
                        grid.end = (r ,c);
                        row.push('z' as i32);
                        row_least_moves.push(None);
                    },
                    _ => {
                        row.push(chars[c] as i32);
                        row_least_moves.push(None);
                    }
                }
            }
            assert_eq!(row.len(),grid.no_cols);
            assert_eq!(row_least_moves.len(),grid.no_cols);
            grid.grid.push(row);
            grid.least_moves.push(row_least_moves);
        }
        assert_eq!(grid.grid.len(),grid.no_rows);
        assert_eq!(grid.least_moves.len(),grid.no_rows);

        return grid;
    }

    fn check_and_update(&mut self, old_height: i32, new_row: usize, new_col: usize, steps: usize ) {
        if  self.least_moves[new_row][new_col] == None &&
            self.grid[new_row][new_col] <= old_height + 1 {
            self.least_moves[new_row][new_col] = Some(steps + 1);
        }
    }
    
    fn solve(&mut self, stop_at: usize ) -> Option<usize> {
        for steps in 0..stop_at {
            for r in 0..self.no_rows {
                for c in 0..self.no_cols {
                    if self.least_moves[r][c] == Some(steps) {
                        if r > 0                { self.check_and_update( self.grid[r][c], r-1,c, steps ); }
                        if r < self.no_rows - 1 { self.check_and_update( self.grid[r][c], r+1,c, steps );}
                        if c > 0                { self.check_and_update( self.grid[r][c], r, c-1, steps); }
                        if c < self.no_cols -1  { self.check_and_update( self.grid[r][c], r, c+1, steps);}
                    }
                }
            }
            if let Some(total_steps) = self.least_moves[self.end.0][self.end.1] {
                return Some(total_steps);
            }
        }

        return None;
        //return self.least_moves[self.end.0][self.end.1].unwrap();
    }

    fn set_start(&mut self, r_start :usize, c_start : usize) {
        self.start = (r_start, c_start);
        for r in 0..self.no_rows {
            for c in 0..self.no_cols {
                self.least_moves[r][c] = None;
            }
        }
        self.least_moves[r_start][c_start] = Some(0);
    }

    fn best_start(&mut self) -> Option<usize> {
        let maximum = self.no_rows * self.no_cols;
        let mut result:usize = maximum;
        for r in 0..self.no_rows {
            for c in 0..self.no_cols {
                if self.grid[r][c] == 'a' as i32 {
                    self.set_start(r, c);
                    if let Some(steps) = self.solve(result) {
                        if steps < result { result = steps; }
                    }
                }
            }
        }
        if result < maximum { return Some(result); }
        else {return None; }
    }
}

fn process_file_contents( contents: &str) -> usize {
    let mut grid = Grid::new(contents);
    let total = grid.solve(grid.no_rows*grid.no_cols).unwrap_or(0);
    return total;
}


fn process_file_contents2( contents: &str) -> usize {
    let mut grid = Grid::new(contents);
    let total = grid.best_start().unwrap_or(0);
    return total;
}



#[cfg(test)]
mod tests {
    use super::*;
   
    
    #[test]
    fn test_process_file_contents() {
        assert_eq!(process_file_contents("Sabqponm
        abcryxxl
        accszExk
        acctuvwj
        abdefghi"), 31);
    }

    #[test]
    fn test_process_file_contents2() {
        assert_eq!(process_file_contents2("Sabqponm
        abcryxxl
        accszExk
        acctuvwj
        abdefghi"), 29);
    }

}

