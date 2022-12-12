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
                        row_least_moves.push(None);
                    },
                    'E' => {
                        grid.end = (r ,c);
                        row.push('z' as i32);
                        row_least_moves.push(Some(0));
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

    fn check_and_update(&mut self, old_height: i32, new_row: usize, new_col: usize, steps: usize, goal : char ) -> Option<usize> {
        if  self.least_moves[new_row][new_col] == None &&
            self.grid[new_row][new_col] >= old_height - 1 {
                self.least_moves[new_row][new_col] = Some(steps + 1);
                if goal == 'a' && self.grid[new_row][new_col] == 'a' as i32 {
                    return Some(steps + 1);
                } else if ( new_row, new_col ) == self.start {
                        return Some(steps + 1);
                }            
        }
        return None;
    }
    
    fn solve(&mut self, stop_at: usize, goal : char ) -> Option<usize> {
        for steps in 0..stop_at {
            for r in 0..self.no_rows {
                for c in 0..self.no_cols {
                    if self.least_moves[r][c] == Some(steps) {
                        if r > 0                {   if let Some(total_steps) = self.check_and_update( self.grid[r][c], r-1,c, steps, goal ) {
                                                        return Some(total_steps);
                                                    } 
                                                }
                        if r < self.no_rows - 1 {   if let Some(total_steps) = self.check_and_update( self.grid[r][c], r+1,c, steps, goal ) {
                                                        return Some(total_steps);
                                                    } 
                                                }
                        if c > 0                {   if let Some(total_steps) = self.check_and_update( self.grid[r][c], r,c-1, steps, goal ) {
                                                        return Some(total_steps);
                                                    } 
                                                }
                        if c < self.no_cols -1  {   if let Some(total_steps) = self.check_and_update( self.grid[r][c], r,c+1, steps, goal ) {
                                                        return Some(total_steps);
                                                    } 
                                                }
                    }
                }
            }
            if let Some(total_steps) = self.least_moves[self.start.0][self.start.1] {
                return Some(total_steps);
            }
        }

        return None;
        //return self.least_moves[self.end.0][self.end.1].unwrap();
    }
    
}

fn process_file_contents( contents: &str) -> usize {
    let mut grid = Grid::new(contents);
    let total = grid.solve(grid.no_rows*grid.no_cols, 'S').unwrap_or(0);
    return total;
}


fn process_file_contents2( contents: &str) -> usize {
    let mut grid = Grid::new(contents);
    let total = grid.solve(grid.no_rows*grid.no_cols, 'a').unwrap_or(0);
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

