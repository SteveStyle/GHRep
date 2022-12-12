// this is a library
use std::fs;
//use std::num::integer::lcm;

pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input12.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The original total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}


fn get_integers( line: &str ) -> Vec<i64> {
    let mut cnt = 0;
    let chars: Vec<char> = line.chars().collect();
    let mut char_iter = line.chars();
    let len = chars.len();
    let mut v:Vec<i64> = Vec::new();
    loop {
        if cnt > len + 2 { panic!("outer: cnt {} is larger than len {}",cnt,len); }
        if let Some(c) = char_iter.next() {
            if c.is_digit(10) { 
                let start = cnt; cnt +=1;
                loop {
                    if cnt > len + 10 { panic!("inner: cnt {} is larger than len {}",cnt,len); }
                    if let Some(c) = char_iter.next() {
                        if !c.is_digit(10) {
                            let end = cnt; 
//                            println!("not final: line: {}\tstart: {}\t end: {}",line,start,end);
                            v.push( chars[start..end].iter().collect::<String>().parse::<i64>().unwrap() );
                            cnt += 1;
                            break;
                        } else {
                            cnt += 1;
                        }
                    } else { 
                        let end = cnt; 
//                        println!("final: line: {}\tstart: {}\t end: {}",line,start,end);
                        v.push( chars[start..end].iter().collect::<String>().parse::<i64>().unwrap() );
                        break;
                    }
                }
            }
            else { cnt += 1; }
        } else {break;}
    }
    return v;
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
        let lines: Vec<&str> = contents.lines().collect();
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
    
    fn solve(&mut self) ->usize {
        for steps in 0..self.no_rows * self.no_cols {
            for r in 0..self.no_rows {
                for c in 0..self.no_cols {
                    if self.least_moves[r][c] == Some(steps) {
                        if r > 0                { Grid::check_and_update( self, self.grid[r][c], r-1,c, steps ); }
                        if r < self.no_rows - 1 { Grid::check_and_update( self, self.grid[r][c], r+1,c, steps );}
                        if c > 0                { Grid::check_and_update( self, self.grid[r][c], r, c-1, steps); }
                        if c < self.no_cols -1  { Grid::check_and_update( self, self.grid[r][c], r, c+1, steps);}
                    }
                }
            }
            if let Some(total_steps) = self.least_moves[self.end.0][self.end.1] {
                return total_steps;
            }
        }

        return self.least_moves[self.end.0][self.end.1].unwrap();
    }
}

fn process_file_contents( contents: &str) -> usize {
    let mut grid = Grid::new(contents);
    let mut total = grid.solve();
    return total;
}


fn process_file_contents2( contents: &str) -> i32 {
    let total =0;
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
        assert_eq!(process_file_contents2("R 4
        U 4
        L 3
        D 1
        R 4
        D 1
        L 5
        R 2"), 0);
    }

}

