// this is a library
use std::fs;
use std::cmp;

pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input9.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The original total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}

#[derive(Default)]
struct Forest { grid : Vec<Vec<u32>> }

impl Forest {

    pub fn from_string( contents: &str) -> Forest {
        let mut forest = Forest{ grid: vec![] };
        for line in contents.lines() {
            let line = line.trim();
            let mut row : Vec<u32> = vec![];
            for c in line.chars() {
                if let Some(d) = c.to_digit(10) {
                    row.push(d);
                } else {
                    panic!("could not convert character {} to a digit in line {}",c,line)
                }
            }
            forest.grid.push(row);
        }
        return forest;
    }

    // is the tree visible in a particular direction
    fn is_visible( value: u32, list : &[u32] ) -> bool {
        return list.iter().fold(true, |acc, x| {acc && cmp::Ordering::Greater == value.cmp(x)});
    }
    
    fn is_visible_all( &self, row : usize, col : usize ) ->bool {
        let tree_height = &self.grid[row][col];
        let left  = &self.grid[row][..col];
        let right = &self.grid[row][col+1..];
        let up_rows= &self.grid[..row];
        let up : Vec<u32> = up_rows.iter().map(|x| x[col]).collect();
        let down_rows = &self.grid[row+1..];
        let down : Vec<u32> = down_rows.iter().map(|x| x[col]).collect();

        return  Forest::is_visible( *tree_height, left)  || 
                Forest::is_visible( *tree_height, right) || 
                Forest::is_visible( *tree_height, &up)   || 
                Forest::is_visible( *tree_height, &down);
    }

    fn count_visible( &self ) -> usize {
        let mut total = 0;
        let no_rows = self.grid.len();
        let no_cols = self.grid[0].len();
        for row in 0..no_rows {
            for col in 0..no_cols {
                if self.is_visible_all(row, col) {
                    total += 1;
                }
            }
        }
        return total;
    }

    fn count_visible2(value: u32, list : Vec<u32> ) -> usize {
        let mut cnt = 0;
        for i in list {
            cnt += 1;
            if i >= value {break;}
        }
        return cnt;
    }

    fn count_visible_all( &self, row : usize, col : usize ) -> usize {
        let tree_height = &self.grid[row][col];
        let mut left  = self.grid[row][..col].to_vec();
        left.reverse();
        let right = self.grid[row][col+1..].to_vec();
        let up_rows= &self.grid[..row];
        let mut up : Vec<u32> = up_rows.iter().map(|x| x[col]).collect();
        up.reverse();
        let down_rows = &self.grid[row+1..];
        let down : Vec<u32> = down_rows.iter().map(|x| x[col]).collect();

        return  Forest::count_visible2( *tree_height, left)  * 
                Forest::count_visible2( *tree_height, right) * 
                Forest::count_visible2( *tree_height, up)   * 
                Forest::count_visible2( *tree_height, down);
    }

    fn most_visible( &self ) -> usize {
        let mut total = 0;
        let mut mrow = 0;
        let mut mcol = 0;
        let no_rows = self.grid.len();
        let no_cols = self.grid[0].len();
        for row in 0..no_rows {
            for col in 0..no_cols {
                let no_visible = self.count_visible_all(row, col);
                if no_visible > total {
                    total = no_visible;
                    mrow = row;
                    mcol = col;
                }
            }
        }
        println!("most found was {} at row {} and column {}", total, mrow, mcol);

        return total;
    }

}

fn process_file_contents( contents: &str) -> usize {
    let forest = Forest::from_string(contents);
    return forest.count_visible();
    
}

fn process_file_contents2( contents: &str) -> usize {
    let forest = Forest::from_string(contents);
    return forest.most_visible();
}



#[cfg(test)]
mod tests {
    use super::*;
   
    
    #[test]
    fn test_process_file_contents() {
        assert_eq!(process_file_contents("30373
        25512
        65332
        33549
        35390"), 21);
    }

    #[test]
    fn test_process_file_contents2() {
        assert_eq!(process_file_contents2("30373
        25512
        65332
        33549
        35390"), 8);
    }

    #[test]
    fn test_most_visible() {

        assert_eq!(process_file_contents2("30373
        25512
        65332
        33549
        35390"), 8);
    }

}

