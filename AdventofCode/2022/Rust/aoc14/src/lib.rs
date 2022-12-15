use std::fmt::{Display, self};
use std::fs;

pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input14.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The part 1 total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}

#[derive(Clone,Debug,PartialEq,Copy)]
enum Cell {
    Space,
    Rock,
    Sand,
}

impl Display for Cell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Cell::Space => write!(f, "."),
            Cell::Rock => write!(f, "#"),
            Cell::Sand => write!(f, "o"),
        }
    }
}


#[derive(Debug)]
struct Cave {
    leftmost: usize,
    rightmost: usize,
    width: usize,
    depth: usize,
    cells : Vec<Vec<Cell>>,
}

impl Display for Cave {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut result = String::from("");
        for i in 0..self.depth {
            for j in 0..self.width {
                result += &format!("{}",self.cells[j][i]);
            }
            result += &format!("\n");
        }
        write!(f,"{}",result)
    }
}

impl Cave {
    fn set_cell(&mut self,right:usize,down:usize,cell: Cell) {
        self.cells[right - self.leftmost][down] = cell;
    }

    fn get_cell(&mut self,right:usize,down:usize) -> Cell {
        return self.cells[right - self.leftmost][down];
    }

fn new(contents: &str, part2: bool) -> Cave {
        let mut leftmost = 600;
        let mut rightmost = 0;
        let mut depth = 0;
        let mut v : Vec<Vec<(usize,usize)>> = vec![];
        for line in contents.lines() {
            let line = line.trim();
            let mut rock_structure : Vec<(usize,usize)> = vec![];
            for part in line.split(" -> ") {
                let parts = part.split(",").collect::<Vec<&str>>();
                let (right,down) = (parts[0].parse::<usize>().unwrap(), parts[1].parse::<usize>().unwrap());
                if right < leftmost {leftmost = right;}
                if right > rightmost {rightmost = right;}
                if down > depth {depth = down;}
                rock_structure.push((right,down));
            }
            v.push(rock_structure);
        }

        if part2 {
            depth = depth+2;
            leftmost = leftmost - depth;
            rightmost = rightmost + depth;
            v.push(vec![(leftmost,depth),(rightmost,depth)]);
        }

        depth += 1; // change depth from the lowest down index to the number of cells going downward
        let width  = rightmost + 1 - leftmost;

        let mut cells : Vec<Vec<Cell>> = vec![];
        for _i in leftmost..rightmost +1 {
            let shaft: Vec<Cell> = vec![Cell::Space;depth];
            cells.push(shaft);
        }

        let mut cave = Cave{ leftmost, rightmost, width, depth,  cells };

        for rock_structure in v {
            for i in 0..rock_structure.len()-1 {
                let (start_right,start_down) = rock_structure[i];
                let (end_right, end_down) = rock_structure[i+1];
                if start_right == end_right {   // check if start and end are vertically aligned
                    if start_down <= end_down {
                        for down in start_down..end_down+1 {
                            cave.set_cell(start_right, down, Cell::Rock);
                        }
                    } else {
                        for down in end_down..start_down+1 {
                            cave.set_cell(start_right, down, Cell::Rock);
                        }
                    }
                } else if start_down == end_down {  // check if start and end are horizontally aligned
                    if start_right <= end_right {
                        for right in start_right..end_right+1 {
                            cave.set_cell(right, start_down, Cell::Rock);
                        }
                    } else {
                        for right in end_right..start_right+1 {
                            cave.set_cell(right, start_down, Cell::Rock);
                        }
                    }
                } else {
                    panic!("start and end of rock structure segment not aligned: {:?}", &rock_structure);
                }
            }
        }

        //println!("{}",cave);

        return cave;

    }

    fn move_sand(&mut self,right:usize,down:usize) -> bool {
        // Either places sand in it's settled location and returns true, or returns false if the sand never settles.
        // right co-ordinate is in the range leftmost to rightmost, not 0 to width-1.
        // So we must use get_cell() and set_cell().
        if self.get_cell(right, down) != Cell::Space {return false;} // this should not happen, so we should probably have an assertion
        if down == self.depth -1 {return false;}  // if we reach the bottom row we fall forever
        if self.get_cell(right,down+1) == Cell::Space {return self.move_sand(right,down+1);}
        if right == self.leftmost {return false;}  // we go off the grid to the left and fall forever
        if self.get_cell(right-1,down+1) == Cell::Space {return self.move_sand(right-1,down+1);}
        if right == self.rightmost {return false;}  // we go off the grid to the right and fall forever
        if self.get_cell(right+1,down+1) == Cell::Space {return self.move_sand(right+1,down+1);}
        self.set_cell(right,down,Cell::Sand);  // the sand settles
        return true;
    }
    fn add_sand(&mut self) -> usize {
        // fills the cave with sand
        let mut total = 0;
        for i in 0..self.width*self.depth {
            if self.move_sand(500,0) {
                total +=1;
            } else {
                return total;
            }
        } 
        total
    }
}


fn process_file_contents( contents: &str) -> usize {
    let mut cave = Cave::new(contents,false);
    let total = cave.add_sand();
    //println!("{}",&cave);
    return total;
}


fn process_file_contents2( contents: &str) -> usize {
    let mut cave = Cave::new(contents,true);
    let total = cave.add_sand();
    //println!("{}",&cave);
    return total;
}



#[cfg(test)]
mod tests {
    use super::*;
   
    
  
    #[test]
    fn test_process_file_contents() {
        assert_eq!(process_file_contents("498,4 -> 498,6 -> 496,6
        503,4 -> 502,4 -> 502,9 -> 494,9"), 24);
    }

    #[test]
    fn test_process_file_contents2() {
        assert_eq!(process_file_contents2("498,4 -> 498,6 -> 496,6
        503,4 -> 502,4 -> 502,9 -> 494,9"), 93);
    }

}

