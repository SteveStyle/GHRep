// this is a library
use std::fs;

pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input5.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The original total score is {}.",total_score);
//    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}

 

fn process_line( line: &str ) -> bool {
    //let chars = line.chars();
    let v = get_integers( line );
    let len = v.len();                      assert!(len==4,"{} integers found in line: {}",len,&line);
    return (v[0] - v[2])*(v[1] - v[3]) <= 0; 
}

fn process_file_contents( contents: &str) -> &str {
    let mut _cnt = 0;
    let move_lines: Vec<&str> = Vec::New();
    let stack_lines: Vec<&str> = Vec::New();
    for line in contents.lines() {
        _cnt += 1;
        if line == "" {continue;}
        if line[0..2] == " 1" {let heading_line = line; continue;}
        if line[0..4] == "move" {move_lines.push(line); continue;}
        stack_lines.push(line);
    }
    
    

    return total;
}



#[cfg(test)]
mod tests {
    use super::*;
    
    
    #[test]
    fn test_process_file_contents() {
        assert_eq!(process_file_contents("    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"), "CMZ");
    }
    
    #[test]
    #[should_panic]
    fn test_panic() {
        process_line("1 2 3");
        process_line("1 2 3 4 5");
    }    
 
}

