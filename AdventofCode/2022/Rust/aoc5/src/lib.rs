// this is a library
use std::fs;

pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input5.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The original total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}


fn get_integers( line: &str ) -> Vec<usize> {
    let mut cnt = 0;
    let chars: Vec<char> = line.chars().collect();
    let mut char_iter = line.chars();
    let len = chars.len();
    let mut v:Vec<usize> = Vec::new();
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
                            v.push( chars[start..end].iter().collect::<String>().parse::<usize>().unwrap() );
                            cnt += 1;
                            break;
                        } else {
                            cnt += 1;
                        }
                    } else { 
                        let end = cnt; 
//                        println!("final: line: {}\tstart: {}\t end: {}",line,start,end);
                        v.push( chars[start..end].iter().collect::<String>().parse::<usize>().unwrap() );
                        break;
                    }
                }
            }
            else { cnt += 1; }
        } else {break;}
    }
    return v;
}            
        
fn print_stacks( stacks : &Vec<Vec<char>>) {
    for i in 0..stacks.len() {
        println!("{}:  {}",i+1,String::from_iter(stacks[i].iter()));
    }
}

// returns move_lines, stacks
fn read_file_data( contents: &str) -> ( Vec<&str>,  Vec<Vec<char>>) {
    let mut _cnt = 0;
    let mut move_lines: Vec<&str> = Vec::new();
    let mut stack_lines: Vec<&str> = Vec::new();
    let mut heading_line = "";
    for line in contents.lines() {
        _cnt += 1;
        if line == "" {continue;}
        if line.starts_with(" 1") {heading_line = line; continue;}
        if line.starts_with("move") {move_lines.push(line); continue;}
        stack_lines.push(line);
    }

    let headings = get_integers(heading_line);
    let mut stacks: Vec<Vec<char>> = Vec::new();
    for _i in headings {
        stacks.push(Vec::new());
    }

    stack_lines.reverse();
    for sl in stack_lines {
        for index in 0..stacks.len() {
            if let Some(c) = sl.chars().nth(4*index +1) {
                if c != ' ' {
                    stacks[index].push(c);
                }
            }
        }
    }

    return ( move_lines, stacks );

}


fn process_file_contents( contents: &str) -> String {
    let ( move_lines, mut stacks ) = read_file_data(contents);
    
    for move_line in move_lines {
        println!("processing {}",move_line);
        let v = get_integers(move_line);                assert_eq!(v.len(),3);
        for _i in 0..v[0] {
            if let Some(c) = stacks[v[1]-1].pop() {
                stacks[v[2]-1].push(c);
            }
        }
        print_stacks(&stacks);
    }    

    let mut result: Vec<char> = Vec::new();
    for i in 0..stacks.len() {
        if let Some(c) = stacks[i].last() {
            result.push(c.clone());
        }
    }
    return String::from_iter(result.into_iter());
}

fn process_file_contents2( contents: &str) -> String {
    let ( move_lines, mut stacks ) = read_file_data(contents);
    
    for move_line in move_lines {
        println!("processing {}",move_line);
        let v = get_integers(move_line);                assert_eq!(v.len(),3);
        let mut temp_stack : Vec<char> = Vec::new();
        for _i in 0..v[0] {
            if let Some(c) = stacks[v[1]-1].pop() {
                temp_stack.push(c);
            }
        }
        for _i in 0..v[0] {
            if let Some(c) = temp_stack.pop() {
                stacks[v[2]-1].push(c);
            }
        }
        print_stacks(&stacks);
    }    

    let mut result: Vec<char> = Vec::new();
    for i in 0..stacks.len() {
        if let Some(c) = stacks[i].last() {
            result.push(c.clone());
        }
    }
    return String::from_iter(result.into_iter());
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
    fn test_process_file_contents2() {
        assert_eq!(process_file_contents2("    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"), "MCD");
    }
    
    #[test]
    #[should_panic]
    fn test_panic() {
        panic!("dummy");
    }    
 
}

