// this is a library
use std::fs;

pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input6.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The original total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}


// find the distane to the first element which ends a sequence of four different characters
fn distance(length: usize, v:&Vec<char>)->Option<usize> {
    'outer: for i in 0..v.len()-3 {
        for j in 0..length-1 {
            for k in j+1..length {
                if v[i+j] == v[i+k] {continue 'outer;}
            }
        }
        return Some(i+length);
    }
    return None;
}

fn process_file_contents( contents: &str) -> usize {
    let v = contents.chars().collect::<Vec<char>>();
    if let Some(result) = distance(4,&v) {
        return result;
    }
    panic!("no result for :{}",contents)
}

fn process_file_contents2( contents: &str) -> usize {
    let v = contents.chars().collect::<Vec<char>>();
    if let Some(result) = distance(14,&v) {
        return result;
    }
    panic!("no result for :{}",contents)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    
    #[test]
    fn test_process_file_contents() {
        assert_eq!(process_file_contents("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 7);
        assert_eq!(process_file_contents("bvwbjplbgvbhsrlpgdmjqwftvncz"), 5);
        assert_eq!(process_file_contents("nppdvjthqldpwncqszvftbrmjlhg"), 6);
        assert_eq!(process_file_contents("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 10);
        assert_eq!(process_file_contents("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 11);
    }

    #[test]
    fn test_process_file_contents2() {
        assert_eq!(process_file_contents2("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 19);
        assert_eq!(process_file_contents2("bvwbjplbgvbhsrlpgdmjqwftvncz"), 23);
        assert_eq!(process_file_contents2("nppdvjthqldpwncqszvftbrmjlhg"), 23);
        assert_eq!(process_file_contents2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 29);
        assert_eq!(process_file_contents2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 26);
    }

}

