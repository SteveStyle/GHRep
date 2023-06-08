// this is a library
use std::fs;

pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input2.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The original total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The revised total score is {}.",total_score);
}

fn rps_letter_to_int( c: Option<char> ) -> Option<isize> {
    if let Some(c1) = c {
        match c1 {
            'A'|'X' => Some(0),
            'B'|'Y' => Some(1),
            'C'|'Z' => Some(2),
            _       => None
        }
    } else {
        None
    }
}

fn convert_line( line: &str ) -> Option<isize> {
    let mut chars = line.chars();
    let char1 = chars.next(); chars.next(); let char3 = chars.next();
    if let Some(opp) = rps_letter_to_int(char1) {
    if let Some(you) = rps_letter_to_int(char3) {
        let result = (you - opp +4)%3;  //+4 to ensure the result is positive
        let score = result*3 + you + 1; //+1 to convert 'you' from 0,1,2 to 1,2,3.
        return Some( score );
    }
    }
    return None;
}

fn process_file_contents( contents: &str) -> isize {
    let mut _cnt = 0;
    let mut total = 0;
    for line in contents.lines() {
      if let Some(score) = convert_line(line) {
        total += score;
        _cnt += 1;
        //println!("{} {} = {}",_cnt,line,score);
      }
    }
    //println!("{} lines counted for a total of {}",cnt,total);
    return total;
}


fn convert_line2( line: &str ) -> Option<isize> {
    let mut chars = line.chars();
    let char1 = chars.next(); chars.next(); let char3 = chars.next();
    if let Some(opp) = rps_letter_to_int(char1) {
    if let Some(result) = rps_letter_to_int(char3) {
        let you = (result + opp + 2)%3;  //+2 to ensure the result is positive
        let score = result*3 + you + 1;  //+1 to convert 'you' from 0,1,2 to 1,2,3.
        return Some( score );
    }
    }
    return None;
}

fn process_file_contents2( contents: &str) -> isize {
    let mut _cnt = 0;
    let mut total = 0;
    for line in contents.lines() {
      if let Some(score) = convert_line2(line) {
        total += score;
        _cnt += 1;
        //println!("{} {} = {}",_cnt,line,score);
      }
    }
    //println!("{} lines counted for a total of {}",_cnt,total);
    return total;
}



#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_conversion() {
        assert_eq!(rps_letter_to_int(Some('A')),Some(0));
        assert_eq!(rps_letter_to_int(Some('B')),Some(1));
        assert_eq!(rps_letter_to_int(Some('C')),Some(2));
        assert_eq!(rps_letter_to_int(Some('X')),Some(0));
        assert_eq!(rps_letter_to_int(Some('Y')),Some(1));
        assert_eq!(rps_letter_to_int(Some('Z')),Some(2));
        assert_eq!(rps_letter_to_int(Some('£')),None);
        assert_eq!(rps_letter_to_int(Some('D')),None);
        assert_eq!(rps_letter_to_int(Some('W')),None);
        assert_eq!(rps_letter_to_int(Some('1')),None);
        assert_eq!(rps_letter_to_int(Some('0')),None);
        assert_eq!(rps_letter_to_int(Some(' ')),None);
        assert_eq!(rps_letter_to_int(Some('a')),None);
        assert_eq!(rps_letter_to_int(Some('b')),None);
        assert_eq!(rps_letter_to_int(Some('c')),None);
        assert_eq!(rps_letter_to_int(Some('x')),None);
        assert_eq!(rps_letter_to_int(Some('y')),None);
        assert_eq!(rps_letter_to_int(Some('z')),None);
    }    
    
    #[test]
    fn test_scoring() {
        
        assert_eq!(convert_line("A Y"), Some(8));
        assert_eq!(convert_line("B X"), Some(1));
        assert_eq!(convert_line("C Z"), Some(6));

        assert_eq!(convert_line("A X"), Some(4));
        assert_eq!(convert_line("A Z"), Some(3));
        assert_eq!(convert_line("B Y"), Some(5));
        assert_eq!(convert_line("B Z"), Some(9));
        assert_eq!(convert_line("C X"), Some(7));
        assert_eq!(convert_line("C Y"), Some(2));


        assert_eq!(convert_line(""), None);
        assert_eq!(convert_line("A"), None);
        assert_eq!(convert_line("A "), None);
        assert_eq!(convert_line("D"), None);
        assert_eq!(convert_line("D "), None);
        assert_eq!(convert_line("D X"), None);
        assert_eq!(convert_line("A W"), None);
    }
    
    #[test]
    fn test_total_score_basic() {
        assert_eq!(process_file_contents("A Y\nB X\nC Z"),15);
    }
    
    #[test]
    fn test_total_score_complex() {
        assert_eq!(process_file_contents("\nA Y \n\na\nA \nA W\nB X\nC Z\n"),15);
    }
    
    #[test]
    fn test_scoring2() {
        
        assert_eq!(convert_line2("A Y"), Some(4));
        assert_eq!(convert_line2("B X"), Some(1));
        assert_eq!(convert_line2("C Z"), Some(7));

        assert_eq!(convert_line2("A X"), Some(3));
        assert_eq!(convert_line2("A Z"), Some(8));
        assert_eq!(convert_line2("B Y"), Some(5));
        assert_eq!(convert_line2("B Z"), Some(9));
        assert_eq!(convert_line2("C X"), Some(2));
        assert_eq!(convert_line2("C Y"), Some(6));


        assert_eq!(convert_line2(""), None);
        assert_eq!(convert_line2("A"), None);
        assert_eq!(convert_line2("A "), None);
        assert_eq!(convert_line2("D"), None);
        assert_eq!(convert_line2("D "), None);
        assert_eq!(convert_line2("D X"), None);
        assert_eq!(convert_line2("A W"), None);
    }

    #[test]
    fn test_total_score_basic2() {
        assert_eq!(process_file_contents2("A Y\nB X\nC Z"),12);
    }
    
    #[test]
    fn test_total_score_complex2() {
        assert_eq!(process_file_contents2("\nA Y \n\na\nA \nA W\nB X\nC Z\n"),12);
    }
    
 
}

