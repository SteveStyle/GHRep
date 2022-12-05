// this is a library
use std::fs;

pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input3.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The original total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}

fn char_to_priority( c: char ) -> u32 {
    match c {
        'a'..='z' => (c as u32) - 0x60,
        'A'..='Z' => (c as u32) - 0x26,
        _        => panic!("character {} out of range",c)
    }
}

fn convert_line( line: &str ) -> u32 {
    //let chars = line.chars();
    let v: Vec<char> = line.chars().collect();
    let len = v.len();        assert_eq!(len%2,0);
    let left = &v[0..(len/2)];
    let right = &v[len/2..len];
    //println!("left: {}      right: {}",left, right);
    for i in left {
      for j in right {
        if i==j { return char_to_priority( i.clone() ) }
      }
    }
    panic!("no matching character found");
}

fn process_file_contents( contents: &str) -> u32 {
    let mut _cnt = 0;
    let mut total = 0;
    for line in contents.lines() {
      let priority = convert_line(line);
      total += priority;
      _cnt += 1;
        //println!("{} {} = {}",_cnt,line,score);
    }
    //println!("{} lines counted for a total of {}",cnt,total);
    return total;
}

fn convert_line2( line1: &str, line2: &str, line3: &str ) -> u32 {
    let v1: Vec<char> = line1.chars().collect();
    let v2: Vec<char> = line2.chars().collect();
    let v3: Vec<char> = line3.chars().collect();
    for c1 in &v1 {
      for c2 in &v2 {
        if c1==c2 { 
            for c3 in &v3 {
              if c1==c3 {
                return char_to_priority( *c1 ) 
              }
            }
        }
      }
    }
    panic!("no matching character found");
}

fn process_file_contents2( contents: &str) -> u32 {
    let mut total = 0;
    let mut lines = contents.lines();
 //   let  _cnt = lines.count();
    loop {
        if let ( Some(line1), Some(line2), Some(line3) ) = ( lines.next(), lines.next(), lines.next() ) {
            total += convert_line2( line1, line2, line3 );
        } else {
            break;
//            panic!("{} not a set of three lines", _cnt);
        }
    }
    return total; 
    //println!("{} lines counted for a total of {}",cnt,total);
}





#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_conversion() {
        assert_eq!( char_to_priority('A'),27 );
        assert_eq!( char_to_priority('Z'),52 );
        assert_eq!( char_to_priority('a'), 1 );
        assert_eq!( char_to_priority('z'),26 );
    }    
    
    #[test]
    fn test_scoring() {
        
        assert_eq!(convert_line("AA"), 27);
        assert_eq!(convert_line("BCBC"), 28);
        assert_eq!(convert_line("abcaef"), 1);
        assert_eq!(convert_line("abcdaf"), 1);
        assert_eq!(convert_line("abcdea"), 1);
        assert_eq!(convert_line("abcbef"), 2);
        assert_eq!(convert_line("abcdbf"), 2);
        assert_eq!(convert_line("abcdeb"), 2);
        assert_eq!(convert_line("abccef"), 3);
        assert_eq!(convert_line("abcdcf"), 3);
        assert_eq!(convert_line("abcdec"), 3);
        
        assert_eq!(convert_line("vJrwpWtwJgWrhcsFMMfFFhFp"),16);
        assert_eq!(convert_line("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"),38);
        assert_eq!(convert_line("PmmdzqPrVvPwwTWBwg"),42);
        assert_eq!(convert_line("wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"),22);
        assert_eq!(convert_line("ttgJtRGJQctTZtZT"),20);
        assert_eq!(convert_line("CrZsJsPPZsGzwwsLwLmpwMDw"),19);

    }
    
  
    #[test]
    fn test_total_score_basic() {
        assert_eq!(process_file_contents("vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"),157);
    }
    
    #[test]
    fn test_convert_line2() {
        assert_eq!(convert_line2("a","a","a"),1);
        assert_eq!(convert_line2("a","abc","abc"),1);
        assert_eq!(convert_line2("abc","abc","a"),1);
        assert_eq!(convert_line2("abc","c","bc"),3);
        assert_eq!(convert_line2("abc","b","bc"),2);
        assert_eq!(convert_line2("abc","b","bc"),2);
        assert_eq!(convert_line2("abcd","bd","bcd"),2);
        assert_eq!(convert_line2("vJrwpWtwJgWrhcsFMMfFFhFp","jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL","PmmdzqPrVvPwwTWBwg"),18);
        assert_eq!(convert_line2("wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn","ttgJtRGJQctTZtZT","CrZsJsPPZsGzwwsLwLmpwMDw"),52);
        

    }
    
    #[test]
    fn test_process_file_contents2() {
        assert_eq!(process_file_contents2("vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"),70);
    }    
    
    #[test]
    #[should_panic]
    fn test_char_out_of_range_panic() {
        char_to_priority('\u{0040}');
    }
    
    #[test]
    #[should_panic]
    fn test_no_repeat_panic() {
        convert_line("abcdef");
    }

    #[test]
    #[should_panic]
    fn test_odd_length_panic() {
        convert_line("abcdefg");
    }  
      
    #[test]
    #[should_panic]
    fn test_convert_line2_no_match_panic() {
        convert_line2("acd","d","bc");
    }    
 
}

