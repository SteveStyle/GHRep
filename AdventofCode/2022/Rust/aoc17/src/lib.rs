use std::collections::HashMap;
//use std::fmt::{Display, self};
use std::fs;
use std::hash::Hash;
use std::str::FromStr;
use regex::Regex;
//extern crate lazy_static;
use lazy_static::lazy_static;


pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input17.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The part 1 total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}


fn get_numbers<T: FromStr>(source: &str) -> Vec<T>
where T::Err: std::fmt::Debug
/* add to toml
[dependencies]
regex = "1.3.9" 
*/
{
    // Use a regular expression to match the first sequence of digits in the string
    // Support negative and floating point numbers.
    let re = Regex::new(r"-?\d+(\.\d+)?").unwrap();
    let mut result: Vec<T> = vec![];
    for captures in re.captures_iter(source) {
        let digit_string = captures.get(0).unwrap().as_str();
        let number = digit_string.parse().unwrap();
        result.push(number);
    }
    return result;
}



fn process_file_contents( contents: &str) -> i32 {
    return 0;
}


fn process_file_contents2( contents: &str) -> i32 {
    return 0;
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn test_process_file_contents() {
        assert_eq!(process_file_contents(">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"), 3068);
    }

    #[test]
    fn test_process_file_contents2() {
        assert_eq!(process_file_contents2(">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"), 0);
   }

}

