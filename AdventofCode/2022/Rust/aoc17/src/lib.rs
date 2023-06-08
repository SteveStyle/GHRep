//use std::collections::HashMap;
//use std::fmt::{Display, self};
use std::fs;

use sprite::sprite::Sprite;
//use std::hash::Hash;
//use std::str::FromStr;
//use regex::Regex;
//extern crate lazy_static;
//use lazy_static::lazy_static;

pub mod sprite;
mod utils;


pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input17.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The part 1 total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}

//use crate::sprite::sprite;
fn process_file_contents( contents: &str) -> i32 {
    return Sprite::new(contents).move_rocks(2022);
}


fn process_file_contents2( contents: &str) -> u128 {
    let result = Sprite::new(contents).find_cycle(20000,5);
    return result as u128;
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn test_find_cycle() {
        let result = Sprite::new(">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>").find_cycle(100000,5);
        println!("result = {}",result);
        assert_eq!(result, 1514285714288);
    }
  
    #[test]
    fn test_move_rock() {
        println!("rock height is {}", Sprite::new(">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>").move_rocks(10) );
    }
 
    #[test]
    fn test_process_file_contents() {
        assert_eq!(process_file_contents(">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"), 3068);
    }

    #[test]
    fn test_process_file_contents2() {
        assert_eq!(process_file_contents2(">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"), 0);
   }

}

