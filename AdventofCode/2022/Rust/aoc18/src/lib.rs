//use std::collections::HashMap;
//use std::fmt::{Display, self};
use std::fs;

use grid_3d::grid_3d::Grid3d;

//use sprite::sprite::Sprite;
//use std::hash::Hash;
//use std::str::FromStr;
//use regex::Regex;
//extern crate lazy_static;
//use lazy_static::lazy_static;

mod utils;
pub mod pos3d;
mod grid_3d;

pub fn show_totals() {
//    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input18.txt";
const FILE_PATH: &str = "d:/GHRep/GHRep/AdventofCode/2022/data/input18.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The part 1 total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}

//use crate::sprite::sprite;
fn process_file_contents( contents: &str) -> i128 {
    let grid = Grid3d::new(contents);
    return grid.surface_area as i128;
}


fn process_file_contents2( contents: &str) -> u128 {
    let grid = Grid3d::new(contents);
    return grid.surface_area2;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_process_file_contents() {
        assert_eq!(process_file_contents("2,2,2
        1,2,2
        3,2,2
        2,1,2
        2,3,2
        2,2,1
        2,2,3
        2,2,4
        2,2,6
        1,2,5
        3,2,5
        2,1,5
        2,3,5"), 64);
    }

    #[test]
    fn test_process_file_contents2() {
        assert_eq!(process_file_contents2("2,2,2
        1,2,2
        3,2,2
        2,1,2
        2,3,2
        2,2,1
        2,2,3
        2,2,4
        2,2,6
        1,2,5
        3,2,5
        2,1,5
        2,3,5"), 58);
   }

}

