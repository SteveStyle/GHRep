//use std::collections::HashMap;
//use std::fmt::{Display, self};
use std::fs;
use chrono::Local;

//use grid_3d::grid_3d::Grid3d;

//use sprite::sprite::Sprite;
//use std::hash::Hash;
//use std::str::FromStr;
//use regex::Regex;
//extern crate lazy_static;
//use lazy_static::lazy_static;

//mod utils;
//pub mod pos3d;
mod geode;

pub fn show_totals() {
//    const ROOT_PATH: &str = "/home/steve/GHRep/";
    const ROOT_PATH: &str = "d:/GHRep/GHRep/";

    const FILE_NAME: &str = "AdventofCode/2022/data/input19.txt";

    let full_path = String::from(ROOT_PATH) + FILE_NAME;
   
    let content = fs::read_to_string(&full_path)
        .expect(&format!("I was not able to read the file {}.",&full_path));
    let total_score = process_file_contents( &content );
    println!("The part 1 total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}

//use crate::sprite::sprite;
fn process_file_contents( contents: &str) -> i128 {
    let results = crate::geode::extract_info( contents );
    let mut result : i128 = 0;
    for i in 0..results.len() {
        let score = results[i].solve(24);
        println!("{}:  The best for blueprint {} is {:#?}",Local::now().format("%Y-%m-%d %H:%M:%S%.6f"), results[i].blueprint, score);
        result += results[i].blueprint as i128 * score as i128;
    }

    return result;
}


fn process_file_contents2( contents: &str) -> u128 {
    return 0;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_process_file_contents() {
        assert_eq!(process_file_contents(
            "Blueprint 1:        Each ore robot costs 4 ore.
        Each clay robot costs 2 ore.
        Each obsidian robot costs 3 ore and 14 clay.
        Each geode robot costs 2 ore and 7 obsidian.
      
      Blueprint 2:
        Each ore robot costs 2 ore.
        Each clay robot costs 3 ore.
        Each obsidian robot costs 3 ore and 8 clay.
        Each geode robot costs 3 ore and 12 obsidian."), 33);
    }

    #[test]
    fn test_process_file_contents2() {
        assert_eq!(process_file_contents2("Blueprint 1:
        Each ore robot costs 4 ore.
        Each clay robot costs 2 ore.
        Each obsidian robot costs 3 ore and 14 clay.
        Each geode robot costs 2 ore and 7 obsidian.
      
      Blueprint 2:
        Each ore robot costs 2 ore.
        Each clay robot costs 3 ore.
        Each obsidian robot costs 3 ore and 8 clay.
        Each geode robot costs 3 ore and 12 obsidian."), 58);
   }

}

