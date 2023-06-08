//use std::collections::HashMap;
//use std::fmt::{Display, self};
use std::fs;
use chrono::Local;


use crate::utils::Timer;


//use grid_3d::grid_3d::Grid3d;

//use sprite::sprite::Sprite;
//use std::hash::Hash;
//use std::str::FromStr;
//use regex::Regex;
//extern crate lazy_static;
//use lazy_static::lazy_static;

//use crate::utils;


mod utils;
mod pos;
mod valley;

fn get_contents() -> String {
    println!("{}:  starting",Local::now().format("%Y-%m-%d %H:%M:%S%.6f"));

    const ROOT_PATH: &str = "/home/steve/GHRep/";
//    const ROOT_PATH: &str = "d:/GHRep/GHRep/";

    const FILE_NAME: &str = "AdventofCode/2022/data/input24.txt";

    let full_path = String::from(ROOT_PATH) + FILE_NAME;

    let content = fs::read_to_string(&full_path)
        .expect(&format!("I was not able to read the file {}.",&full_path));

    content
}

pub fn show_totals() {
    let timer = Timer::new("show_totals()");

    let content = get_contents();
    let total_score = process_file_contents( &content );
    println!("The part 1 total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("{}:  The part 2 total score is {}.",Local::now().format("%Y-%m-%d %H:%M:%S%.6f"),total_score);

    // Get the elapsed time as a duration
    let elapsed = timer.elapsed();

    // Format the duration and print it
    println!("Time taken: {} seconds", elapsed.as_secs_f64());

}

//use crate::sprite::sprite;
fn process_file_contents( contents: &str) -> isize {
    let valley = valley::Valley::new(contents);
    let result = valley.solve(1);
    return result;
}


fn process_file_contents2( contents: &str) -> isize {
    let valley = valley::Valley::new(contents);
    let result = valley.solve(2);
    return result;
}



#[cfg(test)]
mod tests {
    use super::*;


const EXAMPLE_CONTENT: &str = 
"#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#";
 
    #[test]
    fn test_process_file_contents() {
        let result = process_file_contents(EXAMPLE_CONTENT);
        assert_eq!(result, 18);
    }

    #[test]
    fn test_process_file_contents2() {
        let result = process_file_contents2(EXAMPLE_CONTENT);
        assert_eq!(result, 54);
   }

}

