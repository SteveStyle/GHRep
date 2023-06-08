//use std::collections::HashMap;
//use std::fmt::{Display, self};
use std::fs;
use chrono::Local;
use number_list::Num;

use crate::utils::Timer;


//use grid_3d::grid_3d::Grid3d;

//use sprite::sprite::Sprite;
//use std::hash::Hash;
//use std::str::FromStr;
//use regex::Regex;
//extern crate lazy_static;
//use lazy_static::lazy_static;

//use crate::utils;
//use crate::number_list;


mod utils;
//pub mod pos3d;
//mod geode;
mod number_list;

fn get_contents() -> String {
    //    const ROOT_PATH: &str = "/home/steve/GHRep/";
    println!("{}:  starting",Local::now().format("%Y-%m-%d %H:%M:%S%.6f"));

    const ROOT_PATH: &str = "d:/GHRep/GHRep/";

    const FILE_NAME: &str = "AdventofCode/2022/data/input20.txt";

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
fn process_file_contents( contents: &str) -> i128 {
    let mut nl = crate::number_list::NumberList::new( contents, 1);
    nl.find_move_all();
    let v = nl.values_from_0();
    println!("process_file_contents():  values from zero length is {}", v.len());
    println!("get(1000): {}, get(2000): {}, get(3000): {}", nl.get(1000), nl.get(2000), nl.get(3000));
    let result = nl.get(1000) + nl.get(2000) + nl.get(3000);
    return result as i128;
}


fn process_file_contents2( contents: &str) -> i128 {
    let mut nl = crate::number_list::NumberList::new( contents, 2);
    // call find_move_all() ten times
    for _ in 0..10 {
        nl.find_move_all();
    }
    let v = nl.values_from_0();
    println!("process_file_contents():  values from zero length is {}", v.len());
    println!("get(1000): {}, get(2000): {}, get(3000): {}", nl.get(1000), nl.get(2000), nl.get(3000));
    let result = nl.get(1000) + nl.get(2000) + nl.get(3000);
    return result as i128;
}

// produce a string which formats elements from 'a' to 'b' as a string, seperated by tab characters
fn format_vector( v: &Vec<Num>, a: usize, mut b:  usize ) -> String {
    let mut result = String::new();
    if b > v.len() { b = v.len(); }
    for i in a..b {
        result = (result + &v[i].to_string()) + "\t";
    }
    result
}


#[cfg(test)]
mod tests {
    use super::*;

 
    #[test]
    fn test_process_file_contents() {
        assert_eq!(process_file_contents(
            "1
            2
            -3
            3
            -2
            0
            4"), 3);
    }

    #[test]
    fn test_process_file_contents2() {
        assert_eq!(process_file_contents2("1
        2
        -3
        3
        -2
        0
        4"), 1623178306);
   }

}

