//use std::collections::HashMap;
//use std::fmt::{Display, self};
use std::fs;
use chrono::Local;


use crate::utils::Timer;
use crate::snafu::*;


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
mod snafu;

fn get_contents() -> String {
    println!("{}:  starting",Local::now().format("%Y-%m-%d %H:%M:%S%.6f"));

    const ROOT_PATH: &str = "/home/steve/GHRep/";
//    const ROOT_PATH: &str = "d:/GHRep/GHRep/";

    const FILE_NAME: &str = "AdventofCode/2022/data/input25.txt";

    let full_path = String::from(ROOT_PATH) + FILE_NAME;

    let content = fs::read_to_string(&full_path)
        .expect(&format!("I was not able to read the file {}.",&full_path));

    content
}

pub fn show_totals() {
//    let timer = Timer::new("show_totals()");

    let content = get_contents();
    let timer = Timer::new("show_totals()");
    let total_score = process_file_contents2( &content );
    let elapsed = timer.elapsed();
    println!("The part 1 total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("{}:  The part 2 total score is {}.",Local::now().format("%Y-%m-%d %H:%M:%S%.6f"),total_score);

    // Get the elapsed time as a duration
//    let elapsed = timer.elapsed();

    // Format the duration and print it
    println!("Time taken: {} seconds", elapsed.as_secs_f64());

}

//use crate::sprite::sprite;
fn process_file_contents( contents: &str) -> String {
    let sum = contents.lines().map(|s| Snafu::from_string(s).to_value()).sum::<isize>();
    return Snafu::from_value(sum).to_string();
}


fn process_file_contents2( contents: &str) -> String {
    
    return value_to_snafu( contents.lines().map(|s| snafu_to_value(s)).sum::<isize>() );
}



#[cfg(test)]
mod tests {
    use super::*;


const EXAMPLE_CONTENT: &str = 
"1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122";
 
    #[test]
    fn test_process_file_contents() {
        let result = process_file_contents(EXAMPLE_CONTENT);
        assert_eq!(result,"2=-1=0" );
    }

    #[test]
    fn test_process_file_contents2() {
        let result = process_file_contents2(EXAMPLE_CONTENT);
        assert_eq!(result, "");
   }

   #[test]
   fn show_answer() {
       println!("snafu answer: {} is {}", "2-0-020-1==1021=--01", snafu_to_value("2-0-020-1==1021=--01") );
   }

}

