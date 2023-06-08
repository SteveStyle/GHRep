//use std::collections::HashMap;
//use std::fmt::{Display, self};
use std::fs;
use chrono::Local;
use monkey_yell::MonkeyYell;

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
mod monkey_yell;

fn get_contents() -> String {
    //    const ROOT_PATH: &str = "/home/steve/GHRep/";
    println!("{}:  starting",Local::now().format("%Y-%m-%d %H:%M:%S%.6f"));

    const ROOT_PATH: &str = "d:/GHRep/GHRep/";

    const FILE_NAME: &str = "AdventofCode/2022/data/input21.txt";

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
    // create a MonkeyYell object using contents and return the value of 'root'
    let monkey_yell = monkey_yell::MonkeyYell::new(contents);
    let root_value = monkey_yell.get_value("root");
    return root_value as i128;
}


fn process_file_contents2( contents: &str) -> i128 {
    let mut monkey = MonkeyYell::new(contents);
    let result = monkey.solve2();
    if let Some(result) = result {
        return result as i128;
    } else {
        return 0;
    } 
}



#[cfg(test)]
mod tests {
    use super::*;

 
    #[test]
    fn test_process_file_contents() {
        let contents = "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32";
        // create a MonkeyYell object using contents and return the value of 'root'
        let monkey_yell = monkey_yell::MonkeyYell::new(contents);
        let root_value = monkey_yell.get_value("root");
        assert_eq!(root_value, 152);
    }

    #[test]
    fn test_process_file_contents2() {
        assert_eq!(process_file_contents2("1
        2
        -3
        3
        -2
        0
        4"), 0);
   }

}

