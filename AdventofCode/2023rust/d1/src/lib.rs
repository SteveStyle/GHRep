//use std::collections::HashMap;
//use std::fmt::{Display, self};
use chrono::Local;
//use lazy_static::lazy_static;
//use std::fs;
//use utils::get_numbers;

use crate::utils::Timer;
//use crate::snafu::*;

//use grid_3d::grid_3d::Grid3d;

//use sprite::sprite::Sprite;
//use std::hash::Hash;
//use std::str::FromStr;
//use regex::Regex;
//extern crate lazy_static;
//use lazy_static::lazy_static;

//use crate::utils;

const INPUT: &'static str = include_str!("input.txt");

//mod pos;
//mod snafu;
mod utils;

pub fn show_totals() {
    //    let timer = Timer::new("show_totals()");

    let timer = Timer::new("show_totals()");
    let total_score = process_input(&INPUT);
    let elapsed = timer.elapsed();
    println!("The part 1 total score is {}.", total_score);
    let total_score = process_input2(&INPUT);
    println!(
        "{}:  The part 2 total score is {}.",
        Local::now().format("%Y-%m-%d %H:%M:%S%.6f"),
        total_score
    );

    // Get the elapsed time as a duration
    //    let elapsed = timer.elapsed();

    // Format the duration and print it
    println!("Time taken: {} seconds", elapsed.as_secs_f64());
}

//use crate::sprite::sprite;
fn process_line(line: &str) -> u32 {
    let numbers = line
        .chars()
        .filter_map(|c| c.to_digit(10))
        .collect::<Vec<_>>();
    let mut number = 0;
    if numbers.len() > 0 {
        number = numbers[0] * 10 + numbers[numbers.len() - 1]
    };
    return number;
}
fn process_input(input: &str) -> u32 {
    input.lines().map(|s| process_line(s)).sum()
}

fn find_first_and_last_digits(line: &str) -> (u32, u32) {
    let mut first = 0;
    let mut first_found = false;
    let mut last = 0;
    for i in 0..line.len() {
        let part_line = &line[i..];
        match if part_line.starts_with("one") {
            Some(1)
        } else if part_line.starts_with("two") {
            Some(2)
        } else if part_line.starts_with("three") {
            Some(3)
        } else if part_line.starts_with("four") {
            Some(4)
        } else if part_line.starts_with("five") {
            Some(5)
        } else if part_line.starts_with("six") {
            Some(6)
        } else if part_line.starts_with("seven") {
            Some(7)
        } else if part_line.starts_with("eight") {
            Some(8)
        } else if part_line.starts_with("nine") {
            Some(9)
        } else {
            line.chars().nth(i).and_then(|c| c.to_digit(10))
        } {
            Some(digit) => {
                if !first_found {
                    first = digit;
                    first_found = true;
                }
                last = digit;
            }
            None => {}
        }
    }
    //println!("{}:  first = {}, last = {}", line, first, last);
    return (first, last);
}

fn process_input2(input: &str) -> u32 {
    input
        .lines()
        .map(|s| {
            let (first, last) = find_first_and_last_digits(s);
            first * 10 + last
        })
        .sum()
}

#[cfg(test)]
mod tests {
    //use std::f32::consts::E;

    use super::*;

    const EXAMPLE_INPUT: &str = "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet";

    const EXAMPLE_INPUT2: &str = "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen";

    #[test]
    fn test_process_input() {
        let result = process_input(EXAMPLE_INPUT);
        assert_eq!(result, 142);
    }

    #[test]
    fn test_process_line() {
        let result = process_line("1abc2");
        assert_eq!(result, 12);
        let result = process_line("pqr3stu8vwx");
        assert_eq!(result, 38);
        let result = process_line("a1b2c3d4e5f");
        assert_eq!(result, 15);
        let result = process_line("123");
        assert_eq!(result, 13);
        let result = process_line("treb7uchet");
        assert_eq!(result, 77);
        let result = process_line("none");
        assert_eq!(result, 0);
        let result = process_line(
            "
        ",
        );
        assert_eq!(result, 0);
    }

    #[test]
    fn test_process_input2() {
        let result = process_input2("three1abc2");
        assert_eq!(result, 32);
        let result = process_input2("1abc2three");
        assert_eq!(result, 13);
        let result = process_input2("1threeabcthree2");
        assert_eq!(result, 12);
        let result = process_input2("abtwonec2");
        assert_eq!(result, 22);
        let result = process_input2("1abc2threeghk");
        assert_eq!(result, 13);
        let result = process_input2("pqr3stu8vwx");
        assert_eq!(result, 38);
        let result = process_input2("a1b2c3d4e5f");
        assert_eq!(result, 15);
        let result = process_input2("123");
        assert_eq!(result, 13);
        let result = process_input2("treb7uchet");
        assert_eq!(result, 77);
        let result = process_input2("none");
        assert_eq!(result, 11);
        let result = process_input2("nont");
        assert_eq!(result, 0);
        let result = process_input2(
            "
        ",
        );
        assert_eq!(result, 0);
        let result = process_input2(EXAMPLE_INPUT2);
        assert_eq!(result, 281);
    }
}
