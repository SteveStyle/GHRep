use crate::utils::Timer;
use chrono::Local;

const INPUT: &'static str = include_str!("input.txt");

pub(crate) mod game;
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
    match game::Game::from_str(line).and_then(|g| Some(g.game_value())) {
        Some(game_value) => game_value,
        None => 0,
    }
}
fn process_input(input: &str) -> u32 {
    input.lines().map(|s| process_line(s)).sum()
}

fn process_input2(input: &str) -> u32 {
    input.lines().map(|s| process_line2(s)).sum()
}
fn process_line2(line: &str) -> u32 {
    match game::Game::from_str(line).and_then(|g| {
        let (r, g, b) = g.max_counts();
        Some(r * g * b)
    }) {
        Some(game_value) => game_value,
        None => 0,
    }
}

#[cfg(test)]
mod tests {
    //use std::f32::consts::E;

    use super::*;

    const EXAMPLE_INPUT: &str = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";

    const EXAMPLE_INPUT2: &str = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";

    #[test]
    fn test_process_input() {
        let result = process_input(EXAMPLE_INPUT);
        assert_eq!(result, 8);
    }

    #[test]
    fn test_process_line() {}

    #[test]
    fn test_process_input2() {
        let result = process_input2(EXAMPLE_INPUT2);
        assert_eq!(result, 2286);
    }
}
