use crate::utils::Timer;
use chrono::Local;

const INPUT: &'static str = include_str!("input.txt");

mod races;
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

fn process_input(input: &str) -> u32 {
    races::Races::from_string(input).count_product() as u32
}

fn process_input2(input: &str) -> u32 {
    races::Races::from_string2(input).count_product() as u32
}

#[cfg(test)]
mod tests {

    use super::*;

    const EXAMPLE_INPUT: &str = "Time:      7  15   30
Distance:  9  40  200";

    #[test]
    fn test_process_input() {
        let result = process_input(EXAMPLE_INPUT);
        assert_eq!(result, 288);
    }

    #[test]
    fn test_process_input2() {
        let result = process_input2(EXAMPLE_INPUT);
        assert_eq!(result, 71503);
    }
}
