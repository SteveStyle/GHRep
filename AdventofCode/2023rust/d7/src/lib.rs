use crate::utils::Timer;
use chrono::Local;

const INPUT: &'static str = include_str!("input.txt");

mod camelcard;
mod camelcard2;
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
    camelcard::Game::from_string(input).count_product() as u32
}

fn process_input2(input: &str) -> u32 {
    camelcard2::Game::from_string(input).count_product() as u32
}

#[cfg(test)]
mod tests {

    use super::*;

    const EXAMPLE_INPUT: &str = "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483";

    #[test]
    fn test_process_input() {
        let result = process_input(EXAMPLE_INPUT);
        assert_eq!(result, 6440);
    }

    #[test]
    fn test_process_input2() {
        let result = process_input2(EXAMPLE_INPUT);
        assert_eq!(result, 5905);
    }
}
