use std::sync::Arc;

struct Race {
    duration: f64,
    record: f64,
}

impl Race {
    fn new(duration: f64, record: f64) -> Self {
        Self { duration, record }
    }
    fn count(&self) -> u32 {
        let n_plus =
            (self.duration + f64::sqrt(self.duration * self.duration - 4.0 * self.record)) / 2.0;
        let n_minus =
            (self.duration - f64::sqrt(self.duration * self.duration - 4.0 * self.record)) / 2.0;
        (n_plus.ceil() as u32) - (n_minus.floor() as u32) - 1
    }
}

pub struct Races(Vec<Race>);

impl Races {
    pub fn from_string(input: &str) -> Races {
        let mut races = Races(Vec::new());
        let mut lines = input.lines();
        let durations = lines
            .next()
            .unwrap()
            .split_whitespace()
            .map(|s| s.parse::<f64>().ok())
            .filter_map(|x| x)
            .collect::<Vec<f64>>();
        let records = lines
            .next()
            .unwrap()
            .split_whitespace()
            .map(|s| s.parse::<f64>().ok())
            .filter_map(|x| x)
            .collect::<Vec<f64>>();
        assert_eq!(durations.len(), records.len());
        for (duration, record) in durations.iter().zip(records) {
            races.0.push(Race::new(*duration, record));
        }
        races
    }
    pub fn from_string2(input: &str) -> Races {
        let mut races = Races(Vec::new());
        let mut lines = input.lines();
        let duration = lines
            .next()
            .unwrap()
            .chars()
            .filter(|c| c.is_digit(10))
            .collect::<String>()
            .parse::<f64>()
            .unwrap();

        let record = lines
            .next()
            .unwrap()
            .chars()
            .filter(|c| c.is_digit(10))
            .collect::<String>()
            .parse::<f64>()
            .unwrap();

        races.0.push(Race::new(duration, record));

        races
    }
    pub fn count_product(&self) -> u32 {
        self.0.iter().map(|r| r.count()).product()
    }
}
mod test {

    use super::*;

    const EXAMPLE_INPUT: &str = "Time:      7  15   30
Distance:  9  40  200";

    #[test]
    fn test_race() {
        assert_eq!(Race::new(7.0, 9.0).count(), 4);
        assert_eq!(Race::new(15.0, 40.0).count(), 8);
        assert_eq!(Race::new(30.0, 200.0).count(), 9);
    }
}
