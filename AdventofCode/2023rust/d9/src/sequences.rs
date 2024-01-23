pub struct Sequence {
    values: Vec<i64>,
}

impl Sequence {
    pub fn new() -> Sequence {
        Sequence { values: vec![] }
    }

    fn is_zeroes(&self) -> bool {
        for value in &self.values {
            if *value != 0 {
                return false;
            }
        }
        true
    }

    fn differences(&self) -> Sequence {
        let mut result = Sequence::new();
        let mut index = 0;
        while index < self.values.len() - 1 {
            result
                .values
                .push(self.values[index + 1] - self.values[index]);
            index += 1;
        }
        result
    }

    fn next_value(&self) -> i64 {
        if self.is_zeroes() || self.values.len() == 0 {
            return 0;
        }
        self.values[self.values.len() - 1] + self.differences().next_value()
    }

    fn previous_value(&self) -> i64 {
        if self.is_zeroes() || self.values.len() == 0 {
            return 0;
        }
        self.values[0] - self.differences().previous_value()
    }
}

pub struct Sequences {
    sequences: Vec<Sequence>,
}

impl Sequences {
    pub fn new() -> Sequences {
        Sequences { sequences: vec![] }
    }

    pub fn from_string(input: &str) -> Sequences {
        let mut sequences = Sequences::new();
        for line in input.lines() {
            if line.is_empty() {
                continue;
            }
            let mut sequence = Sequence::new();
            for number in line.split_whitespace() {
                if let Ok(number) = number.parse::<i64>() {
                    sequence.values.push(number);
                }
            }
            sequences.sequences.push(sequence);
        }
        sequences
    }

    pub fn sum_next_values(&self) -> i64 {
        let mut result = 0;
        for sequence in &self.sequences {
            result += sequence.next_value();
        }
        result
    }

    pub fn sum_previous_values(&self) -> i64 {
        let mut result = 0;
        for sequence in &self.sequences {
            result += sequence.previous_value();
        }
        result
    }
}

#[allow(dead_code)]
const EXAMPLE_INPUT: &str = "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45";
