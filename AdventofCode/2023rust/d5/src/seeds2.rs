#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Range {
    start: u64,
    length: u64,
}

impl Range {
    fn new(start: u64, length: u64) -> Range {
        Range { start, length }
    }
    fn end(&self) -> u64 {
        self.start + self.length
    }
    fn contains(&self, value: u64) -> bool {
        value >= self.start && value < self.end()
    }
    fn contains_range(&self, other: &Range) -> bool {
        self.contains(other.start) && self.contains(other.end())
    }
    fn overlaps(&self, other: &Range) -> bool {
        self.contains(other.start) || self.contains(other.end())
    }
    fn merge(&self, other: &Range) -> Range {
        assert!(self.overlaps(other));
        let start = std::cmp::min(self.start, other.start);
        let end = std::cmp::max(self.end(), other.end());
        Range::new(start, end - start)
    }
    fn intersection(&self, other: &Range) -> Option<Range> {
        if self.overlaps(other) {
            let start = std::cmp::max(self.start, other.start);
            let end = std::cmp::min(self.end(), other.end());
            Some(Range::new(start, end - start))
        } else {
            None
        }
    }
}

struct Rule {
    source_start: u64,
    dest_start: u64,
    length: u64,
}

impl Rule {
    fn source_range(&self) -> Range {
        Range::new(self.source_start, self.length)
    }
    fn dest_range(&self) -> Range {
        Range::new(self.dest_start, self.length)
    }
    fn delta(&self) -> i64 {
        self.dest_start as i64 - self.source_start as i64
    }
    fn apply(&self, source_range: &Range) -> Option<Range> {
        match source_range.intersection(&self.source_range()) {
            Some(intersection) => Some(Range::new(
                intersection.start as u64 + self.delta() as u64,
                intersection.length,
            )),
            None => None,
        }
    }
    fn apply_to_vec(&self, source_ranges: &Vec<Range>) -> Vec<Range> {
        source_ranges
            .iter()
            .map(|source_range| self.apply(source_range))
            .filter_map(|x| x)
            .collect::<Vec<Range>>()
    }
}

struct Map {
    source_type: String,
    dest_type: String,
    rules: Vec<Rule>,
}

pub struct Almanac2 {
    seeds: Vec<Range>,
    maps: Vec<Map>,
    current_type: String,
    current_values: Vec<Range>,
}

impl Almanac2 {
    pub fn from_string(input: &str) -> Almanac2 {
        let mut lines = input.lines();
        let seed_values = lines
            .next()
            .unwrap()
            .split(": ")
            .nth(1)
            .unwrap()
            .split(" ")
            .map(|s| s.parse::<u64>().unwrap())
            .collect::<Vec<u64>>();
        // take consecutive pairs of values and make ranges
        let mut seeds = Vec::new();
        let mut current_seed: Option<Range> = None;
        for value in seed_values {
            match current_seed {
                Some(mut seed) => {
                    seed.length = value;
                    seeds.push(seed);
                    current_seed = None;
                }
                None => {
                    current_seed = Some(Range::new(value, 1));
                }
            }
        }
        let mut maps = Vec::new();
        let mut current_map = None;
        for line in lines {
            if line.ends_with(" map:") {
                let source_type = line.split("-to-").nth(0).unwrap().to_string();
                let dest_type = line
                    .split("-to-")
                    .nth(1)
                    .unwrap()
                    .split(" ")
                    .nth(0)
                    .unwrap()
                    .to_string();
                current_map = Some(Map {
                    source_type,
                    dest_type,
                    rules: Vec::new(),
                });
            } else if line.len() > 0 {
                let rule = line
                    .split(" ")
                    .map(|s| s.parse::<u64>().unwrap())
                    .collect::<Vec<u64>>();
                current_map.as_mut().unwrap().rules.push(Rule {
                    dest_start: rule[0],
                    source_start: rule[1],
                    length: rule[2],
                });
            } else {
                match current_map {
                    Some(_) => {
                        maps.push(current_map.unwrap());
                    }
                    None => {}
                }
                current_map = None;
            }
        }
        match current_map {
            Some(_) => {
                maps.push(current_map.unwrap());
            }
            None => {}
        }

        let current_values = seeds.clone();
        Almanac2 {
            seeds,
            maps,
            current_type: "seed".to_string(),
            current_values,
        }
    }
    fn apply_next_map(&mut self) -> bool {
        println!("current values: {:?}", self.current_values);
        match self
            .maps
            .iter()
            .find(|m| m.source_type == self.current_type)
        {
            Some(map) => {
                println!("applying map: {:?}", map.source_type);
                self.current_values = map
                    .rules
                    .iter()
                    .map(|rule| rule.apply_to_vec(&self.current_values))
                    .flatten()
                    .collect::<Vec<Range>>();
                self.current_type = map.dest_type.clone();
                true
            }
            None => false,
        }
    }
    fn apply_maps(&mut self) {
        while self.apply_next_map() {}
    }
    pub fn minimal_location(&mut self) -> u64 {
        self.apply_maps();
        assert_eq!(self.current_type, "location");
        self.current_values
            .iter()
            .map(|r| r.start)
            .min()
            .unwrap()
            .clone()
    }
}
