use std::{
    fmt::{Display, Formatter},
    ops::Add,
};

struct VecRange(Vec<Range>);
impl Display for VecRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0.iter().map(|r| format!("{} ", r)).collect::<String>()
        )
    }
}
impl VecRange {
    fn new(ranges: &Vec<Range>) -> VecRange {
        VecRange(ranges.clone())
    }
}
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Range {
    start: i64,
    length: i64,
}

impl Display for Range {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{} {}", self.start, self.end(), self.length)
    }
}

impl Add<i64> for Range {
    type Output = Range;
    fn add(self, other: i64) -> Range {
        Range::new(self.start + other, self.length)
    }
}

impl Range {
    fn new(start: i64, length: i64) -> Range {
        Range { start, length }
    }
    fn end(&self) -> i64 {
        self.start + self.length
    }
    fn without(&self, other: &Range) -> Vec<Range> {
        if self.overlaps(other) {
            let mut result = Vec::new();
            if other.start > self.start {
                result.push(Range::new(self.start, other.start - self.start));
            }
            if other.end() < self.end() {
                result.push(Range::new(other.end(), self.end() - other.end()));
            }
            result
        } else {
            vec![*self]
        }
    }
    fn contains(&self, value: i64) -> bool {
        value >= self.start && value < self.end()
    }
    fn contains_range(&self, other: &Range) -> bool {
        other.start >= self.start && other.end() <= self.end()
    }
    fn overlaps(&self, other: &Range) -> bool {
        other.start < self.end() && other.end() > self.start
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

#[derive(Debug)]
struct Rule {
    source_start: i64,
    dest_start: i64,
    length: i64,
}

impl Display for Rule {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}-{} {}-{} {} {}",
            self.source_start,
            self.source_start + self.length,
            self.dest_start,
            self.dest_start + self.length,
            self.length,
            self.delta()
        )
    }
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
    fn apply(
        &self,
        mut source_ranges: Vec<Range>,
        mut output_ranges: Vec<Range>,
        source_range: Range,
    ) -> (Vec<Range>, Vec<Range>) {
        source_ranges.append(&mut source_range.without(&self.source_range()));

        match source_range.intersection(&self.source_range()) {
            Some(intersection) => output_ranges.push(intersection + self.delta()),
            None => {}
        }
        (source_ranges, output_ranges)
    }
    fn apply_to_vec(
        &self,
        source_ranges: Vec<Range>,
        mut output_ranges: Vec<Range>,
    ) -> (Vec<Range>, Vec<Range>) {
        source_ranges.into_iter().fold(
            (Vec::new(), output_ranges),
            |(mut source_ranges, mut output_ranges), source_range| {
                self.apply(source_ranges, output_ranges, source_range)
            },
        )
    }
}

#[derive(Debug)]
struct Map {
    source_type: String,
    dest_type: String,
    rules: Vec<Rule>,
}

impl Display for Map {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}-to-{} map:\n{}",
            self.source_type,
            self.dest_type,
            self.rules
                .iter()
                .map(|rule| format!("{}\n", rule))
                .collect::<String>()
        )
    }
}

#[derive(Debug)]
pub struct Almanac2 {
    seeds: Vec<Range>,
    maps: Vec<Map>,
    current_type: String,
    current_values: Vec<Range>,
}

impl Display for Almanac2 {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "seeds: {}\n\n{}",
            self.seeds
                .iter()
                .map(|s| format!("{} ", s))
                .collect::<String>(),
            self.maps
                .iter()
                .map(|m| format!("{}\n", m))
                .collect::<String>()
        )
    }
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
            .map(|s| s.parse::<i64>().unwrap())
            .collect::<Vec<i64>>();
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
                    .map(|s| s.parse::<i64>().unwrap())
                    .collect::<Vec<i64>>();
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
        println!("current values: {}", VecRange::new(&self.current_values));
        match self
            .maps
            .iter()
            .find(|m| m.source_type == self.current_type)
        {
            Some(map) => {
                println!("applying map: {}", map.source_type);
                let (source_ranges, mut output_ranges) = map.rules.iter().fold(
                    (self.current_values.clone(), Vec::new()),
                    |(mut source_ranges, mut output_ranges), rule| {
                        rule.apply_to_vec(source_ranges, output_ranges)
                    },
                );
                self.current_values = source_ranges;
                self.current_values.append(&mut output_ranges);

                self.current_type = map.dest_type.clone();
                true
            }
            None => false,
        }
    }
    fn apply_maps(&mut self) {
        while self.apply_next_map() {}
    }
    pub fn minimal_location(&mut self) -> i64 {
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

mod tests {
    use super::*;

    const EXAMPLE_INPUT: &str = "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4";

    fn show_intersection(r1: &Range, r2: &Range) {
        println!(
            "r1 {} {}   r2 {} {}",
            r1.start,
            r1.end(),
            r2.start,
            r2.end()
        );
        println!("r1.overlaps(&r2) {}", r1.overlaps(&r2));
        match r1.intersection(&r2) {
            Some(intersection) => {
                println!("intersection {} {}", intersection.start, intersection.end());
            }
            None => {}
        }
        println!("");
    }

    #[test]
    fn test_range() {
        let s1 = Range::new(79, 14);
        let s2 = Range::new(55, 13);
        let ss1 = Range::new(98, 2);
        let ss2 = Range::new(50, 48);
        let sf1 = Range::new(15, 37);
        let sf2 = Range::new(52, 2);
        let sf3 = Range::new(0, 15);
        let fw1 = Range::new(53, 8);
        let fw2 = Range::new(11, 42);
        let fw3 = Range::new(0, 7);
        let fw4 = Range::new(7, 4);
        let wl1 = Range::new(18, 7);
        let wl2 = Range::new(25, 70);
        let lt1 = Range::new(77, 23);
        let lt2 = Range::new(45, 19);
        let lt3 = Range::new(64, 13);
        let th1 = Range::new(69, 1);
        let th2 = Range::new(0, 69);
        let hl1 = Range::new(56, 37);
        let hl2 = Range::new(93, 4);
        assert_eq!(s1.end(), 93);
        show_intersection(&s1, &ss1);
        show_intersection(&s1, &ss2);
        show_intersection(&s2, &ss1);
        show_intersection(&s2, &ss2);
    }

    #[test]
    fn test_intersects() {
        let r1 = Range::new(5, 10);
        let r2 = Range::new(5, 1);
        let r11 = Range::new(4, 1);
        let r12 = Range::new(4, 2);
        let r13 = Range::new(4, 3);
        let r14 = Range::new(4, 10);
        let r15 = Range::new(4, 11);
        let r16 = Range::new(4, 12);
        let r21 = Range::new(5, 1);
        let r22 = Range::new(5, 2);
        let r24 = Range::new(5, 9);
        let r25 = Range::new(5, 10);
        let r26 = Range::new(5, 11);
        let r34 = Range::new(6, 8);
        let r35 = Range::new(6, 9);
        let r36 = Range::new(6, 10);
        let r41 = Range::new(14, 1);
        let r42 = Range::new(14, 2);
        let r43 = Range::new(14, 3);
        let r51 = Range::new(15, 1);
        let r52 = Range::new(15, 2);
        assert!(r1.intersection(&r11).is_none());
        assert!(r1.intersection(&r12).is_some());
        assert!(r1.intersection(&r13).is_some());
        assert!(r1.intersection(&r14).is_some());
        assert!(r1.intersection(&r15).is_some());
        assert!(r1.intersection(&r16).is_some());
        println!("r1: {:?}  r1.end() {}", r1, r1.end());
        println!("r21: {:?}  r21.end() {}", r21, r21.end());
        println!("r1.overlaps(&r21) {}", r1.overlaps(&r21));
        println!("r1.intersection(&r21) {:?}", r1.intersection(&r21));
        assert!(r1.intersection(&r21).is_some());
        assert!(r1.intersection(&r22).is_some());
        assert!(r1.intersection(&r24).is_some());
        assert!(r1.intersection(&r25).is_some());
        assert!(r1.intersection(&r26).is_some());
        assert!(r1.intersection(&r34).is_some());
        assert!(r1.intersection(&r35).is_some());
        assert!(r1.intersection(&r36).is_some());
        assert!(r1.intersection(&r41).is_some());
        assert!(r1.intersection(&r42).is_some());
        assert!(r1.intersection(&r43).is_some());
        assert!(r1.intersection(&r51).is_none());
        assert!(r1.intersection(&r52).is_none());
    }
    #[test]
    fn test_process_input2() {
        let mut almanac = Almanac2::from_string(EXAMPLE_INPUT);
        println!("almanac: {}", almanac);
        let result = almanac.minimal_location();
        assert_eq!(result, 46);
    }
}
