struct Rule {
    source_start: u64,
    dest_start: u64,
    length: u64,
}

struct Map {
    source_type: String,
    dest_type: String,
    rules: Vec<Rule>,
}

pub struct Almanac {
    seeds: Vec<u64>,
    maps: Vec<Map>,
    current_type: String,
    current_values: Vec<u64>,
}

impl Almanac {
    pub fn from_string(input: &str) -> Almanac {
        let mut lines = input.lines();
        let seeds = lines
            .next()
            .unwrap()
            .split(": ")
            .nth(1)
            .unwrap()
            .split(" ")
            .map(|s| s.parse::<u64>().unwrap())
            .collect::<Vec<u64>>();
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
        Almanac {
            seeds,
            maps,
            current_type: "seed".to_string(),
            current_values,
        }
    }
    fn apply_next_map(&mut self) -> bool {
        match self
            .maps
            .iter()
            .find(|m| m.source_type == self.current_type)
        {
            Some(map) => {
                for value in self.current_values.iter_mut() {
                    for rule in map.rules.iter() {
                        if *value >= rule.source_start && *value < rule.source_start + rule.length {
                            let new_value = ((*value - rule.source_start) + rule.dest_start) as u64;
                            if new_value > 2u64.pow(32) {
                                panic!(
                                    "overflow: {} {} {} {}",
                                    new_value, *value, rule.source_start, rule.dest_start
                                );
                            }
                            *value = new_value as u64;
                            break;
                        }
                    }
                }
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
        self.current_values.iter().min().unwrap().clone()
    }
}
