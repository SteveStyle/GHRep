//use i32 as Counter;
pub type Counter = i32;

//use crate::utils::*;
use std::ops::Add;
use std::ops::Deref;
use std::ops::Sub;

use regex::Regex;

pub const ORE       : usize = 0;
pub const CLAY      : usize = 1;
pub const OBSIDIAN  : usize = 2;
pub const GEODE     : usize = 3;

#[derive(Debug, Clone, Copy, Eq)]
pub struct GeodeVector {
    pub v: [Counter;4],
}

impl Add for GeodeVector {
    type Output = GeodeVector;

    fn add(self, rhs: Self) -> Self::Output {
        GeodeVector { v: [  
            self[ORE]      + rhs[ORE],
            self[CLAY]     + rhs[CLAY],
            self[OBSIDIAN] + rhs[OBSIDIAN],
            self[GEODE]    + rhs[GEODE],
            ] }
    }
}

impl Sub for GeodeVector {
    type Output = GeodeVector;

    fn sub(self, rhs: Self) -> Self::Output {
        GeodeVector { v: [  
            self[ORE]      - rhs[ORE],
            self[CLAY]     - rhs[CLAY],
            self[OBSIDIAN] - rhs[OBSIDIAN],
            self[GEODE]    - rhs[GEODE],
            ] }
    }
}

impl PartialEq for GeodeVector {
    fn eq(&self, other: &Self) -> bool {
        self[ORE]      == other[ORE]        &&
        self[CLAY]     == other[CLAY]       &&
        self[OBSIDIAN] == other[OBSIDIAN]   &&
        self[GEODE]    == other[GEODE]
    }
}

//impl Eq for GeodeVector { }

impl Deref for GeodeVector {
    type Target = [Counter;4];

    fn deref<'a>(&'a self) -> &'a Self::Target {
        &self.v
    }
}

impl GeodeVector {
    fn get(&self, idx: usize) -> Counter {
        self.v[idx]
    }

    fn new(ore: Counter, clay: Counter, obsidian: Counter, geode: Counter) -> GeodeVector {
        GeodeVector { v: [ore, clay, obsidian, geode] }
    }
}

#[derive(Debug)]
struct Blueprint {
    blueprint: Counter, 
    ore_ore: Counter, 
    clay_ore: Counter,
    obsidian_ore: Counter,
    obsidian_clay: Counter, 
    geode_ore: Counter, 
    geode_obsidian: Counter,
}
fn extract_info(input: &str) -> Vec<Blueprint> {
    //let re = Regex::new(r"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.").unwrap();

    let re = Regex::new(r"Blueprint (\d+):\s+Each ore robot costs (\d+) ore\.\s+Each clay robot costs (\d+) ore\.\s+Each obsidian robot costs (\d+) ore and (\d+) clay\.\s+Each geode robot costs (\d+) ore and (\d+) obsidian\.").unwrap();

    let mut results = Vec::new();

    for cap in re.captures_iter(input) {
        let blueprint = cap[1].parse::<Counter>().unwrap();
        let ore_ore = cap[2].parse::<Counter>().unwrap();
        let clay_ore = cap[3].parse::<Counter>().unwrap();
        let obsidian_ore = cap[4].parse::<Counter>().unwrap();
        let obsidian_clay = cap[5].parse::<Counter>().unwrap();
        let geode_ore = cap[6].parse::<Counter>().unwrap();
        let geode_obsidian = cap[7].parse::<Counter>().unwrap();
        results.push(  Blueprint{ blueprint, ore_ore, clay_ore, obsidian_ore, obsidian_clay, geode_ore, geode_obsidian } );
    }

    results
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_extract_info() {
        let results = crate::geode::extract_info(
            "Blueprint 1:
            Each ore robot costs 4 ore.
            Each clay robot costs 2 ore.
            Each obsidian robot costs 3 ore and 14 clay.
            Each geode robot costs 2 ore and 7 obsidian.
          
          Blueprint 2:
            Each ore robot costs 2 ore.
            Each clay robot costs 3 ore.
            Each obsidian robot costs 3 ore and 8 clay.
            Each geode robot costs 3 ore and 12 obsidian."
        );
        println!("{:#?}",results);
    }
}
