//use i32 as Counter;
pub type Counter = i32;

//use core::time;
//use crate::utils::*;
use std::ops::Add;
use std::ops::Deref;
use std::ops::Sub;

use regex::Regex;

//use chrono::Local;

pub const ORE       : usize = 0;
pub const CLAY      : usize = 1;
pub const OBSIDIAN  : usize = 2;
pub const GEODE     : usize = 3;

#[derive(Clone)]
enum Material {
    Ore = 0,
    Clay = 1,
    Obsidian = 2,
    Geode = 3,
}


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

use std::cmp::{PartialOrd, Ordering};

impl PartialOrd for GeodeVector {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if  self.v[ORE]         == other.v[ORE]        &&
            self.v[CLAY]        == other.v[CLAY]       &&
            self.v[OBSIDIAN]    == other.v[OBSIDIAN]   &&
            self.v[GEODE]       == other.v[GEODE] 
        { return Some(Ordering::Equal) }

        if  self.v[ORE]         >= other.v[ORE]        &&
            self.v[CLAY]        >= other.v[CLAY]       &&
            self.v[OBSIDIAN]    >= other.v[OBSIDIAN]   &&
            self.v[GEODE]       >= other.v[GEODE] 
        { return Some(Ordering::Greater) }

        if  self.v[ORE]         <= other.v[ORE]        &&
            self.v[CLAY]        <= other.v[CLAY]       &&
            self.v[OBSIDIAN]    <= other.v[OBSIDIAN]   &&
            self.v[GEODE]       <= other.v[GEODE] 
        { return Some(Ordering::Less) }

        return None;

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

#[derive(Debug,Copy,Clone)]
pub struct Blueprint {
    pub blueprint: Counter, 
    required: [GeodeVector;4],
    max: GeodeVector,
}

impl Blueprint {
    
    pub fn solve(&self, time_left : Counter) -> Counter {
        if let Some(result) = 
            self.rec_solve(
                GeodeVector::new(0,0,0,0),
                GeodeVector::new(1,0,0,0), 
                -10, 
                time_left,
                [false;4]
            ) 
        {
            return result;
        } else {
            return -1;
        }
    }
    
    fn rec_solve(&self, current_materials : GeodeVector, current_robots : GeodeVector, best_score : Counter, time_left: Counter, exclude : [bool;4]  ) -> Option<Counter> {
        if time_left == 0 { 
            let new_score = current_materials[GEODE]; 
            if new_score > best_score { 
//                println!("{}:  new best score {}",Local::now().format("%Y-%m-%d %H:%M:%S%.6f"), new_score);
//                println!("{}:  {:?}",Local::now().format("%Y-%m-%d %H:%M:%S%.6f"), history);
                return Some(new_score); 
            } else { 
                return None; 
            }
        }

        if 2* current_materials[GEODE] + (time_left + 1) * (2 * current_robots[GEODE] + time_left) <= 2 * best_score { return None; }

        let mut new_best_score = best_score;

        let mut new_exclude = exclude.clone();

        for i in (ORE..GEODE+1).rev() {        
            if !exclude[i] && ( i == GEODE || current_robots[i] < self.max[i] ) && current_materials >= self.required[i]  {
                let new_materials = current_materials + current_robots - self.required[i];
                let mut new_robots = current_robots;
                new_robots.v[i] +=1;
                /*let mut new_history = history.clone();
                new_history.push( char::from_digit(i as u32, 10).unwrap() );
                */
                if let Some(score) = self.rec_solve( new_materials, new_robots, new_best_score, time_left - 1, [false;4] ) {
                    new_best_score = score;
                };
                new_exclude[i] = true;
            }
        }

        let new_materials = current_materials + current_robots;
/*        let mut new_history = history.clone();
        new_history.push(' ');
*/
        if let Some(score) = self.rec_solve( new_materials, current_robots, new_best_score, time_left - 1, new_exclude ) {
            new_best_score = score;
        };

        
        if new_best_score > best_score { 
            Some(new_best_score)
        } else {
            None
        }
    }
}

pub fn extract_info(input: &str) -> Vec<Blueprint> {
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
        results.push(  Blueprint{ blueprint, 
                                  required : [  GeodeVector { v: [ore_ore,      0,              0,              0] },
                                                GeodeVector { v: [clay_ore,     0,              0,              0] },
                                                GeodeVector { v: [obsidian_ore, obsidian_clay,  0,              0] },
                                                GeodeVector { v: [geode_ore,    0,              geode_obsidian, 0] } 
                                        ] ,
                                    max : GeodeVector::new(clay_ore.max(obsidian_ore.max(geode_ore)), obsidian_clay, geode_obsidian, 25),
                                } 
                    );
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
    #[test]
    fn test_solve() {
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
        for i in 0..results.len() {
            println!("The best for blueprint {} is {:#?}",results[i].blueprint,results[i].solve(24));
        }
    }
}
