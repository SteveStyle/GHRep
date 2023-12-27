use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
    str::FromStr,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    Left = 0,
    Right,
}

impl Display for Direction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Direction::Left => write!(f, "L"),
            Direction::Right => write!(f, "R"),
        }
    }
}

impl FromStr for Direction {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "L" => Ok(Direction::Left),
            "R" => Ok(Direction::Right),
            _ => Err(format!("Invalid direction: {}", s)),
        }
    }
}

impl Direction {
    fn from_char(c: char) -> Result<Self, String> {
        match c {
            'L' => Ok(Direction::Left),
            'R' => Ok(Direction::Right),
            _ => Err(format!("Invalid direction: {}", c)),
        }
    }
}

pub struct Map {
    directions: Vec<Direction>,
    nodes: HashMap<String, [String; 2]>,
}

impl Map {
    pub fn from_string(input: &str) -> Self {
        let mut nodes = HashMap::new();
        let parts = input.split("\n\n").collect::<Vec<&str>>();
        let directions = parts[0]
            .chars()
            .map(|c| Direction::from_char(c).ok())
            .filter_map(|x| x)
            .collect::<Vec<Direction>>();
        for line in parts[1].lines() {
            assert_eq!(line.len(), 16);
            nodes.insert(
                line[0..3].to_string(),
                [line[7..10].to_string(), line[12..15].to_string()],
            );
        }

        Map { directions, nodes }
    }
    pub fn count_steps(&self) -> usize {
        let mut step: usize = 0;
        let mut node = "AAA";
        loop {
            node = &self.nodes[node][self.directions[step % self.directions.len()] as usize];
            step += 1;
            if node == "ZZZ" {
                break;
            }
        }
        step
    }
}

const EXAMPLE_INPUT: &str = "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)";

const EXAMPLE_INPUT2: &str = "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)";
