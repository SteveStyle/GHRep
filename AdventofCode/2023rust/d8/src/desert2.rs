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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum NodeType {
    Start,
    End,
    Normal,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Node {
    left: usize,
    right: usize,
    node_type: NodeType,
}
pub struct Map {
    directions: Vec<Direction>,
    directions_as_usize: Vec<usize>,
    nodes: HashMap<String, [String; 2]>,
    node_to_index: HashMap<String, usize>,
    index_to_node: HashMap<usize, String>,
    nodes_by_index: Vec<Node>,
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

        let mut directions_as_usize = Vec::new();
        for direction in directions.iter() {
            directions_as_usize.push(*direction as usize);
        }

        let mut node_to_index = HashMap::new();
        let mut index_to_node = HashMap::new();
        for (index, node) in nodes.keys().enumerate() {
            node_to_index.insert(node.clone(), index);
            index_to_node.insert(index, node.clone());
        }

        let mut nodes_by_index = vec![
            Node {
                left: 0,
                right: 0,
                node_type: NodeType::Normal
            };
            nodes.len()
        ];
        for node in &nodes {
            let node_index = node_to_index[node.0];
            let node_left = node_to_index[&node.1[0]];
            let node_right = node_to_index[&node.1[1]];
            nodes_by_index[node_index] = Node {
                left: node_left,
                right: node_right,
                node_type: match &node.0.as_str()[2..3] {
                    "A" => NodeType::Start,
                    "Z" => NodeType::End,
                    _ => NodeType::Normal,
                },
            };
        }

        Map {
            directions,
            directions_as_usize,
            nodes,
            node_to_index,
            index_to_node,
            nodes_by_index,
        }
    }
    pub fn count_steps(&self) -> usize {
        let mut step: usize = 0;
        let mut nodes = self
            .nodes
            .keys()
            .filter(|k| k.ends_with("A"))
            .collect::<Vec<&String>>();

        println!("nodes: {:?}", nodes);
        loop {
            for node in nodes.iter_mut() {
                *node = &self.nodes[*node][self.directions[step % self.directions.len()] as usize];
            }
            println!("nodes: {:?}", nodes);
            step += 1;
            if nodes.iter().all(|n| n.ends_with("Z")) {
                break;
            }
        }

        step
    }
    pub fn count_steps_old(&self) -> usize {
        let mut step: usize = 0;
        let mut nodes = self
            .nodes
            .keys()
            .filter(|k| k.ends_with("A"))
            .collect::<Vec<&String>>();

        println!("nodes: {:?}", nodes);
        loop {
            for node in nodes.iter_mut() {
                *node = &self.nodes[*node][self.directions[step % self.directions.len()] as usize];
            }
            println!("nodes: {:?}", nodes);
            step += 1;
            if nodes.iter().all(|n| n.ends_with("Z")) {
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
