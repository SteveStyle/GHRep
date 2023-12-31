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
    next_nodes: [usize; 2],
    node_type: NodeType,
}

struct EndNode {
    node: usize,
    step: usize,
    base_iterations: usize,
    repeat_iterations: usize,
}
struct StartNode {
    node: usize,
    end_nodes: Vec<EndNode>,
}
pub struct Map {
    directions: Vec<Direction>,
    directions_as_usize: Vec<usize>,
    nodes: HashMap<String, [String; 2]>,
    node_to_index: HashMap<String, usize>,
    index_to_node: HashMap<usize, String>,
    nodes_by_index: Vec<Node>,
    start_nodes: Vec<StartNode>,
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
                next_nodes: [0, 0],
                node_type: NodeType::Normal
            };
            nodes.len()
        ];
        let mut start_nodes = Vec::new();
        for node in &nodes {
            let node_index = node_to_index[node.0];
            let node_left = node_to_index[&node.1[0]];
            let node_right = node_to_index[&node.1[1]];
            let node_type = match &node.0.as_str()[2..3] {
                "A" => NodeType::Start,
                "Z" => NodeType::End,
                _ => NodeType::Normal,
            };
            nodes_by_index[node_index] = Node {
                next_nodes: [node_left, node_right],
                node_type,
            };
            if node_type == NodeType::Start {
                start_nodes.push(StartNode {
                    node: node_index,
                    end_nodes: Vec::new(),
                });
            }
        }

        let directions_length = directions.len();

        let end_nodes_found = Vec::new();
        for start_node in &mut start_nodes {
            let mut node = start_node.node;
            let mut step = 0;
            loop {
                node =
                    nodes_by_index[node].next_nodes[directions[step % directions_length] as usize];

                step += 1;

                if nodes_by_index[node].node_type == NodeType::End {
                    if end_nodes_found.contains(&(node, step % directions_length)) {
                    } else {
                    }
                    end_nodes_found.push((node, step % directions_length));

                    start_node.end_nodes.push(EndNode {
                        node,
                        step: step % directions_length,
                        base_iterations: step / directions_length,
                        repeat_iterations: 0,
                    });
                }
            }
        }

        Map {
            directions,
            directions_as_usize,
            nodes,
            node_to_index,
            index_to_node,
            nodes_by_index,
            start_nodes,
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
