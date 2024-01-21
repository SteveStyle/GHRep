use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Display, Formatter},
    str::FromStr,
};

use crate::{
    modular2::{ComplexConstraint, ModularConstraint},
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct EndNode {
    node: usize,
    step: usize,
    base_iterations: i64,
    repeat_iterations: i64,
}

impl EndNode {
    fn add_to_constraint(&self, constraint: &mut ComplexConstraint) {
        if self.repeat_iterations == 0 {
            constraint
                .specific_value_constraints
                .push(self.base_iterations as i64);
        } else {
            constraint.modular_constraints.push(ModularConstraint::new(
                self.base_iterations as i64,
                self.repeat_iterations as i64,
            ));
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct StartNode {
    node: usize,
    end_nodes: Vec<EndNode>,
}

impl StartNode {
    fn constraint_for_step(&self, step: usize) -> ComplexConstraint {
        let mut constraint = ComplexConstraint::new();
        for end_node in &self.end_nodes {
            if end_node.step == step {
                end_node.add_to_constraint(&mut constraint);
            }
        }
        constraint
    }
    fn get_steps(&self) -> HashSet<usize> {
        let mut steps = HashSet::new();
        for end_node in &self.end_nodes {
            steps.insert(end_node.step);
        }
        steps
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
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

        for start_node in &mut start_nodes {
            let mut node = start_node.node;
            let mut step = 0;
            let mut base_iterations = 0;
            let mut repeat_iterations = 0;
            let mut first_repeated_end_node_index = 0;
            let mut end_nodes_found: HashMap<(usize, usize), usize> = HashMap::new(); // (node, step) -> end_node_index
            loop {
                node = nodes_by_index[node].next_nodes[directions_as_usize[step]];

                step += 1;
                if step == directions_length {
                    base_iterations += 1;
                    step = 0;
                }

                if nodes_by_index[node].node_type == NodeType::End {
                    match end_nodes_found.get(&(node, step)) {
                        Some(end_node_index) => {
                            first_repeated_end_node_index = *end_node_index;
                            repeat_iterations = base_iterations
                                - start_node.end_nodes[*end_node_index].base_iterations;
                            break;
                        }
                        None => {}
                    }
                    end_nodes_found.insert((node, step), start_node.end_nodes.len());

                    start_node.end_nodes.push(EndNode {
                        node,
                        step,
                        base_iterations,
                        repeat_iterations: 0,
                    });
                }
            }
            if repeat_iterations == 0 {
                panic!("No repeat iterations found");
            }
            for end_node_index in first_repeated_end_node_index..start_node.end_nodes.len() {
                start_node.end_nodes[end_node_index].repeat_iterations = repeat_iterations;
            }
        }

        let map = Map {
            directions,
            directions_as_usize,
            nodes,
            node_to_index,
            index_to_node,
            nodes_by_index,
            start_nodes,
        };

        map
    }

    pub fn count_steps(&self) -> usize {
        assert!(self.start_nodes.len() > 0);
        // find the steps which occur in all start nodes
        let mut steps = self.start_nodes[0].get_steps();
        println!(
            "the first start node covers {} steps: {:?}.",
            steps.len(),
            steps
        );

        for start_node in &self.start_nodes[1..] {
            steps = steps
                .intersection(&start_node.get_steps())
                .copied()
                .collect();
        }

        println!(
            "{} steps covered by all start nodes: {:?}",
            steps.len(),
            steps
        );

        let mut least_step = usize::MAX;
        for step in steps {
            // take the intersection of all constraints for this step
            for start_node in &self.start_nodes {
                println!(
                    "start node {} constraint for step {}: {}",
                    start_node.node,
                    step,
                    start_node.constraint_for_step(step)
                );
            }

            let mut constraint = self.start_nodes[0].constraint_for_step(step);
            for start_node in &self.start_nodes[1..] {
                constraint = constraint.intersection(&start_node.constraint_for_step(step));
            }
            let least_iterations = constraint.least_value();
            let least_steps = step as i64 + least_iterations * self.directions.len() as i64;
            least_step = least_step.min(least_steps as usize);
        }

        least_step
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

const EXAMPLE_INPUT3: &str = "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)";

mod tests {
    

    #[test]
    fn test_from_string() {
        let map = Map::from_string(EXAMPLE_INPUT3);
    }
}
