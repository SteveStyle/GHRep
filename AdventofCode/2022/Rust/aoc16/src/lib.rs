use std::collections::HashMap;
//use std::fmt::{Display, self};
use std::fs;
use std::hash::Hash;
use std::str::FromStr;
use regex::Regex;
//extern crate lazy_static;
use lazy_static::lazy_static;


pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input16.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The part 1 total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}


fn get_numbers<T: FromStr>(source: &str) -> Vec<T>
where T::Err: std::fmt::Debug
/* add to toml
[dependencies]
regex = "1.3.9" 
*/
{
    // Use a regular expression to match the first sequence of digits in the string
    // Support negative and floating point numbers.
    let re = Regex::new(r"-?\d+(\.\d+)?").unwrap();
    let mut result: Vec<T> = vec![];
    for captures in re.captures_iter(source) {
        let digit_string = captures.get(0).unwrap().as_str();
        let number = digit_string.parse().unwrap();
        result.push(number);
    }
    return result;
}

#[derive(Debug,Clone)]
struct Node {
    name : String,
    flow : i32,
    neighbours : Vec<String>,
    distances : HashMap<String, ( i32, i32 )>
}

impl Node {
    fn new (line: &str) -> Node {
    // line format Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
        lazy_static! {
            static ref RE: Regex = Regex::new(r"Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)").unwrap();
        }

        let captures = RE.captures(line).unwrap();
        let name = captures[1].to_owned();
        let flow = captures[2].parse().unwrap();
        let neighbours: Vec<String> = captures[3].split(',').map(str::trim).map(str::to_owned).collect();
        let distances = HashMap::new();

        Node { name, flow, neighbours, distances }
    }
    fn get_distance(&self, other_node : &str) -> i32 {
        self.distances.get(other_node).unwrap().clone().0
    }
    fn set_distance(&mut self, other_node : &str, new_distance : i32 ) {
        //        self.distances.entry(other_node.to_string()).and_modify(|d| *d = new_distance);
                self.distances.insert(other_node.to_string(), ( new_distance, 0 ) );
            }
    fn set_distance_if_not_set(&mut self, other_node : &str, new_distance : i32  ) {
        self.distances.entry(other_node.to_string()).or_insert(( new_distance, 0) );
    }
}

#[derive(Debug,Clone)]
struct Graph {
    nodes: HashMap<String,Node>,
    min_distance : i32,
}

impl Graph {
    fn new(contents: &str) -> Graph {
        let mut nodes = HashMap::new();
        for line in contents.lines() {
            let node = Node::new(line);
            nodes.insert(node.name.clone(), node );
        }
        
        Graph { nodes, min_distance: 0 }
    }
    fn get_flow(&self, node_name : &str) -> i32 {
        return self.nodes.get(node_name).unwrap().flow;
    }
    fn calculate_distances(&mut self) {
        let other_nodes = self.nodes.clone();
        for this_node_name in other_nodes.keys() {
            let this_node = self.nodes.get_mut(this_node_name).unwrap();
            this_node.set_distance(this_node_name, 0);
            for distance in 0..other_nodes.len() {
                for distance_entry in &this_node.distances.clone() {
//                    if *distance_entry.1.0 == distance as i32 {
                    if this_node.get_distance(distance_entry.0) == distance as i32 {
                        let neighbours = other_nodes.get(distance_entry.0).unwrap().neighbours.clone();
                        for neighbour in &neighbours {
                            this_node.set_distance_if_not_set(neighbour, distance as i32 + 1);
                        }
                    }
                }
            }
        }
    }
    fn get_node_names(&self) -> Vec<String> {
        let node_names: Vec<String> = self.nodes.keys().map(|k| String::from(k)).collect();
        return node_names;
    }
    fn get_flows(&self) -> Vec<(String,i32)> {
        let mut result : Vec<(String,i32)> = vec![];
        for node_name in self.get_node_names() {
            let flow = self.get_flow(&node_name);
            result.push( ( String::from(node_name), flow ) );
        }
        return result.clone();
    }
    fn update_distance_flow(&mut self) {
        let node_names = self.get_node_names();
        let flows = self.get_flows();
        for this_node_name in node_names.clone() {
            let this_node = self.nodes.get_mut(&this_node_name).unwrap();
            for (other_node_name, flow) in flows.clone() {
                let distance = this_node.distances.get_mut(&other_node_name).unwrap();
                distance.1 = flow;
            }
        }
    }
    fn get_slim_graph(&self) -> Graph {
        let node_names = self.get_node_names();
        let mut nodes: HashMap<String, Node> = HashMap::new();
        let min_distance = 0;
        for node_name in node_names.clone() {
            let old_node = self.nodes.get(&node_name).unwrap();
            if node_name == "AA" || old_node.flow > 0 {
                let old_distances = old_node.distances.clone();
                let mut distances: HashMap<String, (i32,i32)> = HashMap::new();
                for distance_node_name in node_names.clone() {
                    if distance_node_name != node_name {
                        let old_distance = old_distances.get(&distance_node_name).unwrap().clone();
                        if old_distance.1 > 0 {
                            distances.insert(distance_node_name.clone(), old_distance );
                        }
                    }
                }
                let node = Node{ name: node_name.clone(), flow: old_node.flow.clone(), neighbours: old_node.neighbours.clone(), distances };
                nodes.insert(node_name.clone(), node);
            }
        }

        let result = Graph { nodes, min_distance };
        //println!("get_slim_graph: {:#?}",result);
        return result;
    }
    fn new_slim_graph(contents: &str) -> Graph {
        let mut graph = Graph::new(contents);
        graph.calculate_distances();
        graph.update_distance_flow();
        graph.get_slim_graph()
    }
    
    //solve logic for part 1
    fn get_best_path( &self, current_node_name: &str, previous_nodes : Vec<String>, time_left : i32, current_impact: i32, current_best : i32 ) -> Option<i32> {
        let node = self.nodes.get(current_node_name).unwrap();
        //let distances = node.distances;
        let mut impacts : Vec<(i32,i32,String)> = vec![];  // impact, time_operating, next node name
        let mut total_impact = 0;
        for ( distance_node_name, (distance, flow)) in &node.distances {
            if previous_nodes.contains(distance_node_name) { continue; }
            let time_operating = (time_left - distance - 1);
            if time_operating > 0 {
                let impact = time_operating*flow;
                total_impact += impact;
                impacts.push((impact,time_operating,distance_node_name.clone()));
            }
        }
        if total_impact + current_impact <= current_best {
            return None;
        }
        if impacts.len() == 0 {
            if current_impact > current_best { 
                return Some(current_impact);
            } else {
                return None;
            }
        }
        impacts.sort();
        impacts.reverse();
        let mut current_best = current_best;
        let mut better_solution_found = false;
        let mut best_path = vec![];
        for ( impact, time_operating, next_node_name ) in impacts {
            let mut previous_nodes = previous_nodes.clone();
            previous_nodes.push(String::from(current_node_name));
            let previous_nodes_copy = previous_nodes.clone();
            if let Some(value) = 
                self.get_best_path(&next_node_name, previous_nodes, 
                    time_operating , impact + current_impact, current_best) {
                        current_best = value;
                        better_solution_found = true;
                        best_path = previous_nodes_copy;
            }
        } 

        if better_solution_found {
            println!("new best score of {} with path {:?}",current_best, best_path );
            return Some(current_best);
        } 
        return None;

    }

    fn solve(&self) -> Option<i32> {
        return self.get_best_path("AA", vec![], 30, 0, 0);
    }

    //  solve logic for part 2
    fn get_best_path2( &self, me_history : History, el_history : History, current_impact: i32, current_best : i32 ) -> Option<i32> {
        //let distances = node.distances;
        let mut impacts : Vec<(i32,i32,String, String)> = vec![];  // impact, time_operating, next node name, "m" for me or "e" for elephant
        let mut total_impact = 0;
        //calculate impacts for me
        let me_node = self.nodes.get(&me_history.current_node_name).unwrap();
        for ( distance_node_name, (distance, flow)) in &me_node.distances {
            if me_history.previous_nodes.contains(distance_node_name) || el_history.previous_nodes.contains(distance_node_name) { continue; }
            let time_operating = me_history.time_left - distance - 1;
            if time_operating > 0 {
                let impact = time_operating*flow;
                total_impact += impact;
                impacts.push((impact,time_operating,distance_node_name.clone(),String::from("m")));
            }
        }
        //calculate impacts for elephant
        let el_node = self.nodes.get(&el_history.current_node_name).unwrap();
        for ( distance_node_name, (distance, flow)) in &el_node.distances {
            if me_history.previous_nodes.contains(distance_node_name) || el_history.previous_nodes.contains(distance_node_name) { continue; }
            let time_operating = el_history.time_left - distance - 1;
            if time_operating > 0 {
                let impact = time_operating*flow;
                total_impact += impact;
                impacts.push((impact,time_operating,distance_node_name.clone(),String::from("e")));
            }
        }

        let impacts2 = impacts.clone();
        let totals: HashMap<String,i32> = HashMap::new();
        let max_totals = impacts2.iter().fold(totals, |mut totals, (impact, _, node_name, _)| {
            let max_impact = totals.get(node_name).map_or(impact, |current_max| current_max.max(impact));
            totals.insert(node_name.to_string(), max_impact.clone());
            totals
        });
        total_impact = max_totals.values().fold(0,|acc,v| acc + v);

        
        if total_impact + current_impact <= current_best {
            return None;
        }
        if impacts.len() == 0 {
            if current_impact > current_best { 
                println!("better score {}, me {:?}, elepheant {:?}", current_impact, me_history, el_history);
                return Some(current_impact);
            } else {
                return None;
            }
        }
        impacts.sort();
        impacts.reverse();

        
        
        let mut current_best = current_best;
        let mut better_solution_found = false;
        for ( impact, time_operating, next_node_name, who ) in impacts {
            let mut me_history = me_history.clone();
            let mut el_history = el_history.clone();
            if who == "m" {
                //let mut me_previous_nodes = me_history.previous_nodes.clone();
                //let me_previous_nodes_copy = me_previous_nodes.clone();
                me_history.current_node_name = next_node_name.clone();
                me_history.time_left = time_operating;
                me_history.previous_nodes.push(next_node_name);
                //me_history.previous_nodes = me_previous_nodes;

                if let Some(value) = 
                    self.get_best_path2(  me_history, el_history, impact + current_impact, current_best) {
                            current_best = value;
                            better_solution_found = true;
                }
            } else {
                el_history.current_node_name = next_node_name.clone();
                el_history.time_left = time_operating;
                el_history.previous_nodes.push( String::from(next_node_name) );

                if let Some(value) = 
                    self.get_best_path2(  me_history, el_history, impact + current_impact, current_best) {
                            current_best = value;
                            better_solution_found = true;
                }
            }
        }

        if better_solution_found {
            return Some(current_best);
        } 
        return None;

    }

    fn solve2(&self) -> Option<i32> {
        return self.get_best_path2( History { who: String::from("m"), current_node_name: String::from("AA"), time_left: 26, previous_nodes: vec![String::from("AA")] }, 
                                    History { who: String::from("e"), current_node_name: String::from("AA"), time_left: 26, previous_nodes: vec![String::from("AA")] },  
                                    0, 0);
    }
}

#[derive(Clone,Debug)]
struct History {
    who : String,
    current_node_name : String,
    time_left : i32,
    previous_nodes : Vec<String>,
}


fn process_file_contents( contents: &str) -> i32 {
    let graph = Graph::new_slim_graph(contents);
    if let Some(total) = graph.solve() {
        return total;
    }
    return -1;
}


fn process_file_contents2( contents: &str) -> i32 {
    let graph = Graph::new_slim_graph(contents);
    if let Some(total) = graph.solve2() {
        return total;
    }
    return -1;
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn test_solve() {
        let graph = Graph::new_slim_graph("Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
        Valve BB has flow rate=13; tunnels lead to valves CC, AA
        Valve CC has flow rate=2; tunnels lead to valves DD, BB
        Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
        Valve EE has flow rate=3; tunnels lead to valves FF, DD
        Valve FF has flow rate=0; tunnels lead to valves EE, GG
        Valve GG has flow rate=0; tunnels lead to valves FF, HH
        Valve HH has flow rate=22; tunnel leads to valve GG
        Valve II has flow rate=0; tunnels lead to valves AA, JJ
        Valve JJ has flow rate=21; tunnel leads to valve II");
        let score = graph.solve();
        println!("{:#?}",score);
    }  
    #[test]
    fn test_solve2() {
        let graph = Graph::new_slim_graph("Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
        Valve BB has flow rate=13; tunnels lead to valves CC, AA
        Valve CC has flow rate=2; tunnels lead to valves DD, BB
        Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
        Valve EE has flow rate=3; tunnels lead to valves FF, DD
        Valve FF has flow rate=0; tunnels lead to valves EE, GG
        Valve GG has flow rate=0; tunnels lead to valves FF, HH
        Valve HH has flow rate=22; tunnel leads to valve GG
        Valve II has flow rate=0; tunnels lead to valves AA, JJ
        Valve JJ has flow rate=21; tunnel leads to valve II");
        let score = graph.solve2();
        println!("{:#?}",score);
    }  
 
    #[test]
    fn test_new_slim_graph() {
        let graph = Graph::new_slim_graph("Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
        Valve BB has flow rate=13; tunnels lead to valves CC, AA
        Valve CC has flow rate=2; tunnels lead to valves DD, BB
        Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
        Valve EE has flow rate=3; tunnels lead to valves FF, DD
        Valve FF has flow rate=0; tunnels lead to valves EE, GG
        Valve GG has flow rate=0; tunnels lead to valves FF, HH
        Valve HH has flow rate=22; tunnel leads to valve GG
        Valve II has flow rate=0; tunnels lead to valves AA, JJ
        Valve JJ has flow rate=21; tunnel leads to valve II");
        println!("{:#?}",graph);
    }  

    #[test]
    fn test_process_file_contents() {
        assert_eq!(process_file_contents("Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
        Valve BB has flow rate=13; tunnels lead to valves CC, AA
        Valve CC has flow rate=2; tunnels lead to valves DD, BB
        Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
        Valve EE has flow rate=3; tunnels lead to valves FF, DD
        Valve FF has flow rate=0; tunnels lead to valves EE, GG
        Valve GG has flow rate=0; tunnels lead to valves FF, HH
        Valve HH has flow rate=22; tunnel leads to valve GG
        Valve II has flow rate=0; tunnels lead to valves AA, JJ
        Valve JJ has flow rate=21; tunnel leads to valve II"), 1651);
    }

    #[test]
    fn test_process_file_contents2() {
        assert_eq!(process_file_contents2("Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
        Valve BB has flow rate=13; tunnels lead to valves CC, AA
        Valve CC has flow rate=2; tunnels lead to valves DD, BB
        Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
        Valve EE has flow rate=3; tunnels lead to valves FF, DD
        Valve FF has flow rate=0; tunnels lead to valves EE, GG
        Valve GG has flow rate=0; tunnels lead to valves FF, HH
        Valve HH has flow rate=22; tunnel leads to valve GG
        Valve II has flow rate=0; tunnels lead to valves AA, JJ
        Valve JJ has flow rate=21; tunnel leads to valve II"), 1707);
   }

}

