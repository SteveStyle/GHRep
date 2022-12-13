// this is a library
use std::fs;
//use std::num::integer::lcm;

pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input13.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The part 1 total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}

#[derive(Clone)]
enum Packet {
    Int( usize ),
    List( Vec<Packet> ),
}

use std::cmp::Ordering;

impl Packet {
    fn eq_list_int(v : &Vec<Packet>, n : &usize) -> bool {
        if v.len() == 1 {
            match v[0] {
                Self::Int(m) => return m==*n,
                Self::List(_) => return false,
            }
        }
        return false;
    }
}

impl PartialEq for Packet {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::List(l0), Self::List(r0)) if l0.len() == r0.len() => {
                let len = l0.len();
                for i in 0..len {
                    if !(l0[i] == r0[i]) { return false;}
                }
                return true;
            }
            (Self::List(l0), Self::List(r0)) => false,
            (Self::Int(l0), Self::List(r0)) => Self::eq_list_int(r0, l0),
            (Self::List(l0), Self::Int(r0)) => Self::eq_list_int(l0, r0),
        }
    }
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => l0.partial_cmp(r0),
            (Self::List(l0), Self::List(r0)) if l0.len() < r0.len() => {
                let len = l0.len();
                for i in 0..len {
                    let c = l0[i].partial_cmp(&r0[i]).unwrap();
                    if !(c==Ordering::Equal) {return Some(c)};
                }
                return Some(Ordering::Less);
            }
            (Self::List(l0), Self::List(r0)) if l0.len() > r0.len() => {
                let len = r0.len();
                for i in 0..len {
                    let c = l0[i].partial_cmp(&r0[i]).unwrap();
                    if !(c==Ordering::Equal) {return Some(c)};
                }
                return Some(Ordering::Greater);
            }
            (Self::List(l0), Self::List(r0)) => { //lengths are the same
                let len = l0.len();
                for i in 0..len {
                    let c = l0[i].partial_cmp(&r0[i]).unwrap();
                    if !(c==Ordering::Equal) {return Some(c)};
                }
                return Some(Ordering::Equal);
            }
            (Self::Int(l0), Self::List(r0)) => Self::partial_cmp(&Self::List(vec![self.clone()]), self),
            (Self::List(l0), Self::Int(r0)) => Self::partial_cmp(self, &Self::List(vec![other.clone()])),
        }
    }

}

impl Packet {
    fn new(contents: &str) {
        let pairs = contents.split("/n/n");
        let len = pairs.len();
        for i in 0..len {
            let lines = pairs[i].lines();
            
        }
    }
}


fn process_file_contents( contents: &str) -> usize {
    let  total = 0;
    return total;
}


fn process_file_contents2( contents: &str) -> usize {
    let total = 0;
    return total;
}



#[cfg(test)]
mod tests {
    use super::*;
   
    
    #[test]
    fn test_process_file_contents() {
        assert_eq!(process_file_contents("[1,1,3,1,1]
        [1,1,5,1,1]
        
        [[1],[2,3,4]]
        [[1],4]
        
        [9]
        [[8,7,6]]
        
        [[4,4],4,4]
        [[4,4],4,4,4]
        
        [7,7,7,7]
        [7,7,7]
        
        []
        [3]
        
        [[[]]]
        [[]]
        
        [1,[2,[3,[4,[5,6,7]]]],8,9]
        [1,[2,[3,[4,[5,6,0]]]],8,9]"), 13);
    }

    #[test]
    fn test_process_file_contents2() {
        assert_eq!(process_file_contents2("[1,1,3,1,1]
        [1,1,5,1,1]
        
        [[1],[2,3,4]]
        [[1],4]
        
        [9]
        [[8,7,6]]
        
        [[4,4],4,4]
        [[4,4],4,4,4]
        
        [7,7,7,7]
        [7,7,7]
        
        []
        [3]
        
        [[[]]]
        [[]]
        
        [1,[2,[3,[4,[5,6,7]]]],8,9]
        [1,[2,[3,[4,[5,6,0]]]],8,9]"), 0);
    }

}

