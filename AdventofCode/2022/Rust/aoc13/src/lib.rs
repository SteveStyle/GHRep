use std::fmt::Display;
use std::fs;

pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input13.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The part 1 total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}

#[derive(Clone,Debug)]
enum Packet {
    Int( usize ),
    List( Vec<Packet> ),
}

impl Display for Packet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Packet::Int(n) =>   { write!(f, "{}", n) },
            Packet::List(v) =>   { write!(f, "[{}]", v.iter().map(|p| format!("{}",p)).collect::<Vec<String>>().join(", ") ) }, //fold(String::from(""),|acc, x| acc + &x)) },
            
        }
    }
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
        println!("eq: {} vs {}",&self,&other);
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

impl Eq for Packet { }


impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        println!("cmp: {} vs {}",&self,&other);
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
            (Self::Int(l0), Self::List(r0)) => Self::partial_cmp(&Self::List(vec![self.clone()]), other),
            (Self::List(l0), Self::Int(r0)) => Self::partial_cmp(self, &Self::List(vec![other.clone()])),
        }
    }

}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}


impl Packet {
    fn new_chars(chars: &mut Vec<char>) -> Option<Packet> {
        // new_chars() takes a string as a vector of chars.  The order is revered so we read the first character by popping.
        // It's purpose is to read and return the first Packet, removing it from chars.
        // If we are at the end of a list we return None and remove the ].
        // The function calls itself recursively when it encounters the start of a list, indicated by [.
        // It should ignore leading commas.
        // Integers may be several digits long.

        let mut result : Packet;
        if chars.len() == 0 {return None;}
        let mut next_char = chars.pop().unwrap();
        match next_char {
            ']' => None,
            ' ' => Self::new_chars(chars),
            ',' => Self::new_chars(chars),
            '[' => {
                let mut v : Vec<Packet> = vec![];
                while let Some(p) = Self::new_chars(chars) { v.push(p); }
                return Some(Self::List(v));
            }
            _ => {
                assert!(next_char.is_digit(10),"next char is {}, chars is ",next_char);
                let mut s_number: Vec<char> = vec![];
                while next_char.is_digit(10) {
                    s_number.push(next_char);
                    match chars.pop() {
                        None => { return Some(Packet::Int(s_number.iter().collect::<String>().parse::<usize>().unwrap())); },
                        Some(c) => { next_char = c; },
                    }
                }
                chars.push(next_char);
                return Some(Packet::Int(s_number.iter().collect::<String>().parse::<usize>().unwrap() ));
            },
        }
    }

    fn new(contents: &str) -> Option<Self> {
        if contents.len() == 0 {return None;}
        let mut chars = contents.chars().collect::<Vec<char>>();
        chars.reverse();
        let result = Self::new_chars(&mut chars);
        if let Some(ref p ) = result { println!("new Packet {}",&p); }
        return result;
    }
}



fn process_file_contents( contents: &str) -> usize {
    let mut total = 0;
    let idx = 1;
    let mut lines = contents.lines().collect::<Vec<&str>>();
    let len = lines.len();
    let no_pairs = (len +1) / 3;
    for i in 0..no_pairs {
        let packet1 = Packet::new(lines[3*i]).unwrap();
        let packet2 = Packet::new(lines[3*i +1]).unwrap();
        println!("index: {}  testing packet1 {} against packet2 {}",i+1,packet1,packet2);
        if packet1 > packet2 {println!("packet1 >packet2");} else {total +=i+1;println!("packet1 <= packet2: new total {}",total)}
    }
    return total;
}


fn process_file_contents2( contents: &str) -> usize {
    let mut total = 0;
    let mut v: Vec<Packet> = vec![];
    for line in contents.lines() {
        if let Some(p) = Packet::new(line) {
            v.push(p);
        }
    }
    let div1 = Packet::new("[[2]]").unwrap();
    let div2 = Packet::new("[[6]]").unwrap();
    v.push(div1.clone());
    v.push(div2.clone());
    v.sort();
    let mut code1 = 0;
    let mut code2 = 0;
    for (idx,p) in v.iter().enumerate() {
        if div1 == *p {code1 = idx+1};
        if div2 == *p {code2 = idx+1};
    }
    let total = code1 * code2;
    return total;
}



#[cfg(test)]
mod tests {
    use super::*;
   
    
    #[test]
    fn test_comparison() {
        assert_eq!(process_file_contents(
        "[9]
        [[8,7,6]]"), 0);
    }
        
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
        [1,[2,[3,[4,[5,6,0]]]],8,9]"), 140);
    }

}

