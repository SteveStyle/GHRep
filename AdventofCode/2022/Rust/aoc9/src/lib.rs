// this is a library
use std::fs;
use crate::position::position::*;

mod position;

pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input9.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The original total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}


fn get_head_move(line: &str) -> (Pos, i32) {
    let line = line.trim();
    let parts: Vec<&str> = line.split_whitespace().collect();
    
    let n = parts[1]; 
    let n : i32  = n.parse::<i32>().unwrap();
    
    let c = parts[0]; 
    let pos = match c {
        "R" => Pos{x:1,y:0},
        "L" => Pos{x:-1,y:0},
        "U" => Pos{x:0,y:1},
        "D" => Pos{x:0,y:-1},
        _   => panic!("character {} is not R,L,U or D",c),
    };
    return(pos,n);
}

#[derive(Debug)]
struct HistoryItem {
    h_head_move: Pos, 
    h_head: Pos,
    h_tail: Pos 
}

use std::fmt;

impl fmt::Display for HistoryItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "head_move=({}, {}) head=({}, {}) tail=({}, {})", self.h_head_move.x, self.h_head_move.y, self.h_head.x, self.h_head.y, self.h_tail.x, self.h_tail.y)
    }
}

#[derive(Debug)]
struct History {
    history_items : Vec<HistoryItem>,
}

impl fmt::Display for History {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.history_items.iter().fold(Ok(()), |result, history_item| {
            result.and_then(|_| writeln!(f, "{}", history_item))
        })
    }
}


fn process_file_contents( contents: &str) -> usize {
    let mut head = Pos{x:0,y:0};
    let mut tail = Pos{x:0,y:0};
    let mut tail_positions : Vec<Pos> = vec![tail];
    let mut history  = History{history_items: vec![]};
    for line in contents.lines() {
        let (head_move, n) = get_head_move(line);
        for i in 0..n {
            head += head_move;
            tail = Pos::move_tail(&head, &tail);
            tail_positions.push(tail);
            history.history_items.push(HistoryItem { h_head_move: head_move, h_head: head, h_tail: tail });
        }
    }
    println!("history {}",history);
    tail_positions.sort();
    tail_positions.dedup();
    println!("tail positions {:?}",tail_positions);
    return tail_positions.len();
}

fn process_file_contents2( contents: &str) -> usize {
    let mut head = Pos{x:0,y:0};
    let mut tails: Vec<Pos> = vec![Pos{x:0,y:0};9];
    assert_eq!(tails.len(),9);
    let mut tail_positions : Vec<Pos> = vec![tails[8]];
    let mut history  = History{history_items: vec![]};
    for line in contents.lines() {
        let (head_move, n) = get_head_move(line);
        for i in 0..n {
            head += head_move;
            tails[0] = Pos::move_tail(&head, &tails[0]);
            for i in 1..9 {
                tails[i] = Pos::move_tail(&tails[i-1], &tails[i]);
            }
            tail_positions.push(tails[8]);
            history.history_items.push(HistoryItem { h_head_move: head_move, h_head: head, h_tail: tails[8] });
        }
    }
    println!("history {}",history);
    tail_positions.sort();
    tail_positions.dedup();
    println!("tail positions {:?}",tail_positions);
    return tail_positions.len();
}



#[cfg(test)]
mod tests {
    use super::*;
   
    
    #[test]
    fn test_process_file_contents() {
        assert_eq!(process_file_contents("R 4
        U 4
        L 3
        D 1
        R 4
        D 1
        L 5
        R 2"), 13);
    }

    #[test]
    fn test_process_file_contents2() {
        assert_eq!(process_file_contents2("R 4
        U 4
        L 3
        D 1
        R 4
        D 1
        L 5
        R 2"), 1);
    }

    #[test]
    fn test_process_file_contents2_1() {
        assert_eq!(process_file_contents2("R 5
        U 8
        L 8
        D 3
        R 17
        D 10
        L 25
        U 20"), 36);
    }



}

