// this is a library
use std::fs;

pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input10.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The original total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}

#[derive(Debug,Clone)]
enum Opp {
    Addx{ value: i32 },
    Noop
}

impl Opp {
    fn no_cycles(&self) -> i32 {
        match *self {
            Opp::Noop => 1,
            Opp::Addx{value:_} => 2,
        }
    }
}
impl fmt::Display for Opp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Opp::Noop => write!(f, "Noop"),
            Opp::Addx { value } => write!(f, "Addx {}",value)
        }
    }
}

#[derive(Debug,Clone)]
struct HistoryItem {
    cycle: i32, 
    opp:   Opp,
    opp_cycle:  i32,
    x : i32,
}

use std::fmt;

impl fmt::Display for HistoryItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(cycle: {}, opp: {}, opp_cycle {}, x: {})",self.cycle,self.opp,self.opp_cycle,self.x)
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

fn get_opp(line: &str) -> Opp {
    let line = line.trim();
    if line.starts_with("noop") {return Opp::Noop;} 
    else if line.starts_with("addx") {
        let parts : Vec<&str> = line.split_whitespace().collect::<Vec<&str>>();
        let add_value = parts[1].parse::<i32>().unwrap();
        return Opp::Addx { value: add_value };
    } else {
        panic!("operater cannot be decoded: {}",line);
    }
}

fn apply_opp_to_x(opp: &Opp, x: i32) -> i32 {
    match *opp {
        Opp::Noop => {return x;},
        Opp::Addx{value} => {return x + value;},
        //_ => panic!("could not match opp in apply_opp_to_x()"),
    }
}

fn apply_opp(history: &mut History, opp : Opp) {
    let previous_item = history.history_items[history.history_items.len()-1].clone();
    for i in 1..opp.no_cycles() {
        history.history_items.push(HistoryItem { cycle: previous_item.cycle + i, opp: opp.clone(), opp_cycle: i, x: previous_item.x });
    }

    history.history_items.push(HistoryItem { cycle: previous_item.cycle + opp.no_cycles(), opp: opp.clone(), opp_cycle: opp.no_cycles(), x: apply_opp_to_x(&opp, previous_item.x) });
}

fn process_file_contents( contents: &str) -> i32 {
    let mut history  = History{history_items: vec![HistoryItem{ cycle: 1, opp: Opp::Noop, opp_cycle: 1, x: 1 }]};
    for line in contents.lines() {
        apply_opp(&mut history, get_opp(line));
    }

    let mut total = 0;
    for h in history.history_items {
        if h.cycle % 40 == 20 {
            println!("{}",h);
            total += h.cycle * h.x;
        }
    }
    //let total = history.history_items.iter().filter(|h| h.cycle %40 == 20).map(|h| h.cycle * h.x).sum();
    return total;
}

fn cycle_to_pos( c: &i32 ) -> i32 {
    return (c -1) % 40;
}

fn render_image( history: &mut History ) {
    let mut line = String::from("");
    for i in 0..history.history_items.len() {
        let h = &history.history_items[i];
        let pos = cycle_to_pos(&h.cycle);
        if pos == 0 {line.push('\n')}
        if (pos - h.x).abs() < 2 { line.push('#') }
        else {line.push('.')}
    }
    println!("{}",line);
}

fn process_file_contents2( contents: &str) -> i32 {
    let mut history  = History{history_items: vec![HistoryItem{ cycle: 1, opp: Opp::Noop, opp_cycle: 1, x: 1 }]};
    for line in contents.lines() {
        apply_opp(&mut history, get_opp(line));
    }

    render_image( &mut history );
    return 0;
}



#[cfg(test)]
mod tests {
    use super::*;
   
    
    #[test]
    fn test_process_file_contents() {
        assert_eq!(process_file_contents2("addx 15
        addx -11
        addx 6
        addx -3
        addx 5
        addx -1
        addx -8
        addx 13
        addx 4
        noop
        addx -1
        addx 5
        addx -1
        addx 5
        addx -1
        addx 5
        addx -1
        addx 5
        addx -1
        addx -35
        addx 1
        addx 24
        addx -19
        addx 1
        addx 16
        addx -11
        noop
        noop
        addx 21
        addx -15
        noop
        noop
        addx -3
        addx 9
        addx 1
        addx -3
        addx 8
        addx 1
        addx 5
        noop
        noop
        noop
        noop
        noop
        addx -36
        noop
        addx 1
        addx 7
        noop
        noop
        noop
        addx 2
        addx 6
        noop
        noop
        noop
        noop
        noop
        addx 1
        noop
        noop
        addx 7
        addx 1
        noop
        addx -13
        addx 13
        addx 7
        noop
        addx 1
        addx -33
        noop
        noop
        noop
        addx 2
        noop
        noop
        noop
        addx 8
        noop
        addx -1
        addx 2
        addx 1
        noop
        addx 17
        addx -9
        addx 1
        addx 1
        addx -3
        addx 11
        noop
        noop
        addx 1
        noop
        addx 1
        noop
        noop
        addx -13
        addx -19
        addx 1
        addx 3
        addx 26
        addx -30
        addx 12
        addx -1
        addx 3
        addx 1
        noop
        noop
        noop
        addx -9
        addx 18
        addx 1
        addx 2
        noop
        noop
        addx 9
        noop
        noop
        noop
        addx -1
        addx 2
        addx -37
        addx 1
        addx 3
        noop
        addx 15
        addx -21
        addx 22
        addx -6
        addx 1
        noop
        addx 2
        addx 1
        noop
        addx -10
        noop
        noop
        addx 20
        addx 1
        addx 2
        addx 2
        addx -6
        addx -11
        noop
        noop
        noop"), 13140);
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
        R 2"), 0);
    }

}

