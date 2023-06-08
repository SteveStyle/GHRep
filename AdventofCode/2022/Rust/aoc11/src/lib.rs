// this is a library
use std::fs;
//use std::num::integer::lcm;

pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input11.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The original total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}


fn get_integers( line: &str ) -> Vec<i64> {
    let mut cnt = 0;
    let chars: Vec<char> = line.chars().collect();
    let mut char_iter = line.chars();
    let len = chars.len();
    let mut v:Vec<i64> = Vec::new();
    loop {
        if cnt > len + 2 { panic!("outer: cnt {} is larger than len {}",cnt,len); }
        if let Some(c) = char_iter.next() {
            if c.is_digit(10) { 
                let start = cnt; cnt +=1;
                loop {
                    if cnt > len + 10 { panic!("inner: cnt {} is larger than len {}",cnt,len); }
                    if let Some(c) = char_iter.next() {
                        if !c.is_digit(10) {
                            let end = cnt; 
//                            println!("not final: line: {}\tstart: {}\t end: {}",line,start,end);
                            v.push( chars[start..end].iter().collect::<String>().parse::<i64>().unwrap() );
                            cnt += 1;
                            break;
                        } else {
                            cnt += 1;
                        }
                    } else { 
                        let end = cnt; 
//                        println!("final: line: {}\tstart: {}\t end: {}",line,start,end);
                        v.push( chars[start..end].iter().collect::<String>().parse::<i64>().unwrap() );
                        break;
                    }
                }
            }
            else { cnt += 1; }
        } else {break;}
    }
    return v;
}   


enum Param {
    Old,
    Number{ number: i64 },
}
struct Operation {
    symbol : char,
    param1 : Param,
    param2 : Param,
}

struct Monkey {
    pack_items : Vec<i64>,
    oper : Operation,
    test : i64,
    true_monkey : i64,
    false_monkey : i64,
    activity : i64,
}

pub struct Monkeys {
    monkeys : Vec<Monkey>,
    lcm_test : i64,
}

impl Monkeys {
    fn add_monkey( &mut self, text : &str ) {
        let text = text.trim();
        if text.len() <= 0 {
            return;
        }
        let mut monkey = Monkey{ pack_items: vec![], oper: Operation { symbol: '*', param1: Param::Old, param2: Param::Old }, test: 1, true_monkey: 0, false_monkey: 0, activity: 0 };
        for line in text.lines() {
            let line = line.trim();
            let integers = get_integers(line);
            if line.starts_with("Start") {monkey.pack_items = integers.clone();}
            if line.starts_with("Operation") {
                monkey.oper.param1 = Param::Old;
                monkey.oper.param2 = Param::Old;
                if integers.len() > 0 {
                    monkey.oper.param2 = Param::Number { number: integers[0] };
                }
                if line.contains("*") { monkey.oper.symbol = '*' }
                else { monkey.oper.symbol = '+' }
            }
            if line.starts_with("Test") {
                monkey.test = integers[0];
            }
            if line.starts_with("If true") { monkey.true_monkey = integers[0];}
            if line.starts_with("If false") { monkey.false_monkey = integers[0];}
        }
        self.monkeys.push(monkey);
    }


    fn new( text: &str ) -> Monkeys {
        let mut monkeys = Monkeys{monkeys: vec![], lcm_test: 1};
        let text = text.split("Monkey");
        for part in text {
            monkeys.add_monkey(part);
        }
        let mut test_lcm = 1;
        for i in 0..monkeys.monkeys.len() as i32 {
            test_lcm = test_lcm * monkeys.monkeys[i as usize].test;
        }
        monkeys.lcm_test = test_lcm;
        return monkeys;
    }

    fn apply_operation( old: i64, oper: &Operation, lcm_test: i64 ) -> i64 {
        let param1 = match oper.param1 { Param::Old => old, Param::Number { number } => number };
        let param2 = match oper.param2 { Param::Old => old, Param::Number { number } => number };
        let mut result = match oper.symbol { 
            '*' => param1 * param2, 
            '+' => param1 + param2, 
            _ => {panic!("could not match operation")}
        };
        result = result % lcm_test;
        return result;
    }
    fn process_monkeys (&mut self) {
        for i in 0..self.monkeys.len() {
            let no_items = self.monkeys[i].pack_items.len() as i64;
            self.monkeys[i].activity += no_items;
            let true_monkey = self.monkeys[i].true_monkey as usize;
            let false_monkey = self.monkeys[i].false_monkey as usize;
            for j in 0..no_items {
                let old = self.monkeys[i].pack_items[j as usize];
                let new = Monkeys::apply_operation( old, & self.monkeys[i].oper, self.lcm_test );
                if new % self.monkeys[i].test == 0 {
                    self.monkeys[true_monkey].pack_items.push(new);
                } else {
                    self.monkeys[false_monkey].pack_items.push(new);
                }
            }
            self.monkeys[i].pack_items = vec![];

        }
    }
}

fn process_file_contents( contents: &str) -> i64 {
    let mut monkeys = Monkeys::new(contents);
    println!("there are {} monkeys",monkeys.monkeys.len());


    for i in 0..10000 {
        monkeys.process_monkeys();
    }
    let mut v : Vec<i64> = vec![];
    for i in 0..monkeys.monkeys.len() {
        v.push(monkeys.monkeys[i].activity);
    }

    for i in 0..v.len() {
        println!("activity {}",v[i]);
    }

    v.sort();
    v.reverse();

    let total = v[0] * v[1];
    return total;
}


fn process_file_contents2( contents: &str) -> i64 {
    let total =0;
    return total;
}



#[cfg(test)]
mod tests {
    use super::*;
   
    
    #[test]
    fn test_process_file_contents() {
        assert_eq!(process_file_contents("Monkey 0:
        Starting items: 79, 98
        Operation: new = old * 19
        Test: divisible by 23
          If true: throw to monkey 2
          If false: throw to monkey 3
      
      Monkey 1:
        Starting items: 54, 65, 75, 74
        Operation: new = old + 6
        Test: divisible by 19
          If true: throw to monkey 2
          If false: throw to monkey 0
      
      Monkey 2:
        Starting items: 79, 60, 97
        Operation: new = old * old
        Test: divisible by 13
          If true: throw to monkey 1
          If false: throw to monkey 3
      
      Monkey 3:
        Starting items: 74
        Operation: new = old + 3
        Test: divisible by 17
          If true: throw to monkey 0
          If false: throw to monkey 1"), 2713310158);
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

