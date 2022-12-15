use std::fmt::{Display, self};
use std::fs;
use std::str::FromStr;
use regex::Regex;


pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input15.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The part 1 total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}


fn get_numbers<T: FromStr>(source: &str) -> Vec<T>
where T::Err: std::fmt::Debug
{
    // Use a regular expression to match the first sequence of digits in the string
    let re = Regex::new(r"-?\d+(\.\d+)?").unwrap();
    let mut result: Vec<T> = vec![];
    for captures in re.captures_iter(source) {
        let digit_string = captures.get(0).unwrap().as_str();
        let number = digit_string.parse().unwrap();
        result.push(number);
    }
    return result;
}


struct Reading {
    sensor_x : usize,
    sensor_y : usize,
    beacon_x : usize,
    beacon_y : usize,
}

impl Reading {
    fn from(line : &str) -> Reading {
        let numbers = get_numbers::<usize>(line);
        assert_eq!(numbers.len(),4);
        return Reading { sensor_x: numbers[0], sensor_y: numbers[1], beacon_x: numbers[2], beacon_y: numbers[3] };
    }
}

fn process_file_contents( contents: &str) -> usize {
    let total = 0;
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
    fn test_get_integers() {
        let v = get_numbers::<usize>("hello 12 goodbye 13 123 45");
        println!("{:?}",v);
        let v = get_numbers::<i32>("hello 12 goodbye -13 123 45");
        println!("{:?}",v);
        let v = get_numbers::<f32>("hello 12.2 goodbye -13.5 123 45");
        println!("{:?}",v);

    }
  
    #[test]
    fn test_process_file_contents() {
        assert_eq!(process_file_contents("Sensor at x=2, y=18: closest beacon is at x=-2, y=15
        Sensor at x=9, y=16: closest beacon is at x=10, y=16
        Sensor at x=13, y=2: closest beacon is at x=15, y=3
        Sensor at x=12, y=14: closest beacon is at x=10, y=16
        Sensor at x=10, y=20: closest beacon is at x=10, y=16
        Sensor at x=14, y=17: closest beacon is at x=10, y=16
        Sensor at x=8, y=7: closest beacon is at x=2, y=10
        Sensor at x=2, y=0: closest beacon is at x=2, y=10
        Sensor at x=0, y=11: closest beacon is at x=2, y=10
        Sensor at x=20, y=14: closest beacon is at x=25, y=17
        Sensor at x=17, y=20: closest beacon is at x=21, y=22
        Sensor at x=16, y=7: closest beacon is at x=15, y=3
        Sensor at x=14, y=3: closest beacon is at x=15, y=3
        Sensor at x=20, y=1: closest beacon is at x=15, y=3"), 26);
    }

    #[test]
    fn test_process_file_contents2() {
        assert_eq!(process_file_contents2("Sensor at x=2, y=18: closest beacon is at x=-2, y=15
        Sensor at x=9, y=16: closest beacon is at x=10, y=16
        Sensor at x=13, y=2: closest beacon is at x=15, y=3
        Sensor at x=12, y=14: closest beacon is at x=10, y=16
        Sensor at x=10, y=20: closest beacon is at x=10, y=16
        Sensor at x=14, y=17: closest beacon is at x=10, y=16
        Sensor at x=8, y=7: closest beacon is at x=2, y=10
        Sensor at x=2, y=0: closest beacon is at x=2, y=10
        Sensor at x=0, y=11: closest beacon is at x=2, y=10
        Sensor at x=20, y=14: closest beacon is at x=25, y=17
        Sensor at x=17, y=20: closest beacon is at x=21, y=22
        Sensor at x=16, y=7: closest beacon is at x=15, y=3
        Sensor at x=14, y=3: closest beacon is at x=15, y=3
        Sensor at x=20, y=1: closest beacon is at x=15, y=3"), 0);
    }

}

