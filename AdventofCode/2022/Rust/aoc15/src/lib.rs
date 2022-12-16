use std::fmt::{Display, self};
use std::fs;
use std::str::FromStr;
use regex::Regex;
use std::num;


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


struct Reading {
    sensor_x : i32,
    sensor_y : i32,
    beacon_x : i32,
    beacon_y : i32,
}

impl Reading {
    fn from(line : &str) -> Reading {
        let numbers = get_numbers::<i32>(line);
        assert_eq!(numbers.len(),4);
        return Reading { sensor_x: numbers[0], sensor_y: numbers[1], beacon_x: numbers[2], beacon_y: numbers[3] };
    }
    fn distance(&self) -> i32 {
        (self.sensor_x - self.beacon_x).abs() + (self.sensor_y - self.beacon_y).abs()
    }
    fn get_interval(&self, line_no: i32) -> Option<(i32,i32)> {
        let distance = self.distance();
        let delta_y = (self.sensor_y - line_no).abs();
        let result = (self.sensor_x + delta_y - distance, distance - delta_y + self.sensor_x);
        if result.0 <= result.1 { Some(result) }
        else { None }
    }
    pub fn merge_intervals(intervals: Vec<(i32, i32)>) -> Vec<(i32, i32)> {
        let mut merged: Vec<(i32, i32)> = Vec::new();
    
        // Sort the intervals by their start time
        let mut sorted_intervals = intervals.clone();
        sorted_intervals.sort_by_key(|i| i.0);
    
        // Iterate over the sorted intervals and merge any overlapping intervals
        let mut current_interval = sorted_intervals[0];
        for interval in sorted_intervals {
            if interval.0 <= current_interval.1 + 1{
                // The current interval overlaps with the next interval, so merge them
                current_interval.1 = current_interval.1.max(interval.1);
            } else {
                // The current interval does not overlap with the next interval, so add it to the list of merged intervals
                merged.push(current_interval);
                current_interval = interval;
            }
        }
    
        // Add the last interval to the list of merged intervals
        merged.push(current_interval);
    
        return merged;
    }
    
}

fn in_interval(intervals: &Vec<(i32, i32)>, x: i32) -> bool {
    // Check if the integer is contained within any of the intervals in the input vector
    for interval in intervals {
        if interval.0 <= x && x <= interval.1 {
            // The integer is contained within the current interval, so return true
            return true;
        }
    }

    // The integer was not contained within any of the intervals, so return false
    return false;
}


fn get_intersection( contents: &str, line_no: i32 ) -> i32 {
    let mut intervals : Vec<(i32,i32)> = vec![];
    let mut beacons : Vec<i32> = vec![];
    for line in contents.lines() {
        let reading = Reading::from(line);
        if let Some(interval) = reading.get_interval(line_no) { intervals.push(interval );}
        if reading.beacon_y == line_no {beacons.push(reading.beacon_x)}
    }
    beacons.sort();
    beacons.dedup();
    println!("{:?}",intervals);
    intervals = Reading::merge_intervals(intervals);
    println!("{:?}",intervals);
    let mut result =  intervals.iter().map(|(start,end)| end - start + 1).sum();
    for x in beacons {
        if in_interval( &intervals, x) {result -= 1}
    }
    return result;
}

fn get_gaps( contents: &str, x_start: i32, x_end : i32, y_start: i32, y_end: i32 ) -> Vec<(i32,i32)> {
    let mut intervals : Vec<(i32,i32)>;
    let mut readings : Vec<Reading> = vec![];
    let mut gaps : Vec<(i32,i32)> = vec![];
    for line in contents.lines() {
        let reading = Reading::from(line);
        readings.push(reading);
    }
    for y in y_start..y_end+1 {
        intervals = vec![];
        for reading in &readings {
            if let Some(interval) = reading.get_interval(y) {
                intervals.push(interval);
            }
        }
        intervals = Reading::merge_intervals(intervals);
        let mut truncated_intervals :Vec<(i32,i32)> = vec![];
        for interval in &intervals {
            if interval.1 >= x_start && interval.0 <= x_end {
                truncated_intervals.push((interval.0.max(x_start),interval.1.min(x_end)));
            }
        }
        //println!("y {}  {:?}",y,truncated_intervals);
        if truncated_intervals.len() == 2 {
            gaps.push((truncated_intervals[0].1 +1,y));
        }
    }
    println!("{:?}",gaps);
    return gaps;
}

fn process_file_contents( contents: &str) -> usize {
    let total = get_intersection(contents, 2000000) as usize;
    return total;
}


fn process_file_contents2( contents: &str) -> i64 {
    let gaps = get_gaps(contents,0,4000000,0,4000000);
    let total = gaps[0].0 as i64 * 4000000 + gaps[0].1 as i64;
    return total;
}





#[cfg(test)]
mod tests {
    use super::*;
   
    #[test]
    fn test_merge_intervals () {    
        let intervals = vec![(1, 5), (3, 8), (10, 12), (11, 15)];
        let merged = Reading::merge_intervals(intervals);
        println!("{:?}", merged); // [(1, 8), (10, 15)]
    }

  
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
    fn test_get_gaps() {
        get_gaps("Sensor at x=2, y=18: closest beacon is at x=-2, y=15
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
        Sensor at x=20, y=1: closest beacon is at x=15, y=3", 0, 20, 0, 20);
    }
  
    #[test]
    fn test_process_file_contents() {
        assert_eq!(get_intersection("Sensor at x=2, y=18: closest beacon is at x=-2, y=15
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
        Sensor at x=20, y=1: closest beacon is at x=15, y=3", 10), 26);
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

