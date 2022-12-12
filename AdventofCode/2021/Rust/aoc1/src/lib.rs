use std::fs;

pub fn run() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2021/data/input1.txt";
    let content = fs::read_to_string(FILE_PATH)
    .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let mut result = process_file( &content );
    println!("The original total score is {}.",result);
    result = process_file2( &content );
    println!("The part 2 total score is {}.",result);
}

fn contents_to_vector(content : &str) -> Vec<usize> {
    let mut result : Vec<usize> = vec![];
    for line in content.lines() {
        let line = line.trim();
        result.push( line.parse().unwrap() );
    }
    return result;
}

fn process_file( content : &str) -> usize {
    let mut old_depth : usize = 0;
    let mut new_depth : usize;
    let mut total : usize = 0;
    let arr = contents_to_vector(content);
    for i in arr {
        new_depth = i;
        if new_depth > old_depth { total += 1; }
        old_depth = new_depth;
    }

    return total -1;
}

fn get_sums(arr: Vec<usize>) -> Vec<usize> {
    let mut result = vec![];
    for i in 0..arr.len() -2 {
        result.push(arr[i]+ arr[i+1] + arr[i+2]);
    }
    return result;
}


fn process_file2( content : &str) -> usize {
    let arr = contents_to_vector(content);
    let arr = get_sums(arr);
    let mut old_depth : usize = 0;
    let mut new_depth : usize;
    let mut total : usize = 0;
    for n in arr {
        new_depth = n as usize;
        if new_depth > old_depth { total += 1; }
        old_depth = new_depth;
    }

    return total -1;
}



#[cfg(test)]
mod tests {
    use super::*;
    
    
    #[test]
    fn test_process_file() {
        assert_eq!(process_file("199
        200
        208
        210
        200
        207
        240
        269
        260
        263"), 7);
    }

    #[test]
    fn test_process_file2() {
        assert_eq!(process_file2("199
        200
        208
        210
        200
        207
        240
        269
        260
        263"), 5);
    }
    
    #[test]
    #[should_panic]
    fn test_panic() {
        panic!("dummy");
    }    
 
}

