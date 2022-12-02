// this is a library
use std::fs;

pub fn lib_fn () {
    println!("hello from the library");
}

pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input1.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let v = process_file_contents( &content );
    println!("The most calories for one elf is {}.",get_largest(&v));
    println!("The top three elves have {} calories between them.",get_largest_3(&v));
}

fn process_file_contents( contents: &str) -> Vec<usize> {
    let mut v:Vec<usize> = Vec::new();
    let mut total = 0;
    let mut elf:usize = 1;
    for line in contents.lines() {
      if let Ok(calories) = line.parse::<usize>() {
        total += calories;
      } else {
        v.push(total);
        total = 0;
        elf += 1;
      }
    }
    if total > 0 {
      v.push(total);
    }
    v.sort();
    v.reverse();
    return v;
}

fn get_largest( v:&Vec<usize> ) -> usize {
  return v.get(0).copied().unwrap_or(0);
}

fn get_largest_3( v:&Vec<usize> ) -> usize {
  return v.get(0).copied().unwrap_or(0) +
         v.get(1).copied().unwrap_or(0) +
         v.get(2).copied().unwrap_or(0);
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn check_vector() {
        let v = process_file_contents( "2345\n\n3456\n1234" );
        for i in &v {
            println!{"{}",i};
        }
        assert_eq!(v, vec![4690,2345]);

        println!("end of first test");
        let v = process_file_contents( "2345\n\n3456\n1234\n" );
        for i in &v {
            println!{"{}",i};
        }
        println!("end of second test\n");
        assert_eq!(v, vec![4690,2345]);
//        assert_eq!(v.get(0).copied().unwrap_or(0), 1234);
    }
    
    #[test]
    fn check_sums() {
        let v = process_file_contents( "5\n2\n3\n\n101\n\n50\n\n6" );
        for i in &v {
            println!{"{}",i};
        }
        assert_eq!(get_largest(&v),101);
        assert_eq!(get_largest_3(&v),161);
    }    
}

