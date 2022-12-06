// this is a library
use std::fs;

pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input4.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The original total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}

fn get_integers( line: &str ) -> Vec<i32> {
    let mut cnt = 0;
    let chars: Vec<char> = line.chars().collect();
    let mut char_iter = line.chars();
    let len = chars.len();
    let mut v:Vec<i32> = Vec::new();
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
                            v.push( chars[start..end].iter().collect::<String>().parse::<i32>().unwrap() );
                            cnt += 1;
                            break;
                        } else {
                            cnt += 1;
                        }
                    } else { 
                        let end = cnt; 
//                        println!("final: line: {}\tstart: {}\t end: {}",line,start,end);
                        v.push( chars[start..end].iter().collect::<String>().parse::<i32>().unwrap() );
                        break;
                    }
                }
            }
            else { cnt += 1; }
        } else {break;}
    }
    return v;
}            
        

fn process_line( line: &str ) -> bool {
    //let chars = line.chars();
    let v = get_integers( line );
    let len = v.len();                      assert!(len==4,"{} integers found in line: {}",len,&line);
    return (v[0] - v[2])*(v[1] - v[3]) <= 0; 
}

fn process_file_contents( contents: &str) -> u32 {
    let mut _cnt = 0;
    let mut total = 0;
    for line in contents.lines() {
      if process_line( &line ) { total += 1; }
      _cnt += 1;
    }
    return total;
}

fn process_line2( line: &str ) -> bool {
    //let chars = line.chars();
    let v = get_integers( line );
    let len = v.len();                      assert!(len==4,"{} integers found in line: {}",len,&line);
    return (v[0] <= v[3]) && (v[1] >= v[2]) ; 
}

fn process_file_contents2( contents: &str) -> u32 {
    let mut _cnt = 0;
    let mut total = 0;
    for line in contents.lines() {
      if process_line2( &line ) { total += 1; }
      _cnt += 1;
    }
    return total;
}


#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_get_integers() {
        assert_eq!(get_integers("211-433,62-8190"),vec![211,433,62,8190]);
        assert_eq!(get_integers("2-4,6-8"),vec![2,4,6,8]);
    }


    #[test]
    fn test_process_line() {
        assert_eq!(process_line("211-433,62-8190"),true);

        assert_eq!(process_line("2-4,6-8"),false);
        assert_eq!(process_line("2-3,4-5"),false);
        assert_eq!(process_line("5-7,7-9"),false);
        assert_eq!(process_line("2-8,3-7"),true);
        assert_eq!(process_line("6-6,4-6"),true);
        assert_eq!(process_line("2-6,4-8"),false);
    }    
    
    #[test]
    fn test_process_file_contents() {
        assert_eq!(process_file_contents("2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"), 2);
    }
    
    #[test]
    fn test_process_line2() {
        assert_eq!(process_line2("2-4,6-8"),false);
        assert_eq!(process_line2("2-3,4-5"),false);
        assert_eq!(process_line2("5-7,7-9"),true);
        assert_eq!(process_line2("2-8,3-7"),true);
        assert_eq!(process_line2("6-6,4-6"),true);
        assert_eq!(process_line2("2-6,4-8"),true);
    }
  
    #[test]
    #[should_panic]
    fn test_panic() {
        process_line("1 2 3");
        process_line("1 2 3 4 5");
    }    
 
}

