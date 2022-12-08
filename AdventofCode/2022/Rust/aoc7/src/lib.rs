// this is a library
use std::fs;
use crate::directory::directory::*;
pub mod directory;

pub fn show_totals() {
    const FILE_PATH: &str = "/home/steve/GHRep/AdventofCode/2022/data/input7.txt";
   
    let content = fs::read_to_string(FILE_PATH)
        .expect(&format!("I was not able to read the file {}.",FILE_PATH));
    let total_score = process_file_contents( &content );
    println!("The original total score is {}.",total_score);
    let total_score = process_file_contents2( &content );
    println!("The part 2 total score is {}.",total_score);
}




fn process_file_contents( contents: &str) -> usize {
    let dir  = create_directory(contents);

    let ( _, total ) = dir.get_directory_sizes(100000);
    return total;
    
}

fn create_directory(contents: &str) -> DirectoryItem {
    let mut stack: Vec<DirectoryItem> = vec![];
    for line in contents.lines() {
        let line = line.trim();
        let chars = line.chars();
        let vchars: Vec<char> = chars.collect();
        if line.starts_with("$ cd ..") {
            add_child_to_parent(&mut stack);
        
        } else if line.starts_with("$ cd ") {
                stack.push( DirectoryItem::Directory{ name: vchars[5..line.len()].iter().collect(), dir_items: vec![]});
        } else if line.starts_with("$ ls") { continue; 
        } else if line.starts_with("dir") { continue; 
        } else if line.starts_with("$ ls") { continue; 
        } else {
            let parts: Vec<&str> = line.split(" ").collect();
            let file_size = parts[0].parse::<usize>().unwrap();
            let file_name = parts[1];
            let len = stack.len();
            stack[len-1].add( & DirectoryItem::File { name: file_name.to_string(), size: file_size });
        }
    }
    while stack.len() >1 {
        add_child_to_parent(&mut stack);
    }
    return stack[0].clone();
}

fn add_child_to_parent(stack: &mut Vec<DirectoryItem>) {
    let len = stack.len();
    if len >= 2 {
        if let Some(child) = stack.pop() {
            stack[len-2].add(&child);
        }
    }
}

fn process_file_contents2( contents: &str) -> usize {
    const FILE_SPACE :isize = 70000000;
    const REQUIRED_SPACE :isize = 30000000;
    let dir  = create_directory(contents);

    let ( v, _ ) = dir.get_directory_sizes(100000000);
    let (root_size, _ ) = v[0];
    let required_to_free = REQUIRED_SPACE + root_size as isize - FILE_SPACE;
    let required_to_free = required_to_free as usize;
    let mut v: Vec<usize> = v.iter().map(|(s,_)| *s).filter(|s| *s>=required_to_free).collect();
    v.sort();
    let total = v[0];

    return total;
}



#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_create_directory() {
        let root = create_directory("$ cd /
        $ ls
        dir a
        14848514 b.txt
        8504156 c.dat
        dir d
        $ cd a
        $ ls
        dir e
        29116 f
        2557 g
        62596 h.lst
        $ cd e
        $ ls
        584 i
        $ cd ..
        $ cd ..
        $ cd d
        $ ls
        4060174 j
        8033020 d.log
        5626152 d.ext
        7214296 k");
        
        println!("the name is {} and it has size {}", root.name(), root.size());

        for item in root.clone() {
            let item_size = &item.size();
            match item {
                DirectoryItem::File { name, size } => {
                    println!("Found file {} with size {}", name, size);
                }
                DirectoryItem::Directory {  name,  dir_items } => {
                    println!("Found directory {} with {} items and size {}", name, dir_items.len(), item_size);
                }
            }
        };

        let ( v, total ) = root.get_directory_sizes(100000);
        for (dir_size, dir_name) in v {
            println!("{}\t{}",dir_size,dir_name);
        }
        println!("the total is {}",total);

        assert_eq!(total, 95437);


    }

    
    
    #[test]
    fn test_process_file_contents() {
        assert_eq!(process_file_contents("$ cd /
        $ ls
        dir a
        14848514 b.txt
        8504156 c.dat
        dir d
        $ cd a
        $ ls
        dir e
        29116 f
        2557 g
        62596 h.lst
        $ cd e
        $ ls
        584 i
        $ cd ..
        $ cd ..
        $ cd d
        $ ls
        4060174 j
        8033020 d.log
        5626152 d.ext
        7214296 k"), 95437);
    }

    #[test]
    fn test_process_file_contents2() {
        assert_eq!(process_file_contents2("$ cd /
        $ ls
        dir a
        14848514 b.txt
        8504156 c.dat
        dir d
        $ cd a
        $ ls
        dir e
        29116 f
        2557 g
        62596 h.lst
        $ cd e
        $ ls
        584 i
        $ cd ..
        $ cd ..
        $ cd d
        $ ls
        4060174 j
        8033020 d.log
        5626152 d.ext
        7214296 k"), 24933642);
    }

}

