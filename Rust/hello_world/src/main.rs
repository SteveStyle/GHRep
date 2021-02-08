use std::fmt;

#[derive(Debug)]
struct List(Vec<i32>);

impl fmt::Display for List {
    fn fmt( &self, f: &mut fmt::Formatter ) -> fmt::Result {
        let vec = &self.0;
        write!(f, "[")?;
        for (count, value) in vec.iter().enumerate() {
            if count != 0 { write!(f, ", ")?; }
            write!(f, "{}: {}", count, value)?;
        }
        write!(f, "]")
    }
}

fn main() {
    
    let l = List(vec![1, 2, 3]);
    
    println!("Display is {}", l);
    println!("Debug is {:?}", l);
}