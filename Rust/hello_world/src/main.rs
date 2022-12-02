fn main () {
    let sentence = String::from("hello world");
    let fw = first_word(&sentence);
    println!("function returned {}", fw);
}
    
fn first_word( s : &String ) -> &str {
    let bytes = s.as_bytes();
    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            println!("found a space at {}", i);
            return &s[..i];
        }
    }
    
    return &s[..];
}
    
    
