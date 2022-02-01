fn main () {
    let sentance = String::from("hello world");
    let fw = first_word(&sentance);
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
    
    