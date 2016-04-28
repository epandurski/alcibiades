extern crate regex;

pub mod utils;
pub mod basetypes;

fn main() {
    use regex::Regex;
    let re = Regex::new(r"^[a-h][1-8]$").unwrap();
    if re.is_match("e8") {
        println!("The square is \"{}\"", "e8");
        
    } else {
        println!("Wrong format!");
    }
}
