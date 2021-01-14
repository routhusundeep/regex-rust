use std::env;

fn main() {
    let (regex, input_string) = parse_args().unwrap();
    let is_match = true;
    println!("args are {} and {}", &regex, &input_string);
    println!("Result is {}", is_match);
}

fn parse_args() -> Result<(String, String), String> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        Result::Err(String::from("Need 2 args"))
    } else {
        let first: &str = args.get(1).unwrap();
        let second: &str = args.get(2).unwrap();
        Ok((String::from(first), String::from(second)))
    }
}
