use regex::Regex;
use std::process::Command;

fn main() {
    //foreach test file:
    // read in the test file
    // parse out all the expectations
    // feed test file to gravlox
    // consume stdout and check the expects in order
    let expect_regex = Regex::new("//.*");

    let mut command = Command::new("./target/debug/gravlox");

    let output = command.arg("./test/basic").output().unwrap();

    println!("{:?}", output);
}
