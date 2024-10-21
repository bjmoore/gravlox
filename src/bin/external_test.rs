use regex::Regex;
use std::error::Error;
use std::fs;
use std::process::Command;

const GRAVLOX_PATH: &str = "./target/debug/gravlox";
const TEST_DIR: &str = "./test";

fn main() -> Result<(), Box<dyn Error>> {
    //foreach test file:
    // read in the test file
    // parse out all the expectations
    // feed test file to gravlox
    // consume stdout and check the expects in order
    let expect_regex = Regex::new("// expect: (.*)")?;
    for test_file in fs::read_dir(TEST_DIR)? {
        let contents = fs::read_to_string(test_file?.path())?;
        for line in contents.lines() {
            let captures = expect_regex.captures(line).unwrap();
            println!("{:?}", captures);
        }
    }

    Ok(())
}
