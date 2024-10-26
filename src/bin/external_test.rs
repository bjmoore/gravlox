use regex::Regex;
use std::error::Error;
use std::fs;
use std::io::BufRead;
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
        let test_file = test_file?;
        let test_name = test_file.file_name().into_string().unwrap();
        print!("Runing test: {} ... ", test_name);
        let contents = fs::read_to_string(test_file.path())?;

        let mut expectations = Vec::new();

        // put these into a vec
        for line in contents.lines() {
            match expect_regex.captures(line) {
                Some(captures) => expectations.push(captures.get(1).unwrap().as_str().to_string()),
                None => (),
            }
        }

        // compare vecs elt by elt and check they are =
        let output = Command::new(GRAVLOX_PATH).arg(test_file.path()).output()?;
        for line in output.stdout.lines() {
            let line = line.unwrap();
            if expectations[0] != line {
                println!("\x1b[0;31mfail!\x1b[0m");
                println!("Expected: {}, Actual: {}", expectations[0], line);
            } else {
                println!("pass!");
            }
        }
    }

    Ok(())
}
