use regex::Regex;
use std::collections::VecDeque;
use std::error::Error;
use std::fs;
use std::io::BufRead;
use std::process::Command;

const GRAVLOX_PATH: &str = "./target/debug/gravlox";
const TEST_DIR: &str = "./test";

fn main() -> Result<(), Box<dyn Error>> {
    let expect_regex = Regex::new("// expect: (.*)")?;
    for test_file in fs::read_dir(TEST_DIR)? {
        let mut passing = true;

        let test_file = test_file?;
        let test_name = test_file.file_name().into_string().unwrap();
        print!("Running test: {} ... ", test_name);
        let contents = fs::read_to_string(test_file.path())?;

        let mut expectations = VecDeque::new();

        // put these into a vec
        for line in contents.lines() {
            if let Some(captures) = expect_regex.captures(line) {
                if let Some(expected_value) = captures.get(1) {
                    expectations.push_back(expected_value.as_str());
                }
            }
        }

        // compare vecs elt by elt and check they are =
        let output = Command::new(GRAVLOX_PATH).arg(test_file.path()).output()?;
        let mut failed_expectations = Vec::new();
        for line in output.stdout.lines() {
            let line = line.unwrap();
            if expectations[0] != line {
                passing = false;
                failed_expectations.push(format!("Expected: {}, Actual: {}", expectations[0], line));
            } else {
                expectations.pop_front();
            }
        }

        if expectations.len() > 0 {
            passing = false;
        }

        if passing {
            println!("pass!");
        } else {
            println!("\x1b[0;31mfail!\x1b[0m");
            for failure in failed_expectations {
                println!("{}", failure);
            }
            for missed_expectation in expectations {
                println!("Expected, not found: {}", missed_expectation);
            }
        }
    }

    Ok(())
}
