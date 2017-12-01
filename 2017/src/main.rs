extern crate adventofcode_2017;

use std::string::String;
use std::io::prelude::*;
use std::fs::File;

use adventofcode_2017::*;

struct Day {
    name: String,
    file: String,
    solve: fn(&String) -> String,
}

fn main() {
    let days = [
        Day {
            name: String::from("Day 1"),
            file: String::from("day1.txt"),
            solve: day1::solve,
        },
    ];

    for &Day {
        ref name,
        ref file,
        solve,
    } in days.iter()
    {
        let input_file = "./input/".to_string() + file;
        let mut input = String::new();
        let mut file = File::open(input_file).expect("Failed to open file");
        file.read_to_string(&mut input).expect(
            "Failed reading file",
        );
        println!("{}: {}", name, solve(&input));
    }
}
