extern crate aoc2016;

use std::string::String;
use std::io::prelude::*;
use std::fs::File;

use aoc2016::*;

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
        Day {
            name: String::from("Day 2.1"),
            file: String::from("day2.txt"),
            solve: day2::simple_solve,
        },
        Day {
            name: String::from("Day 2.2"),
            file: String::from("day2.txt"),
            solve: day2::fun_solve,
        },
        Day {
            name: String::from("Day 3.1"),
            file: String::from("day3.txt"),
            solve: day3::solve_hor,
        },
        Day {
            name: String::from("Day 3.2"),
            file: String::from("day3.txt"),
            solve: day3::solve_ver,
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
