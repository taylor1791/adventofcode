pub mod parser;

use std::collections::HashSet;

#[derive(Eq, PartialEq)]
struct Solution {
    location: Location,
    hq: Option<Location>,
    theta: i32,
}

#[derive(Eq, PartialEq)]
struct Location {
    x: i32,
    y: i32,
}

pub fn solve(input: &String) -> String {
    let end = _solve(input);
    let hq = end.hq.unwrap();

    format!(
        "end=({}, {}) {} blocks. hq=({}, {}) {} blocks",
        end.location.x,
        end.location.y,
        manhattan(&end.location),
        hq.x,
        hq.y,
        manhattan(&hq) // Lower than 251
    )
}

fn manhattan(location: &Location) -> i32 {
    location.x.abs() + location.y.abs()
}

fn _solve(input: &String) -> Solution {
    let mut location = Solution {
        location: Location { x: 0, y: 0 },
        hq: None,
        theta: 0,
    };
    let mut path = HashSet::new();
    path.insert((0 as i32, 0 as i32));

    let (_, motions) = parser::parse(&input.as_bytes()).unwrap();
    for motion in motions {
        step(&mut path, &mut location, motion);
    }

    location
}

fn step(path: &mut HashSet<(i32, i32)>, solution: &mut Solution, motion: parser::Motion) {
    let delta = if motion.direction == parser::Direction::Left {
        -90
    } else {
        90
    };
    solution.theta = (solution.theta + delta + 360) % 360;

    let mut step = (0 as i32, 0 as i32);
    if solution.theta == 0 {
        step = (0, -1);
    } else if solution.theta == 90 {
        step = (1, 0);
    } else if solution.theta == 180 {
        step = (0, 1);
    } else if solution.theta == 270 {
        step = (-1, 0);
    }

    for _ in 0..motion.length {
        solution.location.x += step.0;
        solution.location.y += step.1;
        if solution.hq.is_none() && path.contains(&(solution.location.x, solution.location.y)) {
            solution.hq = Some(
                (Location {
                     x: solution.location.x,
                     y: solution.location.y,
                 }),
            );
        }
        path.insert((solution.location.x, solution.location.y));
    }
}

#[cfg(test)]
mod test {
    use day1;

    #[test]
    fn test_p1_1() {
        let sol = day1::_solve(&"R2, R3".to_string());
        assert_eq!(day1::manhattan(&sol.location), 5);
    }

    #[test]
    fn test_p1_2() {
        let sol = day1::_solve(&"R2, R2, R2".to_string());
        assert_eq!(day1::manhattan(&sol.location), 2);
    }

    #[test]
    fn test_p1_3() {
        let sol = day1::_solve(&"R5, L5, R5, R3".to_string());
        assert_eq!(day1::manhattan(&sol.location), 12);
    }

    #[test]
    fn test_p2_1() {
        let sol = day1::_solve(&"R8, R4, R4, R8".to_string());
        assert_eq!(day1::manhattan(&(sol.hq.unwrap())), 4);
    }
}
