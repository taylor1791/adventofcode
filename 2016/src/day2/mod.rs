use std::vec;

struct Position {
    x: i32,
    y: i32,
}

pub fn fun_solve(input: &String) -> String {
    let keypad = vec![
        vec![' ', ' ', '1', ' ', ' '],
        vec![' ', '2', '3', '4', ' '],
        vec!['5', '6', '7', '8', '9'],
        vec![' ', 'A', 'B', 'C', ' '],
        vec![' ', ' ', 'D', ' ', ' '],
    ];
    let mut position = Position { x: 0, y: 2 };

    solve(&input, &keypad, &mut position)
}

pub fn simple_solve(input: &String) -> String {
    let keypad = vec![
        vec!['1', '2', '3'],
        vec!['4', '5', '6'],
        vec!['7', '8', '9'],
    ];
    let mut position = Position { x: 1, y: 1 };

    solve(&input, &keypad, &mut position)
}

fn solve(input: &String, keypad: &Vec<Vec<char>>, position: &mut Position) -> String {
    let mut code = vec::Vec::new();

    for line in input.lines() {
        for char in line.to_string().chars() {
            let mut step = (0, 0);
            if char == 'U' {
                step = (0, -1);
            } else if char == 'D' {
                step = (0, 1);
            } else if char == 'L' {
                step = (-1, 0);
            } else if char == 'R' {
                step = (1, 0);
            }

            let next_x = clip(0, (keypad[0].len() - 1) as i32, position.x + step.0);
            let next_y = clip(0, (keypad.len() - 1) as i32, position.y + step.1);

            if keypad[next_y as usize][next_x as usize] != ' ' {
                position.x = next_x;
                position.y = next_y;
            }
        }

        code.push(keypad[position.y as usize][position.x as usize])
    }

    code.iter().collect()
}

fn clip(min: i32, max: i32, value: i32) -> i32 {
    value.max(min).min(max)
}

#[cfg(test)]
mod test {
    use day2;

    #[test]
    fn test_p1() {
        assert_eq!(
            day2::simple_solve(&"ULL\nRRDDD\nLURDL\nUUUUD".to_string()),
            "1985"
        )
    }

    #[test]
    fn test_p2() {
        assert_eq!(
            day2::fun_solve(&"ULL\nRRDDD\nLURDL\nUUUUD".to_string()),
            "5DB3"
        )
    }
}
