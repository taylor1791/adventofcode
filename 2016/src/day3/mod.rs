use std::str::FromStr;

struct Triangle {
    a: i32,
    b: i32,
    c: i32,
}

impl Triangle {
    fn valid(&self) -> bool {
        self.a + self.b > self.c && self.a + self.c > self.b && self.b + self.c > self.a
    }
}

pub fn solve_ver(input: &String) -> String {
    let mut valid_count = 0;

    let mut triangles = [
        &mut Triangle { a: 0, b: 0, c: 0 },
        &mut Triangle { a: 0, b: 0, c: 0 },
        &mut Triangle { a: 0, b: 0, c: 0 },
    ];
    let assigns = [assign_a, assign_b, assign_c];

    for (i, line) in input.lines().enumerate() {
        let sides: Vec<&str> = line.split_whitespace().collect();

        assigns[i % 3](&mut triangles, &sides);
        if i % 3 == 2 {
            valid_count += count_valid(&triangles);
        }
    }

    format!("{}", valid_count)
}

fn count_valid(triangles: &[&mut Triangle; 3]) -> u32 {
    let mut count = 0;
    for j in 0..3 {
        if triangles[j].valid() {
            count += 1;
        }
    }

    count
}

fn assign_a(triangles: &mut [&mut Triangle; 3], sides: &Vec<&str>) {
    for j in 0..3 {
        triangles[j].a = FromStr::from_str(sides[j]).unwrap();
    }
}

fn assign_b(triangles: &mut [&mut Triangle; 3], sides: &Vec<&str>) {
    for j in 0..3 {
        triangles[j].b = FromStr::from_str(sides[j]).unwrap();
    }
}

fn assign_c(triangles: &mut [&mut Triangle; 3], sides: &Vec<&str>) {
    for j in 0..3 {
        triangles[j].c = FromStr::from_str(sides[j]).unwrap();
    }
}

pub fn solve_hor(input: &String) -> String {
    let mut valid_count = 0;

    for line in input.lines() {
        let sides: Vec<&str> = line.split_whitespace().collect();
        let tri = Triangle {
            a: FromStr::from_str(sides[0]).unwrap(),
            b: FromStr::from_str(sides[1]).unwrap(),
            c: FromStr::from_str(sides[2]).unwrap(),
        };
        if tri.valid() {
            valid_count += 1;
        }
    }

    format!("{}", valid_count)
}

#[cfg(test)]
mod test {
    use day3;

    #[test]
    fn test_p1() {
        assert_eq!(day3::Triangle { a: 5, b: 10, c: 25 }.valid(), false)
    }
}
