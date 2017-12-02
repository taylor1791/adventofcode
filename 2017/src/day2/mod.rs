pub fn solve(input: &String) -> String {
    format!(
        "rangesum: {} quotientsum: {}",
        _solve(&input, row_range),
        _solve(&input, row_quotient) // > 103
    )
}

pub fn _solve(input: &String, checksum: fn(&Vec<i64>) -> i64) -> i64 {
    let row_checksums = input.lines().map(
        |l| row_checksum(&l.to_string(), checksum),
    );

    row_checksums.sum()
}

pub fn row_checksum(row: &String, checksum: fn(&Vec<i64>) -> i64) -> i64 {
    let cols: Vec<i64> = row.split_whitespace().map(|n| n.parse().unwrap()).collect();
    checksum(&cols)
}

pub fn row_range(row: &Vec<i64>) -> i64 {
    let max: i64 = row.iter().cloned().max().unwrap();
    let min: i64 = row.iter().cloned().min().unwrap();
    max - min
}

pub fn row_quotient(row: &Vec<i64>) -> i64 {
    for i in 0..row.len() {
        for j in (i + 1)..row.len() {
            let n = row[i].min(row[j]);
            let m = row[i].max(row[j]);
            if m % n == 0 {
                return m / n;
            }
        }
    }
    panic!("Couldn't find dividend for {:?}", row);
}

#[cfg(test)]
mod test {
    use day2;

    #[test]
    fn test_p1() {
        assert_eq!(
            day2::_solve(&"5 1 9 5\n7 5 3\n2 4 6 8\n".to_string(), day2::row_range),
            18
        );
    }

    #[test]
    fn test_p2() {
        assert_eq!(
            day2::_solve(
                &"5 9 2 8\n9 4 7 3\n3 8 6 5\n".to_string(),
                day2::row_quotient,
            ),
            9
        );
    }
}
