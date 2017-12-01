pub fn solve(input: &String) -> String {
    let input = input.lines().nth(0).unwrap().to_string();

    format!(
        "Skip 1: {} Skip n/2: {}",
        _solve(&input, &1),
        _solve(&input, &(input.len() / 2))
    )
}

fn _solve(input: &String, skip: &usize) -> usize {
    let nums: Vec<usize> = input.chars().map(|x| x as usize - '0' as usize).collect();

    let mut sum = 0;
    let l = nums.len();
    for (i, m) in nums.iter().enumerate() {
        let n = nums[(i + *skip as usize) % l];
        if n == *m {
            sum += nums[i];
        }
    }

    sum
}

#[cfg(test)]
mod test {
    use day1;

    #[test]
    fn test_p1_1() {
        assert_eq!(day1::_solve(&"1122".to_string(), &1), 3);
    }

    #[test]
    fn test_p1_2() {
        assert_eq!(day1::_solve(&"1111".to_string(), &1), 4);
    }

    #[test]
    fn test_p1_3() {
        assert_eq!(day1::_solve(&"1234".to_string(), &1), 0);
    }

    #[test]
    fn test_p1_4() {
        assert_eq!(day1::_solve(&"91212129".to_string(), &1), 9);
    }

    #[test]
    fn test_p2_1() {
        assert_eq!(day1::_solve(&"1212".to_string(), &2), 6);
    }

    #[test]
    fn test_p2_2() {
        assert_eq!(day1::_solve(&"1221".to_string(), &2), 0);
    }

    #[test]
    fn test_p2_3() {
        assert_eq!(day1::_solve(&"123425".to_string(), &3), 4);
    }

    #[test]
    fn test_p2_4() {
        assert_eq!(day1::_solve(&"12131415".to_string(), &4), 4);
    }
}
