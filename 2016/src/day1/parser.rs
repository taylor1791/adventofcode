use nom::digit;
use std::str;
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq)]
pub struct Motion {
    pub direction: Direction,
    pub length: u32,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Direction {
    Left,
    Right,
}

named!(
    pub parse<&[u8],Vec<Motion>>,
    separated_list_complete!(
        char!(','),
        parse_motion
    )
);

named!(
    parse_motion<&[u8], Motion>,
    ws!(do_parse!(
        direction: parse_direction >>
        length: parse_length >>
        (Motion {direction: direction, length: length})
    ))
);

named!(
    parse_length<&[u8], u32>,
    map_res!(
        map_res!(digit, str::from_utf8),
        FromStr::from_str
    )
);

named!(
    parse_direction<&[u8], Direction>,
    alt!(
        map!(tag!("L"), |_| Direction::Left) |
        map!(tag!("R"), |_| Direction::Right)
    )
);

#[cfg(test)]
mod test {
    use nom::{IResult, ErrorKind};

    use day1::parser;

    #[test]
    fn parse_sample_document() {
        assert_eq!(
            IResult::Done(
                &b""[..],
                vec![
                    parser::Motion {
                        direction: parser::Direction::Left,
                        length: 23,
                    },
                    parser::Motion {
                        direction: parser::Direction::Right,
                        length: 2,
                    },
                    parser::Motion {
                        direction: parser::Direction::Right,
                        length: 191,
                    },
                ],
            ),
            parser::parse(&b"L23, R2, R191"[..])
        );
    }

    #[test]
    fn parse_sample_motion() {
        assert_eq!(
            IResult::Done(
                &b""[..],
                parser::Motion {
                    direction: parser::Direction::Left,
                    length: 23,
                },
            ),
            parser::parse_motion(&b" L23 "[..])
        );
    }

    #[test]
    fn parse_direction_left() {
        assert_eq!(
            IResult::Done(&b""[..], parser::Direction::Left),
            parser::parse_direction(&b"L"[..])
        );
    }

    #[test]
    fn parse_direction_right() {
        assert_eq!(
            IResult::Done(&b""[..], parser::Direction::Right),
            parser::parse_direction(&b"R"[..])
        );
    }

    #[test]
    fn parse_direction_fail() {
        assert_eq!(
            IResult::Error(ErrorKind::Alt),
            parser::parse_direction(&b"X23"[..])
        );
    }
}
