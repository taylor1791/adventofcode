extern crate regex;

use self::regex::Regex;
use std::char;

#[derive(Debug)]
struct EncryptedRoom {
    characters: String,
    sector_id: u32,
    checksum: String,
}

struct Room {
    name: String,
    sector_id: u32,
}

impl EncryptedRoom {
    fn is_valid(&self) -> bool {
        let mut char_count = [0 as u32; 26];
        for c in self.characters.chars().filter(|x| *x != '-') {
            char_count[c as usize - 'a' as usize] += 1;
        }

        let mut char_with_count: Vec<(char, &u32)> = char_count
            .into_iter()
            .enumerate()
            .map(|(i, count)| {
                (char::from_u32((i + 'a' as usize) as u32).unwrap(), count)
            })
            .collect();


        char_with_count.sort_by(|a, b| if a.1 == b.1 {
            a.0.cmp(&b.0)
        } else {
            b.1.cmp(a.1)
        });
        let checksum: String = char_with_count
            .into_iter()
            .take(5)
            .map(|(x, _)| x)
            .collect();

        checksum == self.checksum
    }

    fn decrypt(&self) -> Room {
        Room {
            name: self.characters
                .chars()
                .map(|c| if c == '-' {
                    ' '
                } else {
                    rotate(c, self.sector_id)
                })
                .collect(),
            sector_id: self.sector_id,
        }
    }
}

fn rotate(c: char, n: u32) -> char {
    let char_index = c as u32 - 'a' as u32;
    let rotated = (char_index + n) % 26;

    char::from_u32('a' as u32 + rotated).unwrap()
}

pub fn solve(input: &String) -> String {
    let enc_rooms: Vec<EncryptedRoom> = input.lines().map(|line| parse(line)).collect();
    let room_count = enc_rooms.len();
    let valid: Vec<EncryptedRoom> = enc_rooms
        .into_iter()
        .filter(|room| room.is_valid())
        .collect();
    let valid_count = valid.len();
    let rooms: Vec<Room> = valid.iter().map(|room| room.decrypt()).collect();

    let sector_sum = valid.into_iter().fold(0, |acc, room| acc + room.sector_id);
    let northpole_objects = rooms.into_iter().find(|x| x.name.contains("pole")).unwrap();

    format!(
        "{}/{} sectors valid. Sector Sum: {} North Pole Objects: {}",
        valid_count,
        room_count,
        sector_sum,
        northpole_objects.sector_id
    )
}

lazy_static ! {
    static ref REG: Regex = Regex::new(r"^([a-z-]+)(\d+)\[([a-z]+)\]").unwrap();
}

fn parse(room_line: &str) -> EncryptedRoom {
    let parts = REG.captures(room_line).unwrap();

    let mut characters = String::from(parts.get(1).unwrap().as_str());
    characters.pop();
    EncryptedRoom {
        characters,
        sector_id: String::from(parts.get(2).unwrap().as_str())
            .parse()
            .unwrap(),
        checksum: String::from(parts.get(3).unwrap().as_str()),
    }
}

#[cfg(test)]
mod test {
    use day4;

    #[test]
    fn p1_1() {
        assert_eq!(day4::parse("aaaaa-bbb-z-y-x-123[abxyz]").is_valid(), true);
    }

    #[test]
    fn p1_2() {
        assert_eq!(day4::parse("a-b-c-d-e-f-g-h-987[abcde]").is_valid(), true);
    }

    #[test]
    fn p1_3() {
        assert_eq!(day4::parse("not-a-real-room-404[oarel]").is_valid(), true);
    }

    #[test]
    fn p1_4() {
        assert_eq!(
            day4::parse("totally-real-room-200[decoy]").is_valid(),
            false
        );
    }

    #[test]
    fn p2() {
        assert_eq!(
            day4::parse("qzmt-zixmtkozy-ivhz-343[xxxxx]").decrypt().name,
            "very encrypted name"
        );
    }
}
