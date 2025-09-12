use std::fs::read_to_string;
use regex::Regex;

fn read_lines(filename: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut first_char: char = '0';
    let mut second_char: char = '0';
    let mut extracted_num: Option<i32> = Some(0);
    let mut total_num: i32 = 0;

    for line in read_to_string(filename).unwrap().lines() {
        result.push(line.to_string());
        println!("{}", line.to_string());

        first_char = get_first_digit(line);
        second_char = get_last_digit(line);

        println!("{}", first_char);

        println!("{}", second_char);

        extracted_num = combine_chars_to_int(first_char, second_char);
        match extracted_num {
            Some(num) => {
                println!("{}", num);
                total_num += num;
            }
            None => println!("number not found"),
        }
    }

    println!("and the answer is {}", total_num);

    result
}

fn get_first_digit(line: &str) -> char {
    let re = Regex::new(r"\d").unwrap();
    let mut chars: Vec<char> = line.chars().collect();
    let mut result_c: char = '0';

    for char_c in chars.iter() {
        if re.is_match(&char_c.to_string()) {
            result_c = *char_c;
            break;
        }
    }
    
    result_c
}

fn get_last_digit(line: &str) -> char {
    let re = Regex::new(r"\d+").unwrap();
    let mut chars: Vec<char> = line.chars().collect();
    let mut result_c: char = '0';

    for char_c in chars.iter().rev() {
        if re.is_match(&char_c.to_string()) {
            result_c = *char_c;
            break;
        }
    }

    result_c
}

fn combine_chars_to_int(c1: char, c2: char) -> Option<i32> {
    // Convert chars to digits (0-9)
    let digit1 = c1.to_digit(10)?;
    let digit2 = c2.to_digit(10)?;

    // Combine the digits: digit1 * 10 + digit2
    Some((digit1 * 10 + digit2) as i32)
}

fn main() {
   let filename2 = "./data/input.txt";

   let result = read_lines(filename2);
   /*println!("{}", result);*/
}
