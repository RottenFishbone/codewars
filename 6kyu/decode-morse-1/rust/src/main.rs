#[macro_use]
extern crate lazy_static;

use std::collections::HashMap;

lazy_static! {
    static ref MORSE_CODE: HashMap <&'static str, &'static str> = {
        let mut map = HashMap::new();
        map.insert(".", "E");
        map.insert("-.-", "A");
        return map
    };
}

fn main() {
    assert_eq!(decode_morse("   -.- . . -.-   . . "), String::from("AEEA EE"));
}

fn decode_morse(encoded: &str) -> String {
    encoded.trim()              // Remove whitespace
        .split("   ")           // Split into words (of morse code)
        .map(|morseword| {      
            morseword
               .split(" ")      // Split morse words into morse chars
               .map(|morsechar| // Map morsechar to string
                   MORSE_CODE.get(morsechar).unwrap().to_string())
               .collect::<Vec<String>>()
               .join("")        // Join into a single word
        })
        .collect::<Vec<String>>()
        .join(" ")              // Join all words separated by space
}
