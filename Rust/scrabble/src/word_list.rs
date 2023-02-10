use std::collections::HashSet;

use lazy_static::lazy_static;

use crate::tiles::LetterSet;
use crate::tiles::TileBag;

use super::utils::Timer;

use itertools::Itertools;
use itertools::Permutations;

use crate::tiles::ALPHABET;

const WORD_FILE: &'static str = include_str!("sowpods.txt");

lazy_static! {
    pub static ref WORDS: std::collections::HashSet<&'static str> =
        WORD_FILE.split_whitespace().collect();
}

lazy_static! {
    pub static ref LETTER_PREFIXES: [LetterSet; 26] = ALPHABET
        .iter()
        .map(|&letter| {
            let mut preceders = LetterSet::new_empty();
            for preceder in ALPHABET {
                if is_word(format!("{}{}", preceder, letter).as_str()) {
                    preceders.add(*preceder);
                }
            }
            preceders
        })
        .collect::<[LetterSet; 26]>();
}

impl FromIterator<LetterSet> for [LetterSet; 26] {
    fn from_iter<T: IntoIterator<Item = LetterSet>>(iter: T) -> Self {
        let mut result = [LetterSet::new_empty(); 26];
        for (i, item) in iter.into_iter().enumerate() {
            result[i] = item;
        }
        result
    }
}

lazy_static! {
    pub static ref LETTER_SUFFIXES: [LetterSet; 26] = ALPHABET
        .iter()
        .map(|&letter| {
            let mut suffixes = LetterSet::new_empty();
            for suffix in ALPHABET {
                if is_word(format!("{}{}", letter, suffix).as_str()) {
                    suffixes.add(*suffix);
                }
            }
            suffixes
        })
        .collect::<[LetterSet; 26]>();
}

pub fn is_word(word: &str) -> bool {
    WORDS.contains(word)
}

pub fn generate_anagrams(tiles: &TileBag) -> HashSet<String> {
    let timer = Timer::new("generate_anagrams()");
    let mut anagrams = HashSet::new();
    let mut tiles = tiles.to_vec();
    tiles.sort();
    for length in 1..=tiles.len() {
        for word in tiles.iter().permutations(length) {
            let word = word.iter().map(|&c| c).collect::<String>();
            if is_word(&word) {
                anagrams.insert(word.to_string());
            }
        }
    }
    let elapsed = timer.elapsed();
    println!(
        "generate_anagrams() time taken: {} seconds",
        elapsed.as_secs_f64()
    );
    anagrams
}

#[cfg(test)]
mod test {
    use super::Timer;
    #[test]
    fn test_is_word() {
        let timer = Timer::new("show_totals()");
        let result =
            super::is_word("AAS") && super::is_word("AARDVARK") && !super::is_word("AARDVARKS");
        let elapsed = timer.elapsed();
        println!("Time taken: {} seconds", elapsed.as_secs_f64());

        println!(" AAS: {}", super::is_word("AAS"));
    }
    #[test]
    fn test_is_word2() {
        let timer = Timer::new("show_totals()");
        let mut letters = [0; 26];
        letters[0] = 2;
        letters[1] = 1;
        letters[2] = 1;
        letters[3] = 1;
        letters[4] = 2;

        let hs = super::generate_anagrams(&crate::tiles::TileBag { letters, blanks: 0 });
        println!("hs: {:?}", hs);
        let result = hs.contains("CEDE");
        println!("result: {}", result);
        let timer = Timer::new("test");
        let result = hs.contains("BADE") && hs.contains("ABED") && hs.contains("BEAD");
        let elapsed = timer.elapsed();
        println!("Time taken: {} seconds", elapsed.as_secs_f64());
        println!("result: {}", result);
    }
}
