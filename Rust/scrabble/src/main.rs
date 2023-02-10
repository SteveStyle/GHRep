use std::io;

use scrabble::board::SCRABBLE_VARIANT_WORDFEUD;
use scrabble::*;

use chrono::Local;
use scrabble::pos::Position;
use scrabble::tiles::{Letter, TileList};

use crate::utils::Timer;

//TO DO : use terminal escape codes to clear the screen
//https://stackoverflow.com/questions/2979383/c-clear-the-console
//https://stackoverflow.com/questions/4842424/list-of-ansi-color-escape-sequences
//https://stackoverflow.com/questions/2616906/how-do-i-output-coloured-text-to-a-linux-terminal

fn main() {
    let mut game = Game::new(&SCRABBLE_VARIANT_WORDFEUD);

    // let the player input a move
    let mut input = String::new();
    println!("Your rack is {} ", game.players[0].rack);
    println!("Enter a move: ");
    io::stdin().read_line(&mut input).unwrap();

    println!("You entered: {}", input);

    let tiles: TileList = input.as_str().into();

    let length = tiles.len() as u8;

    let mut game_move = GameMove {
        position: Position::new(7, 7),
        player: 0,
        first_move: true,
        direction: Direction::Horizontal,
        length,
        move_position_map: None,
        word: None,
        score: None,
    };

    game.validate_position(&mut game_move);
    game.validate_move(&mut game_move, tiles);
}

fn test_anagram_version() {
    let timer = Timer::new("show_totals()");
    let mut letters = [0; 26];
    letters[0] = 2;
    letters[1] = 1;
    letters[2] = 1;
    letters[3] = 1;
    letters[4] = 2;

    let hs = word_list::generate_anagrams(&tiles::TileBag { letters, blanks: 0 });
    println!("hs: {:?}", hs);
    let result = hs.contains("CEDE");
    println!("result: {}", result);
    let timer = Timer::new("test");
    let mut result = hs.contains("BADE")
        && hs.contains("ABED")
        && hs.contains("BEAD")
        && hs.contains("CADEE")
        && hs.contains("ACE")
        && hs.contains("ECAD");
    for i in 0..10000000 {
        result = hs.contains("BADE")
            && hs.contains("ABED")
            && hs.contains("BEAD")
            && hs.contains("CADEE")
            && hs.contains("ACE")
            && hs.contains("ECAD");
    }
    let elapsed = timer.elapsed();
    println!(
        "Time taken anagram version: {} seconds",
        elapsed.as_secs_f64()
    );
    println!("result: {}", result);
}

fn test_full_version() {
    let timer = Timer::new("show_totals()");

    let result = word_list::is_word("CEDE");
    println!("result: {}", result);
    let timer = Timer::new("test full version");
    let mut result = word_list::is_word("BADE")
        && word_list::is_word("ABED")
        && word_list::is_word("BEAD")
        && word_list::is_word("CADEE")
        && word_list::is_word("ACE")
        && word_list::is_word("ECAD");
    for i in 0..10000000 {
        result = word_list::is_word("BADE")
            && word_list::is_word("ABED")
            && word_list::is_word("BEAD")
            && word_list::is_word("CADEE")
            && word_list::is_word("ACE")
            && word_list::is_word("ECAD");
    }
    let elapsed = timer.elapsed();
    println!("Time taken full version: {} seconds", elapsed.as_secs_f64());
    println!("result: {}", result);
}
