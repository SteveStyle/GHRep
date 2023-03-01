use std::cell;
use std::io;
use std::str::FromStr;

use scrabble::board::SCRABBLE_VARIANT_OFFICIAL;
use scrabble::board::SCRABBLE_VARIANT_WORDFEUD;
use scrabble::*;

use chrono::Local;
use scrabble::board::ScrabbleVariant;
use scrabble::pos::Position;
use scrabble::tiles::{Letter, TileList};
use scrabble::word_list::is_word;

use crate::utils::Timer;

//TO DO : use terminal escape codes to clear the screen
//https://stackoverflow.com/questions/2979383/c-clear-the-console
//https://stackoverflow.com/questions/4842424/list-of-ansi-color-escape-sequences
//https://stackoverflow.com/questions/2616906/how-do-i-output-coloured-text-to-a-linux-terminal

fn main() {
    //let mut game = Game::new(&SCRABBLE_VARIANT_OFFICIAL);
    //game.computer_move();

    menu_top();
}

fn menu_top() {
    loop {
        // clear the screen
        print!("\x1B[2J\x1B[1;1H");
        println!("1) Computer vs Computer");
        println!("2) Human vs Computer");
        println!("3) Human vs Human");
        println!("9) Look up word");
        println!("0) Exit");

        // read a char from stdin and compare to  1 to 4
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        match input.trim() {
            "1" => computer_vs_computer(),
            "2" => human_vs_computer(),
            "3" => human_vs_human(),

            "9" => look_up_word(),
            "0" => break,
            _ => println!("Invalid input"),
        }
    }
}

enum UserInputError {
    UserCancelled,
}

fn ad_hoc_game() -> Result<(), UserInputError> {
    let scrabble_variant = get_user_input_scrabble_variant()?;
    let no_players = get_user_input_integer("Enter number of players", "2", 2, 4)? as usize;
    let players = [Player::new(PlayerType::Human); 4];
    let player_name: Vec<String> = vec![];

    for i in 0..no_players {
        let player_type = get_user_input_player_type()?;
        let name = get_user_input_string(
            &format!("Enter name for player {}", i + 1),
            &format!("Player {}", i + 1),
        )?;

        players[i as usize] = Player::new(player_type);
        player_name.push(name);
    }

    let mut game = Game::new(&scrabble_variant, no_players, players, player_name);

    game.start();
    while !game.is_game_over() {
        match game.current_player().game_type {
            PlayerType::Human => {
                human_move(&mut game)?;
            }
            PlayerType::Computer => {
                computer_move(&mut game)?;
            }
        }
        println!("{}", game);
    }

    // print the final score

    Ok(())
}

fn get_user_input_string(caption: &str, default: &str) -> Result<String, UserInputError> {
    println!("{}\n(0 to cancel, return for {})", caption, default);
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    let input = input.trim().to_uppercase();
    match &input {
        "0" => Err(UserInputError::UserCancelled),
        "" => Ok(default.to_string()),
        _ => Ok(input),
    }
}

fn get_user_input_position(caption: &str, default: &str) -> Result<Position, UserInputError> {
    loop {
        let input = get_user_input_string(caption, default)?;
        match Position::from_str(&input) {
            Ok(p) => return Ok(p),
            Err(e) => println!("{}", e),
        }
    }
}

fn get_user_input_letter(caption: &str, default: &str) -> Result<Letter, UserInputError> {
    loop {
        let input = get_user_input_string(caption, default)?;
        match Letter::from_str(&input) {
            Ok(l) => return Ok(l),
            Err(e) => println!("{}", e),
        }
    }
}

fn get_user_input_tile_list(caption: &str, default: &str) -> Result<TileList, UserInputError> {
    loop {
        let input = get_user_input_string(caption, default)?;
        match TileList::from_str(&input) {
            Ok(t) => return Ok(t),
            Err(e) => println!("{}", e),
        }
    }
}

fn get_user_input_integer(
    caption: &str,
    default: &str,
    min: i32,
    max: i32,
) -> Result<i32, UserInputError> {
    loop {
        let input = get_user_input_string(caption, default)?;
        match input.parse::<i32>() {
            Ok(i) => {
                if i >= min && i <= max {
                    return Ok(i);
                } else {
                    println!("Please enter a number between {} and {}", min, max);
                }
            }
            Err(e) => println!("{}", e),
        }
    }
}

fn get_user_input_direction(caption: &str, default: &str) -> Result<Direction, UserInputError> {
    loop {
        let input = get_user_input_string(caption, default)?;
        match Direction::from_str(&input) {
            Ok(d) => return Ok(d),
            Err(e) => println!("{}", e),
        }
    }
}

fn get_user_input_bool(caption: &str, default: &str) -> Result<bool, UserInputError> {
    loop {
        let input = get_user_input_string(caption, default)?;
        match input.as_str() {
            "Y" => return Ok(true),
            "N" => return Ok(false),
            _ => println!("Please enter 'Y' or 'N'"),
        }
    }
}

fn get_user_input_scrabble_variant() -> Result<ScrabbleVariant, UserInputError> {
    loop {
        let input = get_user_input_string(
            "Which scrabble ruleset should we use?\n1) Official\n2)Wordfeud",
            "1",
        )?;
        match input.as_str() {
            "1" => return Ok(SCRABBLE_VARIANT_OFFICIAL),
            "2" => return Ok(SCRABBLE_VARIANT_WORDFEUD),
            _ => println!("Please enter '1' or '2'"),
        }
    }
}

fn get_user_input_player_type() -> Result<PlayerType, UserInputError> {
    loop {
        let input = get_user_input_string(
            "Which player type should we use?\n1) Human\n2) Computer",
            "1",
        )?;
        match input.as_str() {
            "1" => return Ok(PlayerType::Human),
            "2" => return Ok(PlayerType::Computer),
            _ => println!("Please enter '1' or '2'"),
        }
    }
}

fn computer_vs_computer() {
    let mut game = Game::new(&SCRABBLE_VARIANT_OFFICIAL);
    let mut pause: bool = false;

    let mut result1 = game.computer_move();
    // return for the next move, 'a' to play all moves without stopping
    while !game.is_over {
        if pause {
            println!("Enter 'a' to play all moves without stopping, or any other key to continue");
            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();
            match input.trim().to_uppercase().as_str() {
                "A" => {
                    pause = false;
                    break;
                }
                _ => {}
            }
        }
        println!("{}", game);
        result1 = game.computer_move();
    }

    println!("{}", game);
}

fn human_vs_computer() {
    let mut game = Game::new(&SCRABBLE_VARIANT_OFFICIAL);
    let mut timer_human = Timer::new(false);
    let mut timer_computer = Timer::new(false);

    let human_first: bool;
    loop {
        // ask who should move first?  the human or the computer?
        println!("Who should move first?  Enter 'h' for human or 'c' for computer");
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        match input.trim().to_uppercase().as_str() {
            "H" => {
                human_first = true;
                break;
            }
            "C" => {
                human_first = false;
                break;
            }
            _ => println!(
                "You entered {}.  I didn't understand that.  Please enter 'h' or 'c'\n",
                input.trim()
            ),
        }
    }

    if !human_first {
        game.computer_move();
    }
    while !game.is_over {
        human_move(&mut game);
        game.computer_move();
    }

    println!("{}", game);

    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
}

fn human_move(game: &mut Game) {
    if game.is_over {
        return;
    }
    let mut debug_info = String::new();
    loop {
        println!("{}", game);
        println!("{}", debug_info);
        println!("What would you like to do?");
        println!("1) Play a word");
        println!("2) Pass");
        println!("3) Exchange tiles");
        println!("4) Show cell info");
        println!("0) Quit");

        // read a char from stdin and compare to  1 to 4
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        match input.trim() {
            "1" => {
                play_word(game);
                break;
            }
            "2" => {
                game.pass();
                break;
            }
            "3" => {
                println!("Enter tiles to exchange: ");
                io::stdin().read_line(&mut input).unwrap();

                println!("You entered: {}", input);

                match game.exchange_tiles(&input.as_str().into()) {
                    Ok(_) => {
                        println!("{}", game);
                        break;
                    }
                    Err(e) => println!("Exchange rejected: {:?}", e),
                }
            }
            "4" => {
                //take a list of cells and show the info for each
                loop {
                    println!("Enter cell to show info for: ");
                    let mut input = String::new();
                    io::stdin().read_line(&mut input).unwrap();
                    let input = input.trim().to_uppercase();

                    println!("You entered: {}", input);

                    match Position::from_str(&input) {
                        Ok(p) => {
                            if game.board.is_valid_position(p) {
                                let cell = game.board.get_cell_pos(p);
                                debug_info.push_str(
                                    format!("Cell {} ({},{}) {:?}", &input, p.x, p.y, cell)
                                        .as_str(),
                                );
                                debug_info.push('\n');
                                break;
                            } else {
                                println!("Invalid position: {:?}", p);
                                continue;
                            }
                        }
                        Err(e) => {
                            println!("Invalid position: {:?}", e);
                            continue;
                        }
                    }
                }
            }
            "0" => {
                game.quit();
                break;
            }
            _ => println!("Invalid input"),
        }
    }
}

fn play_word(game: &mut Game) {
    // capture the starting cell
    let mut starting_position: Position;
    let mut direction: Direction;
    let mut tiles: String;

    loop {
        loop {
            println!("{}", game);
            let mut input = String::new();
            println!("Enter starting cell: ");
            io::stdin().read_line(&mut input).unwrap();
            let cell_string = input.trim().to_uppercase();
            if cell_string.len() < 2 {
                println!(
                "You entered {}.  I didn't understand that.  Please enter a cell like A1 or O15\n",
                cell_string
            );
                continue;
            }
            let column = cell_string.chars().nth(0).unwrap();
            match column {
                'A'..='O' => (),
                _ => {
                    println!("Invalid cell");
                    continue;
                }
            }
            let column: Letter = column.into();
            let column: u8 = column.as_byte();
            let mut row = String::new();
            row.push(cell_string.chars().nth(1).unwrap());
            if cell_string.len() > 2 {
                row.push(cell_string.chars().nth(2).unwrap());
            }
            match row.parse::<u8>() {
                Ok(r) => starting_position = Position::new(column, r - 1),
                Err(e) => {
                    println!("Invalid cell.  You entered {}. Please enter a cell like A1 or H15.\nError message: {}", cell_string, e);
                    continue;
                }
            }

            if !game.board.is_valid_position(starting_position) {
                println!("Invalid cell");
                continue;
            }
            break;
        }

        //capture the direction
        loop {
            println!("Enter direction (h or v): ");
            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();
            let direction_string = input.trim().to_uppercase();
            match direction_string.as_str() {
                "H" => {
                    direction = Direction::Horizontal;
                    break;
                }
                "V" => {
                    direction = Direction::Vertical;
                    break;
                }
                _ => println!(
                    "You entered {}.  I didn't understand that.  Please enter 'h' or 'v'\n",
                    direction_string
                ),
            }
        }

        println!("Enter tiles to play: ");
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        tiles = input.clone().trim().to_uppercase();

        match game.human_move(starting_position, direction, &tiles) {
            Ok(_) => {
                println!("{}", game);
                break;
            }
            Err(e) => {
                println!("Move rejected: {:?}", e);
                io::stdin().read_line(&mut input).unwrap();
            }
        }
    }
}

fn human_vs_human() {
    /*
    let mut game = Game::new(&SCRABBLE_VARIANT_OFFICIAL);
    let timer1 = Timer::new(false);

    let mut result1 = game.computer_move();
    while !game.is_over {
        let mut input = String::new();
        println!("Your rack is {} ", game.players[0].rack);
        println!("Enter a move: ");
        io::stdin().read_line(&mut input).unwrap();

        println!("You entered: {}", input);

        match game.human_move(&input, MoveType::Play) {
            Ok(_) => println!("{}", game),
            Err(e) => println!("Move rejected: {:?}", e),
        }

        let mut input = String::new();
        println!("Your rack is {} ", game.players[1].rack);
        println!("Enter a move: ");
        io::stdin().read_line(&mut input).unwrap();

        println!("You entered: {}", input);

        match game.human_move(&input, MoveType::Play) {
            Ok(_) => println!("{}", game),
            Err(e) => println!("Move rejected: {:?}", e),
        }
    }

    let elapsed = timer.elapsed();

    println!("{}", game);

    println!("Time taken: {} seconds", elapsed.as_secs_f64());
    */
}

fn look_up_word() {
    let mut input = String::new();
    println!("Enter a word: ");
    io::stdin().read_line(&mut input).unwrap();

    let result = is_word(&input.trim().to_uppercase());
    match result {
        true => println!("{input} is a word.  Follow the link for the definition:  https://www.collinsdictionary.com/dictionary/english/{}", input),
        false => println!("{} is not a word", input),
    }
}

fn check_word(word: &str) -> bool {
    let result = is_word(&word.trim().to_uppercase());
    match result {
        true => println!("{word} is a word.  Follow the link for the definition:  https://www.collinsdictionary.com/dictionary/english/{word}"),
        false => println!("{} is not a word", word),
    }
    result
}

/*
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
*/
