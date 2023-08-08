use crate::chess::{Board, Position};
use crate::game_state::GameState;

use std::io::Write;

const CLEAR_SCREEN: &str = "\x1B[2J\x1B[1;1H";

pub fn run() {
    loop {
        let choice = input(
            r#"Choose game mode:
Player vs Computer: 1 [Coming soon]
Player 1 vs Player 2: 2
Custom setup(computer): 3 [Coming soon]
Custom setup(2 player): 4
Exit: 5
> "#,
        )
        .parse::<i32>()
        .unwrap_or_default();

        match choice {
            1 => single_player(&mut Board::new()),
            2 => two_players(&mut Board::new()),
            3 => custom_setup_single_player(),
            4 => custom_setup_two_players(),
            5 => break,
            _ => {
                print!("\x1B[2J\x1B[1;1H");
                println!("Enter a valid choice.");
                continue;
            }
        }

        if input("Play again? ") == "y" {
            print!("\x1B[2J\x1B[1;1H");
        } else {
            break;
        }
    }
}

fn single_player(_board: &mut Board) {
    todo!()
}

fn two_players(board: &mut Board) {
    let mut turn = true;

    while !board.game_over() {
        println!(
            "{CLEAR_SCREEN}{}\n{}",
            board.as_string(turn),
            if turn { "White's turn" } else { "Black's turn" }
        );

        loop {
            let from = match get_position("From: ") {
                Some(i) => {
                    if i == "resign" {
                        board.resign(turn);
                        break;
                    } else {
                        i
                    }
                }
                None => {
                    println!("Invalid position");
                    continue;
                }
            };

            let to = match get_position("To: ") {
                Some(i) => {
                    if i == "resign" {
                        board.resign(turn);
                        break;
                    } else {
                        i
                    }
                }
                None => {
                    println!("Invalid position");
                    continue;
                }
            };

            match board.move_piece(
                Position::from(from.as_bytes()),
                Position::from(to.as_bytes()),
                turn,
            ) {
                Ok(_) => {
                    turn = !turn;
                    break;
                }
                Err(i) => println!("{i}"),
            }
        }
    }

    print!("\x1B[2J\x1B[1;1H\n{}\n", board.as_string(turn));

    match board.state {
        GameState::Stalemate => println!("Stalemate"),
        GameState::WhiteWins => println!("White wins"),
        GameState::BlackWins => println!("Black wins"),
        _ => unreachable!(),
    }
}

fn custom_setup_single_player() {
    todo!()
}

fn custom_setup_two_players() {
    let mut board;

    loop {
        board = match Board::try_from(input("Enter the board string: ")) {
            Ok(i) => i,
            Err(_) => {
                println!("Enter a valid board state");
                continue;
            }
        };

        break;
    }

    two_players(&mut board);
}

fn input(msg: &str) -> String {
    print!("{msg}");
    std::io::stdout().flush().unwrap();

    let mut buffer = String::new();
    std::io::stdin().read_line(&mut buffer).unwrap();
    buffer.trim().to_string()
}

fn validate_position(pos: &str) -> Result<(), ()> {
    if pos.len() != 2 {
        return Err(());
    }

    let pos_bytes = pos.as_bytes();

    let file = pos_bytes[0];
    let rank = pos_bytes[1];

    if !(97..=104).contains(&file) || !(48..=56).contains(&rank) {
        return Err(());
    }

    Ok(())
}

fn get_position(msg: &str) -> Option<String> {
    let pos = input(msg).to_ascii_lowercase();

    match validate_position(&pos) {
        Ok(_) => Some(pos),
        Err(_) => {
            if pos == "resign" {
                Some(pos)
            } else {
                None
            }
        }
    }
}
