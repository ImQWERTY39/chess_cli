mod piece;
use piece::*;

use crate::game_state::GameState;

const BOARD_SIZE: usize = 8;
const DEFAULT_BOARD: &str = "rhbqkbhr/pppppppp/////PPPPPPPP/RHBQKBHR";

pub struct Position {
    file: usize,
    rank: usize,
}

impl Position {
    fn new(file: usize, rank: usize) -> Self {
        Self { file, rank }
    }
}

pub struct Board {
    board: Vec<Vec<Piece>>,
    white_pieces: Vec<Position>,
    black_pieces: Vec<Position>,
    moves: Vec<String>,
    pub state: GameState,
}

impl From<&str> for Board {
    fn from(value: &str) -> Self {
        let mut board = Board {
            board: Vec::with_capacity(BOARD_SIZE),
            white_pieces: Vec::with_capacity(NUMBER_OF_PIECES),
            black_pieces: Vec::with_capacity(NUMBER_OF_PIECES),
            moves: Vec::new(),
            state: GameState::OnGoing,
        };

        let board_setup = value
            .split('/')
            .map(|x| x.chars().collect())
            .collect::<Vec<Vec<char>>>();

        for (rank_index, i) in board_setup.iter().enumerate() {
            let mut rank = Vec::with_capacity(BOARD_SIZE);

            for (file_index, j) in i.iter().enumerate() {
                let piece = Piece::from((*j, file_index, rank_index));

                match piece.colour {
                    PieceColour::None => (),
                    PieceColour::White => board
                        .white_pieces
                        .push(Position::new(file_index, rank_index)),
                    PieceColour::Black => board
                        .black_pieces
                        .push(Position::new(file_index, rank_index)),
                }

                rank.push(piece);
            }

            for _ in 0..BOARD_SIZE - rank.len() {
                rank.push(Piece::default());
            }

            board.board.push(rank);
        }

        for _ in 0..BOARD_SIZE - board.board.len() {
            let mut rank = Vec::with_capacity(BOARD_SIZE);

            for _ in 0..BOARD_SIZE {
                rank.push(Piece::default());
            }

            board.board.push(rank);
        }

        board
    }
}

impl TryFrom<String> for Board {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        if value.chars().filter(|x| *x == 'k').count() != 1 {
            return Err(String::from("You can only have 1 king"));
        }

        if value.chars().filter(|x| *x == 'K').count() != 1 {
            return Err(String::from("You can only have 1 king"));
        }

        Ok(Self::from(value.as_str()))
    }
}

impl Board {
    fn get(&self, pos: &Position) -> &Piece {
        &self.board[pos.rank][pos.file]
    }

    fn set(&mut self, pos: &Position, to_piece: Piece) {
        let replace_piece = &mut self.board[pos.rank][pos.file];

        replace_piece.piece_type = match to_piece.piece_type {
            PieceType::King(true) => PieceType::King(false),
            PieceType::Rook(true) => PieceType::Rook(false),
            _ => to_piece.piece_type,
        };
        replace_piece.colour = to_piece.colour;
    }

    fn set_none(&mut self, pos: &Position) {
        self.board[pos.rank][pos.file] = Piece::default();
    }
}

impl Board {
    pub fn new() -> Self {
        Board::from(DEFAULT_BOARD)
    }

    pub fn game_over(&self) -> bool {
        self.state == GameState::WhiteWins
            || self.state == GameState::BlackWins
            || self.state == GameState::Stalemate
    }

    pub fn resign(&mut self, turn: bool) {
        self.state = if turn {
            GameState::BlackWins
        } else {
            GameState::WhiteWins
        };
    }

    pub fn as_string(&self, turn: bool) -> String {
        let letters = if turn {
            "    a   b   c   d   e   f   g   h\n"
        } else {
            "    h   g   f   e   d   c   b   a\n"
        };
        let sep_line_top = "  ┌───┬───┬───┬───┬───┬───┬───┬───┐\n";
        let sep_line = "  ├───┼───┼───┼───┼───┼───┼───┼───┤\n";
        let sep_line_bottom = "  └───┴───┴───┴───┴───┴───┴───┴───┘\n";

        let mut board_str = letters.to_string() + sep_line_top;

        if turn {
            for (idx, i) in self.board.iter().enumerate() {
                board_str += format!("{} ", BOARD_SIZE - idx).as_str();

                for j in i {
                    board_str += format!("│ {} ", j.to_string()).as_str();
                }

                board_str += format!("│ {}\n", BOARD_SIZE - idx).as_str();
                board_str += sep_line;
            }
        } else {
            for (idx, i) in self.board.iter().rev().enumerate() {
                board_str += format!("{} ", idx + 1).as_str();

                for j in i.iter().rev() {
                    board_str += format!("│ {} ", j.to_string()).as_str();
                }

                board_str += format!("│ {}\n", idx + 1).as_str();
                board_str += sep_line;
            }
        }

        board_str = board_str[..board_str.len() - sep_line.len()].to_string();
        board_str += (sep_line_bottom.to_owned() + letters).as_str();

        board_str
    }

    pub fn move_piece(&mut self, from: &Position, to: &Position, turn: bool) -> Result<(), String> {
        let from_piece = self.get(from).clone();

        if from_piece.colour != turn.into() {
            if from_piece.piece_type == PieceType::None {
                return Err(String::from("There is no piece to move"));
            }

            return Err(String::from("You cannot move your opponent's piece"));
        }

        self.set_none(from);
        self.set(to, from_piece);

        Ok(())
    }
}

impl Board {
    fn all_pawn_moves(
        &self,
        from: &Position,
        piece_colour: &PieceColour,
    ) -> Vec<(Position, MoveType)> {
        todo!()
    }

    fn all_knight_moves(
        &self,
        from: &Position,
        piece_colour: &PieceColour,
    ) -> Vec<(Position, MoveType)> {
        todo!()
    }

    fn all_bishop_moves(
        &self,
        from: &Position,
        piece_colour: &PieceColour,
    ) -> Vec<(Position, MoveType)> {
        todo!()
    }

    fn all_rook_moves(
        &self,
        from: &Position,
        piece_colour: &PieceColour,
        can_castle: bool,
    ) -> Vec<(Position, MoveType)> {
        todo!()
    }

    fn all_queen_moves(
        &self,
        from: &Position,
        piece_colour: &PieceColour,
    ) -> Vec<(Position, MoveType)> {
        self.all_bishop_moves(from, piece_colour)
            .into_iter()
            .chain(self.all_rook_moves(from, piece_colour, false).into_iter())
            .collect()
    }

    fn all_king_moves(
        &self,
        from: &Position,
        piece_colour: &PieceColour,
        can_castle: bool,
    ) -> Vec<(Position, MoveType)> {
        todo!()
    }

    fn is_legal_position(&self) -> bool {
        todo!()
    }
}

pub fn position_as_index(pos: &[u8]) -> Position {
    Position::new((pos[0] - 97) as usize, (8 - (pos[1] - 48)) as usize)
}

fn position_as_str(pos: &Position) -> String {
    format!(
        "{}{}",
        match pos.file {
            0 => 'a',
            1 => 'b',
            2 => 'c',
            3 => 'd',
            4 => 'e',
            5 => 'f',
            6 => 'g',
            7 => 'h',
            _ => unreachable!(),
        },
        (pos.rank + 1)
    )
}
