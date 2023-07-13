use std::iter::zip;

mod piece;
use piece::*;

pub use piece::Position;

use crate::game_state::GameState;

const BOARD_SIZE: usize = 8;
const DEFAULT_BOARD: &str = "rhbqkbhr/pppppppp/////PPPPPPPP/RHBQKBHR";

pub struct Board {
    board: Vec<Vec<Piece>>,
    white_pieces: Vec<Position>,
    black_pieces: Vec<Position>,
    moves: Vec<String>,
    last_move: Option<(Position, Position)>,
    pub state: GameState,
}

impl From<&str> for Board {
    fn from(value: &str) -> Self {
        let mut board = Board {
            board: Vec::with_capacity(BOARD_SIZE),
            white_pieces: Vec::with_capacity(NUMBER_OF_PIECES),
            black_pieces: Vec::with_capacity(NUMBER_OF_PIECES),
            moves: Vec::new(),
            last_move: None,
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
                    PieceColour::White => board
                        .white_pieces
                        .push(Position::new(file_index, rank_index)),
                    PieceColour::Black => board
                        .black_pieces
                        .push(Position::new(file_index, rank_index)),
                    PieceColour::None => (),
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
        if value.split('/').count() != 8 {
            Err(String::from("Invalid FEN"))
        } else {
            Ok(Self::from(value.as_str()))
        }
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

    pub fn move_piece(&mut self, from: Position, to: Position, turn: bool) -> Result<(), String> {
        // add to self.moves

        match self.try_move(&from, &to, turn.into()) {
            MoveType::Illegal(i) => return Err(i),
            MoveType::Normal => {}
            MoveType::Capture(_) => {}
            MoveType::PromotePawn(_, _) => {}
            MoveType::Castling(_) => {}
        }

        self.set(&to, self.get(&from).clone());
        self.set_none(&from);

        self.last_move = Some((from, to));

        // self.check_state();

        Ok(())
    }
}

impl Board {
    fn try_move(&self, from: &Position, to: &Position, turn_colour: PieceColour) -> MoveType {
        let from_piece = self.get(from).clone();
        let to_piece = self.get(to).clone();

        if from_piece.colour != turn_colour {
            if from_piece.piece_type == PieceType::None {
                return MoveType::Illegal(String::from("There is no piece to move"));
            }

            return MoveType::Illegal(String::from("You cannot move your opponent's piece"));
        }

        if to_piece.colour == turn_colour {
            return MoveType::Illegal(String::from("You cannot capture your own pieces"));
        }

        let legal_moves = match from_piece.piece_type {
            PieceType::None => unreachable!(),
            PieceType::Pawn => self.all_pawn_moves(from, &turn_colour),
            PieceType::Knight => self.all_knight_moves(from, &turn_colour),
            PieceType::Bishop => self.all_bishop_moves(from, &turn_colour),
            PieceType::Rook(_) => self.all_rook_moves(from, &turn_colour),
            PieceType::Queen => self.all_queen_moves(from, &turn_colour),
            PieceType::King(can_castle) => self.all_king_moves(from, &turn_colour, can_castle),
        };

        let (_, to_move_type) = match legal_moves.iter().find(|(position, _)| position == to) {
            Some(i) => i,
            None => {
                return MoveType::Illegal(format!(
                    "You cannot move this piece to {}",
                    position_as_str(to)
                ))
            }
        };

        /*if !self.is_legal_position(from, to, to_move_type) {
            return MoveType::Illegal(String::from("It is not a legal move"));
        }*/

        to_move_type.clone()
    }

    fn all_pawn_moves(
        &self,
        from: &Position,
        turn_colour: &PieceColour,
    ) -> Vec<(Position, MoveType)> {
        let mut possible_moves = Vec::with_capacity(MAX_POSSIBLE_PAWN_MOVES);
        let front_one;
        let front_two;
        let capture_left;
        let capture_right;

        if *turn_colour == PieceColour::White {
            front_one = Position::new(from.file, from.rank - 1);

            front_two = if from.rank == PAWN_START_RANK_WHITE {
                Some(Position::new(from.file, from.rank - 2))
            } else {
                None
            };

            capture_left = if from.file > 0 {
                Some(Position::new(from.file - 1, from.rank - 1))
            } else {
                None
            };

            capture_right = if from.file < 7 {
                Some(Position::new(from.file + 1, from.rank - 1))
            } else {
                None
            };
        } else {
            front_one = Position::new(from.file, from.rank + 1);

            front_two = if from.rank == PAWN_START_RANK_BLACK {
                Some(Position::new(from.file, from.rank + 2))
            } else {
                None
            };

            capture_left = if from.file > 0 {
                Some(Position::new(from.file - 1, from.rank + 1))
            } else {
                None
            };

            capture_right = if from.file < 7 {
                Some(Position::new(from.file + 1, from.rank + 1))
            } else {
                None
            };
        }

        if self.get(&front_one).piece_type == PieceType::None {
            if front_one.rank == PROMOTION_RANK_WHITE || front_one.rank == PROMOTION_RANK_BLACK {
                possible_moves.push((front_one, MoveType::PromotePawn(PieceType::Queen, None)));
            } else {
                possible_moves.push((front_one, MoveType::Normal));

                match front_two {
                    Some(i) => {
                        if self.get(&i).piece_type == PieceType::None {
                            possible_moves.push((i, MoveType::Normal));
                        }
                    }
                    None => (),
                }
            }
        }

        match capture_left {
            Some(capture_left) => {
                let en_passant = match &self.last_move {
                    Some(last_move) => {
                        last_move.0
                            == Position::new(
                                capture_left.file,
                                if *turn_colour == PieceColour::White {
                                    PAWN_START_RANK_BLACK
                                } else {
                                    PAWN_START_RANK_WHITE
                                },
                            )
                            && last_move.1
                                == Position::new(
                                    capture_left.file,
                                    if *turn_colour == PieceColour::White {
                                        capture_left.rank + 1
                                    } else {
                                        capture_left.rank - 1
                                    },
                                )
                    }
                    None => false,
                };

                if self.get(&capture_left).colour == turn_colour.opposite() {
                    possible_moves.push((
                        capture_left.clone(),
                        if capture_left.rank == PROMOTION_RANK_WHITE
                            || capture_left.rank == PROMOTION_RANK_BLACK
                        {
                            MoveType::PromotePawn(PieceType::Queen, Some(capture_left))
                        } else {
                            MoveType::Capture(capture_left)
                        },
                    ))
                } else if en_passant
                    && ((*turn_colour == PieceColour::White && from.rank == 3)
                        || (*turn_colour == PieceColour::Black && from.rank == 4))
                {
                    possible_moves.push((
                        capture_left.clone(),
                        MoveType::Capture(Position::new(
                            capture_left.file,
                            if *turn_colour == PieceColour::White {
                                capture_left.rank + 1
                            } else {
                                capture_left.rank - 1
                            },
                        )),
                    ))
                }
            }
            None => (),
        }

        match capture_right {
            Some(capture_right) => {
                let en_passant = match &self.last_move {
                    Some(last_move) => {
                        last_move.0
                            == Position::new(
                                capture_right.file,
                                if *turn_colour == PieceColour::White {
                                    PAWN_START_RANK_BLACK
                                } else {
                                    PAWN_START_RANK_WHITE
                                },
                            )
                            && last_move.1
                                == Position::new(
                                    capture_right.file,
                                    if *turn_colour == PieceColour::White {
                                        capture_right.rank + 1
                                    } else {
                                        capture_right.rank - 1
                                    },
                                )
                    }
                    None => false,
                };

                if self.get(&capture_right).colour == turn_colour.opposite() {
                    possible_moves.push((
                        capture_right.clone(),
                        if capture_right.rank == PROMOTION_RANK_WHITE
                            || capture_right.rank == PROMOTION_RANK_BLACK
                        {
                            MoveType::PromotePawn(PieceType::Queen, Some(capture_right))
                        } else {
                            MoveType::Capture(capture_right)
                        },
                    ))
                } else if en_passant
                    && ((*turn_colour == PieceColour::White && from.rank == 3)
                        || (*turn_colour == PieceColour::Black && from.rank == 4))
                {
                    possible_moves.push((
                        capture_right.clone(),
                        MoveType::Capture(Position::new(
                            capture_right.file,
                            if *turn_colour == PieceColour::White {
                                capture_right.rank + 1
                            } else {
                                capture_right.rank - 1
                            },
                        )),
                    ))
                }
            }
            None => (),
        }

        possible_moves
    }

    fn all_knight_moves(
        &self,
        from: &Position,
        turn_colour: &PieceColour,
    ) -> Vec<(Position, MoveType)> {
        let mut all_moves = Vec::with_capacity(MAX_POSSIBLE_KNIGHT_MOVES);

        if from.file > 0 && from.rank > 1 {
            all_moves.push(Position::new(from.file - 1, from.rank - 2));
        }

        if from.file < 7 && from.rank > 1 {
            all_moves.push(Position::new(from.file + 1, from.rank - 2));
        }

        if from.file > 1 && from.rank > 0 {
            all_moves.push(Position::new(from.file - 2, from.rank - 1));
        }

        if from.file < 6 && from.rank > 0 {
            all_moves.push(Position::new(from.file + 2, from.rank - 1));
        }

        if from.file > 0 && from.rank < 6 {
            all_moves.push(Position::new(from.file - 1, from.rank + 2));
        }

        if from.file < 7 && from.rank < 6 {
            all_moves.push(Position::new(from.file + 1, from.rank + 2));
        }

        if from.file > 1 && from.rank < 7 {
            all_moves.push(Position::new(from.file - 2, from.rank + 1));
        }

        if from.file < 6 && from.rank < 7 {
            all_moves.push(Position::new(from.file + 2, from.rank + 1));
        }

        all_moves
            .iter()
            .map(|to_square| {
                let colour_on_check_square = &self.get(&to_square).colour;
                if *colour_on_check_square == turn_colour.opposite() {
                    Some((to_square.clone(), MoveType::Capture(to_square.clone())))
                } else if *colour_on_check_square == PieceColour::None {
                    Some((to_square.clone(), MoveType::Normal))
                } else {
                    None
                }
            })
            .filter(|is_possible_move| is_possible_move.is_some())
            .map(|possible_move| possible_move.unwrap())
            .collect()
    }

    fn all_bishop_moves(
        &self,
        from: &Position,
        turn_colour: &PieceColour,
    ) -> Vec<(Position, MoveType)> {
        let mut possible_moves = Vec::with_capacity(MAX_POSSIBLE_BISHOP_MOVES);

        for (f, r) in zip((0..from.file).rev(), (0..from.rank).rev()) {
            let current_position = Position::new(f, r);
            let on_position_piece = self.get(&current_position);

            if on_position_piece.colour == PieceColour::None {
                possible_moves.push((current_position, MoveType::Normal))
            } else if on_position_piece.colour == turn_colour.opposite() {
                possible_moves.push((
                    current_position.clone(),
                    MoveType::Capture(current_position),
                ));
                break;
            } else {
                break;
            }
        }

        for (f, r) in zip((from.file + 1)..BOARD_SIZE, (0..from.rank).rev()) {
            let current_position = Position::new(f, r);
            let on_position_piece = self.get(&current_position);

            if on_position_piece.colour == PieceColour::None {
                possible_moves.push((current_position, MoveType::Normal))
            } else if on_position_piece.colour == turn_colour.opposite() {
                possible_moves.push((
                    current_position.clone(),
                    MoveType::Capture(current_position),
                ));
                break;
            } else {
                break;
            }
        }

        for (f, r) in zip((0..from.file).rev(), (from.rank + 1)..BOARD_SIZE) {
            let current_position = Position::new(f, r);
            let on_position_piece = self.get(&current_position);

            if on_position_piece.colour == PieceColour::None {
                possible_moves.push((current_position, MoveType::Normal))
            } else if on_position_piece.colour == turn_colour.opposite() {
                possible_moves.push((
                    current_position.clone(),
                    MoveType::Capture(current_position),
                ));
                break;
            } else {
                break;
            }
        }

        for (f, r) in zip((from.file + 1)..BOARD_SIZE, (from.rank + 1)..BOARD_SIZE) {
            let current_position = Position::new(f, r);
            let on_position_piece = self.get(&current_position);

            if on_position_piece.colour == PieceColour::None {
                possible_moves.push((current_position, MoveType::Normal))
            } else if on_position_piece.colour == turn_colour.opposite() {
                possible_moves.push((
                    current_position.clone(),
                    MoveType::Capture(current_position),
                ));
                break;
            } else {
                break;
            }
        }

        possible_moves
    }

    fn all_rook_moves(
        &self,
        from: &Position,
        turn_colour: &PieceColour,
    ) -> Vec<(Position, MoveType)> {
        let mut possible_moves = Vec::with_capacity(MAX_POSSIBLE_BISHOP_MOVES);

        for file in (0..from.file).rev() {
            let current_position = Position::new(file, from.rank);
            let on_position_piece = self.get(&current_position);

            if on_position_piece.colour == PieceColour::None {
                possible_moves.push((current_position, MoveType::Normal))
            } else if on_position_piece.colour == turn_colour.opposite() {
                possible_moves.push((
                    current_position.clone(),
                    MoveType::Capture(current_position),
                ));
                break;
            } else {
                break;
            }
        }

        for file in (from.file + 1)..BOARD_SIZE {
            let current_position = Position::new(file, from.rank);
            let on_position_piece = self.get(&current_position);

            if on_position_piece.colour == PieceColour::None {
                possible_moves.push((current_position, MoveType::Normal))
            } else if on_position_piece.colour == turn_colour.opposite() {
                possible_moves.push((
                    current_position.clone(),
                    MoveType::Capture(current_position),
                ));
                break;
            } else {
                break;
            }
        }

        for rank in (0..from.rank).rev() {
            let current_position = Position::new(from.file, rank);
            let on_position_piece = self.get(&current_position);

            if on_position_piece.colour == PieceColour::None {
                possible_moves.push((current_position, MoveType::Normal))
            } else if on_position_piece.colour == turn_colour.opposite() {
                possible_moves.push((
                    current_position.clone(),
                    MoveType::Capture(current_position),
                ));
                break;
            } else {
                break;
            }
        }

        for rank in (from.rank + 1)..BOARD_SIZE {
            let current_position = Position::new(from.file, rank);
            let on_position_piece = self.get(&current_position);

            if on_position_piece.colour == PieceColour::None {
                possible_moves.push((current_position, MoveType::Normal))
            } else if on_position_piece.colour == turn_colour.opposite() {
                possible_moves.push((
                    current_position.clone(),
                    MoveType::Capture(current_position),
                ));
                break;
            } else {
                break;
            }
        }

        possible_moves
    }

    fn all_queen_moves(
        &self,
        from: &Position,
        turn_colour: &PieceColour,
    ) -> Vec<(Position, MoveType)> {
        self.all_bishop_moves(from, turn_colour)
            .into_iter()
            .chain(self.all_rook_moves(from, turn_colour).into_iter())
            .collect()
    }

    fn all_king_moves(
        &self,
        from: &Position,
        turn_colour: &PieceColour,
        can_castle: bool,
    ) -> Vec<(Position, MoveType)> {
        todo!()
    }

    fn is_legal_position(&self, from: &Position, to: &Position, to_move_type: &MoveType) -> bool {
        todo!()
    }

    fn is_protected_square(&self, square: &Position, turn_colour: &PieceColour) -> bool {
        todo!()
    }

    fn check_state(&mut self) {
        todo!()
    }
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
        (BOARD_SIZE - pos.rank)
    )
}
