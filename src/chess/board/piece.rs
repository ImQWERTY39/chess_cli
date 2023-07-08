use std::ops;

pub const NUMBER_OF_PIECES: usize = 16;

pub const MAX_POSSIBLE_PAWN_MOVES: usize = 4;
pub const MAX_POSSIBLE_KNIGHT_MOVES: usize = 8;
pub const MAX_POSSIBLE_BISHOP_MOVES: usize = 13;
pub const MAX_POSSIBLE_ROOK_MOVES: usize = 14;
pub const MAX_POSSIBLE_KING_MOVES: usize = 8;

pub const KNIGHT_NOTATION: &str = "N";
pub const BISHOP_NOTATION: &str = "B";
pub const ROOK_NOTATION: &str = "N";
pub const QUEEN_NOTATION: &str = "N";
pub const KING_NOTATION: &str = "N";

pub const SHORT_CASTLE_NOTATION: &str = "O-O";
pub const LONG_CASTLE_NOTATION: &str = "O-O-O";

pub const CAPTURE_NOTATION: &str = "x";
pub const CHECK_NOTATION: &str = "+";
pub const CHECKMATE_NOTATION: &str = "#";
pub const PROMOTION_TO_NOTATION: &str = "=";

#[derive(PartialEq, Clone)]
pub(super) enum PieceType {
    None,
    Pawn,
    Knight,
    Bishop,
    Rook(bool),
    Queen,
    King(bool),
}

#[derive(PartialEq, Clone)]
pub(super) enum PieceColour {
    None,
    White,
    Black,
}

impl From<bool> for PieceColour {
    fn from(value: bool) -> Self {
        if value {
            PieceColour::White
        } else {
            PieceColour::Black
        }
    }
}

impl ops::Not for PieceColour {
    type Output = PieceColour;

    fn not(self) -> Self::Output {
        match self {
            PieceColour::None => self,
            PieceColour::White => PieceColour::Black,
            PieceColour::Black => PieceColour::White,
        }
    }
}

#[derive(Clone)]
pub(super) struct Piece {
    pub(super) piece_type: PieceType,
    pub(super) colour: PieceColour,
}

impl ToString for Piece {
    fn to_string(&self) -> String {
        match self.piece_type {
            PieceType::None => " ",
            PieceType::Pawn => {
                if self.colour == PieceColour::White {
                    "♟"
                } else {
                    "♙"
                }
            }
            PieceType::Knight => {
                if self.colour == PieceColour::White {
                    "♞"
                } else {
                    "♘"
                }
            }
            PieceType::Bishop => {
                if self.colour == PieceColour::White {
                    "♝"
                } else {
                    "♗"
                }
            }
            PieceType::Rook(_) => {
                if self.colour == PieceColour::White {
                    "♜"
                } else {
                    "♖"
                }
            }
            PieceType::Queen => {
                if self.colour == PieceColour::White {
                    "♛"
                } else {
                    "♕"
                }
            }
            PieceType::King(_) => {
                if self.colour == PieceColour::White {
                    "♚"
                } else {
                    "♔"
                }
            }
        }
        .to_owned()
    }
}

impl Default for Piece {
    fn default() -> Self {
        Piece {
            piece_type: PieceType::None,
            colour: PieceColour::None,
        }
    }
}

impl From<(char, usize, usize)> for Piece {
    fn from(value: (char, usize, usize)) -> Self {
        Self {
            piece_type: match value.0.to_ascii_lowercase() {
                'p' => PieceType::Pawn,
                'h' => PieceType::Knight,
                'b' => PieceType::Bishop,
                'q' => PieceType::Queen,
                'k' => PieceType::King(
                    value.1 == 4 && (value.2 == if value.0.is_ascii_uppercase() { 7 } else { 0 }),
                ),
                'r' => PieceType::Rook(
                    (value.1 == 0 || value.1 == 7)
                        && (value.2 == if value.0.is_ascii_uppercase() { 7 } else { 0 }),
                ),
                _ => PieceType::None,
            },
            colour: if value.0 == '.' {
                PieceColour::None
            } else {
                value.0.is_ascii_uppercase().into()
            },
        }
    }
}

#[derive(PartialEq)]
pub(super) enum MoveType {
    Illegal(String),
    Normal,
    Capture(usize, usize),
    PromotePawn(PieceType),
    Castling(bool),
}
