pub const NUMBER_OF_PIECES: usize = 16;

pub const PAWN_START_RANK_WHITE: usize = 6;
pub const PAWN_START_RANK_BLACK: usize = 1;

pub const PROMOTION_RANK_WHITE: usize = 0;
pub const PROMOTION_RANK_BLACK: usize = 7;

pub const MAX_POSSIBLE_PAWN_MOVES: usize = 4;
pub const MAX_POSSIBLE_KNIGHT_MOVES: usize = 8;
pub const MAX_POSSIBLE_BISHOP_MOVES: usize = 13;
pub const MAX_POSSIBLE_ROOK_MOVES: usize = 14;
pub const MAX_POSSIBLE_KING_MOVES: usize = 8;

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

impl PieceColour {
    pub fn opposite(&self) -> Self {
        match self {
            PieceColour::None => PieceColour::None,
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
                    "P"
                } else {
                    "p"
                }
            }
            PieceType::Knight => {
                if self.colour == PieceColour::White {
                    "N"
                } else {
                    "n"
                }
            }
            PieceType::Bishop => {
                if self.colour == PieceColour::White {
                    "B"
                } else {
                    "b"
                }
            }
            PieceType::Rook(_) => {
                if self.colour == PieceColour::White {
                    "R"
                } else {
                    "r"
                }
            }
            PieceType::Queen => {
                if self.colour == PieceColour::White {
                    "Q"
                } else {
                    "q"
                }
            }
            PieceType::King(_) => {
                if self.colour == PieceColour::White {
                    "K"
                } else {
                    "k"
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

#[derive(PartialEq, Clone)]
pub(super) enum MoveType {
    Illegal(String),
    Normal,
    Capture(Position),
    PromotePawn(PieceType, Option<Position>),
    Castling(bool),
}

#[derive(PartialEq, Clone)]
pub struct Position {
    pub(super) file: usize,
    pub(super) rank: usize,
}

impl Position {
    pub(super) fn new(file: usize, rank: usize) -> Self {
        Self { file, rank }
    }
}

impl From<&[u8]> for Position {
    fn from(value: &[u8]) -> Self {
        Position::new((value[0] - 97) as usize, (8 - (value[1] - 48)) as usize)
    }
}
