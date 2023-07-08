mod piece;
use crate::game_state::GameState;
use piece::*;

use std::fmt;

const BOARD_SIZE: usize = 8;
const DEFAULT_BOARD: &str = "rhbqkbhr/pppppppp/////PPPPPPPP/RHBQKBHR";

pub struct Board {
    board: Vec<Vec<Piece>>,
    white_pieces: Vec<(usize, usize)>,
    black_pieces: Vec<(usize, usize)>,
    moves: Vec<String>,
    pub state: GameState,
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

    pub fn move_piece(&mut self, from: &str, to: &str, turn: bool) -> Result<(), String> {
        let turn_colour = PieceColour::from(turn);
        let (from_file, from_rank) = position_as_index(from.as_bytes());
        let (to_file, to_rank) = position_as_index(to.as_bytes());

        match self.try_move(from, to, &turn_colour) {
            MoveType::Illegal(msg) => return Err(msg),
            MoveType::Normal => {
                self.set(to, self.get(from).clone());
                self.set_none(from);
            }
            MoveType::Capture(capture_file, capture_rank) => {
                if turn {
                    let position = self
                        .black_pieces
                        .iter()
                        .position(|x| x.0 == capture_file && x.1 == capture_rank)
                        .unwrap();
                    self.black_pieces.remove(position);
                } else {
                    let position = self
                        .white_pieces
                        .iter()
                        .position(|x| x.0 == capture_file && x.1 == capture_rank)
                        .unwrap();
                    self.white_pieces.remove(position);
                }

                self.set_none(&position_as_string(capture_file, capture_rank));
                self.set(to, self.get(from).clone());
                self.set_none(from);
            }
            MoveType::Castling(side) => {
                self.set_none(from);

                self.set(
                    to,
                    Piece {
                        piece_type: PieceType::King(false),
                        colour: turn.into(),
                    },
                );

                let (from_file_rook, to_file_rook) = if side { (7, 5) } else { (0, 3) };

                self.set(
                    &position_as_string(to_file, to_file_rook),
                    Piece {
                        piece_type: PieceType::Rook(false),
                        colour: turn.into(),
                    },
                );

                if turn {
                    let position = self
                        .white_pieces
                        .iter()
                        .position(|x| x.0 == from_file_rook && x.1 == from_rank)
                        .unwrap();
                    self.white_pieces[position] = (to_file_rook, to_rank);
                } else {
                    let position = self
                        .black_pieces
                        .iter()
                        .position(|x| x.0 == from_file_rook && x.1 == from_rank)
                        .unwrap();
                    self.black_pieces[position] = (to_file_rook, to_rank);
                }
            }
            MoveType::PromotePawn(to_piece_type) => {
                if from_file != to_file {
                    if turn {
                        let position = self
                            .black_pieces
                            .iter()
                            .position(|x| x.0 == to_file && x.1 == to_rank)
                            .unwrap();
                        self.black_pieces.remove(position);
                    } else {
                        let position = self
                            .white_pieces
                            .iter()
                            .position(|x| x.0 == to_file && x.1 == to_rank)
                            .unwrap();
                        self.white_pieces.remove(position);
                    }
                }

                self.set_none(from);
                self.set(
                    to,
                    Piece {
                        piece_type: to_piece_type,
                        colour: turn.into(),
                    },
                )
            }
        }

        if turn {
            let position = self
                .white_pieces
                .iter()
                .position(|x| x.0 == from_file && x.1 == from_rank)
                .unwrap();
            self.white_pieces[position] = (to_file, to_rank);
        } else {
            let position = self
                .black_pieces
                .iter()
                .position(|x| x.0 == from_file && x.1 == from_rank)
                .unwrap();
            self.black_pieces[position] = (to_file, to_rank);
        }

        Ok(())
    }
}

impl Board {
    fn get(&self, pos: &str) -> &Piece {
        let (file, rank) = position_as_index(pos.as_bytes());
        &self.board[rank][file]
    }

    fn set(&mut self, pos: &str, to_piece: Piece) {
        let (file, rank) = position_as_index(pos.as_bytes());
        let replace_piece = &mut self.board[rank][file];

        replace_piece.piece_type = match to_piece.piece_type {
            PieceType::King(true) => PieceType::King(false),
            PieceType::Rook(true) => PieceType::Rook(false),
            _ => to_piece.piece_type,
        };
        replace_piece.colour = to_piece.colour;
    }

    fn set_none(&mut self, pos: &str) {
        let (file, rank) = position_as_index(pos.as_bytes());
        self.board[rank][file] = Piece::default();
    }
}

// move checks
impl Board {
    fn try_move(&self, from: &str, to: &str, turn_colour: &PieceColour) -> MoveType {
        let from_piece = self.get(from);
        let to_piece = self.get(to);

        let (from_file, from_rank) = position_as_index(from.as_bytes());
        let (to_file, to_rank) = position_as_index(to.as_bytes());

        if from_piece.piece_type == PieceType::None {
            return MoveType::Illegal(String::from("No piece to move."));
        }

        if from_piece.colour != *turn_colour {
            return MoveType::Illegal(String::from("Cannot move your opponent's pieces."));
        }

        if self.get(to).colour == *turn_colour {
            return MoveType::Illegal(String::from("Square is occupied by your piece."));
        }

        let possible_moves: Vec<(usize, usize)> = self
            .get_possible_moves(from, turn_colour)
            .iter()
            .filter(|x| {
                self.is_valid_move(*x, from_file, from_rank, turn_colour)
                    && x.0 == to_file
                    && x.1 == to_rank
            })
            .map(|x| *x)
            .collect();

        if possible_moves.len() == 0 {
            return MoveType::Illegal(String::from("You cannot make that move"));
        }

        get_move_type(
            from_file,
            to_file,
            to_rank,
            &from_piece.piece_type,
            &to_piece.piece_type,
            turn_colour,
        )
    }

    fn is_attacked_square(
        &self,
        turn_colour: &PieceColour,
        square_file: usize,
        square_rank: usize,
    ) -> bool {
        todo!()
    }

    fn is_valid_move(
        &self,
        possible_move: &(usize, usize),
        from_file: usize,
        from_rank: usize,
        turn_colour: &PieceColour,
    ) -> bool {
        let (to_file, to_rank) = possible_move;
        let from_piece_type = &self
            .get(&position_as_string(from_file, from_rank))
            .piece_type;
        let to_piece_type = &self.get(&position_as_string(*to_file, *to_rank)).piece_type;

        match get_move_type(
            from_file,
            *to_file,
            *to_rank,
            from_piece_type,
            to_piece_type,
            turn_colour,
        ) {
            MoveType::Illegal(_) => unreachable!(),
            MoveType::Normal => todo!(),
            MoveType::Capture(_, _) => todo!(),
            MoveType::PromotePawn(_) => todo!(),
            MoveType::Castling(_) => true,
        }

        /*
        make move
        if self.is_in_check() {
            revert move
            true
        } else {
            revert move
            false
        }
        */
    }

    fn get_possible_moves(&self, from: &str, turn_colour: &PieceColour) -> Vec<(usize, usize)> {
        match self.get(from).piece_type {
            PieceType::None => unreachable!(),
            PieceType::Pawn => self.possible_pawn_moves(from, turn_colour),
            PieceType::Knight => self.possible_knight_moves(from, turn_colour),
            PieceType::Bishop => self.possible_bishop_moves(from, turn_colour),
            PieceType::Rook(_) => self.possible_rook_moves(from, turn_colour),
            PieceType::Queen => self.possible_queen_moves(from, turn_colour),
            PieceType::King(can_castle) => self.possible_king_moves(from, turn_colour, can_castle),
        }
    }
}

impl Board {
    fn possible_pawn_moves(&self, from: &str, colour: &PieceColour) -> Vec<(usize, usize)> {
        let (file_index, rank_index) = position_as_index(from.as_bytes());
        let mut possible_moves = Vec::with_capacity(MAX_POSSIBLE_PAWN_MOVES);

        match colour {
            PieceColour::None => unreachable!(),
            PieceColour::White => {
                if self
                    .get(&position_as_string(file_index, rank_index - 1))
                    .piece_type
                    == PieceType::None
                {
                    possible_moves.push((file_index, rank_index - 1));

                    if self
                        .get(&position_as_string(file_index, rank_index - 2))
                        .piece_type
                        == PieceType::None
                        && rank_index == 6
                    {
                        possible_moves.push((file_index, rank_index - 2));
                    }
                }

                let ((_, last_move_from_rank), (last_move_to_file, last_move_to_rank)) =
                    split_move(match self.moves.last() {
                        Some(i) => i,
                        None => return possible_moves,
                    });

                if file_index < 7 {
                    match self
                        .get(&position_as_string(file_index + 1, rank_index - 1))
                        .colour
                    {
                        PieceColour::None => {
                            if rank_index == 3 {
                                let piece = self
                                    .get(&position_as_string(last_move_to_file, last_move_to_rank));

                                if piece.colour == PieceColour::Black
                                    && piece.piece_type == PieceType::Pawn
                                    && last_move_to_file == file_index + 1
                                    && last_move_to_rank - last_move_from_rank == 2
                                {
                                    possible_moves.push((file_index + 1, rank_index - 1));
                                }
                            }
                        }
                        PieceColour::White => (),
                        PieceColour::Black => possible_moves.push((file_index + 1, rank_index - 1)),
                    }
                }

                if file_index > 0 {
                    match self
                        .get(&position_as_string(file_index - 1, rank_index - 1))
                        .colour
                    {
                        PieceColour::None => {
                            if rank_index == 3 {
                                let piece = self
                                    .get(&position_as_string(last_move_to_file, last_move_to_rank));

                                if piece.colour == PieceColour::Black
                                    && piece.piece_type == PieceType::Pawn
                                    && last_move_to_file == file_index - 1
                                    && last_move_to_rank - last_move_from_rank == 2
                                {
                                    possible_moves.push((file_index - 1, rank_index - 1));
                                }
                            }
                        }
                        PieceColour::White => (),
                        PieceColour::Black => possible_moves.push((file_index - 1, rank_index - 1)),
                    }
                }
            }
            PieceColour::Black => {
                if self
                    .get(&position_as_string(file_index, rank_index + 1))
                    .piece_type
                    == PieceType::None
                {
                    possible_moves.push((file_index, rank_index + 1));

                    if self
                        .get(&position_as_string(file_index, rank_index + 2))
                        .piece_type
                        == PieceType::None
                        && rank_index == 1
                    {
                        possible_moves.push((file_index, rank_index + 2));
                    }
                }

                let ((_, last_move_from_rank), (last_move_to_file, last_move_to_rank)) =
                    split_move(match self.moves.last() {
                        Some(i) => i,
                        None => return possible_moves,
                    });

                if file_index < 7 {
                    match self
                        .get(&position_as_string(file_index + 1, rank_index + 1))
                        .colour
                    {
                        PieceColour::None => {
                            if rank_index == 4 {
                                let piece = self
                                    .get(&position_as_string(last_move_to_file, last_move_to_rank));

                                if piece.colour == PieceColour::White
                                    && piece.piece_type == PieceType::Pawn
                                    && last_move_to_file == file_index + 1
                                    && last_move_from_rank - last_move_to_rank == 2
                                {
                                    possible_moves.push((file_index + 1, rank_index + 1));
                                }
                            }
                        }
                        PieceColour::White => (),
                        PieceColour::Black => possible_moves.push((file_index + 1, rank_index + 1)),
                    }
                }

                if file_index > 0 {
                    match self
                        .get(&position_as_string(file_index - 1, rank_index + 1))
                        .colour
                    {
                        PieceColour::None => {
                            if rank_index == 4 {
                                let piece = self
                                    .get(&position_as_string(last_move_to_file, last_move_to_rank));

                                if piece.colour == PieceColour::White
                                    && piece.piece_type == PieceType::Pawn
                                    && last_move_to_file == file_index - 1
                                    && last_move_from_rank - last_move_to_rank == 2
                                {
                                    possible_moves.push((file_index - 1, rank_index + 1));
                                }
                            }
                        }
                        PieceColour::White => (),
                        PieceColour::Black => possible_moves.push((file_index - 1, rank_index + 1)),
                    }
                }
            }
        }

        possible_moves
    }

    fn possible_knight_moves(&self, from: &str, colour: &PieceColour) -> Vec<(usize, usize)> {
        let mut all_possible_moves = Vec::with_capacity(MAX_POSSIBLE_KNIGHT_MOVES);
        let mut possible_moves = Vec::with_capacity(MAX_POSSIBLE_KNIGHT_MOVES);
        let (file_index, rank_index) = position_as_index(from.as_bytes());

        if file_index < 7 {
            if rank_index > 1 {
                all_possible_moves.push((file_index + 1, rank_index - 2));
            }

            if rank_index < 6 {
                all_possible_moves.push((file_index + 1, rank_index + 2));
            }
        }

        if file_index < 6 {
            if rank_index > 0 {
                all_possible_moves.push((file_index + 2, rank_index - 1));
            }

            if rank_index < 6 {
                all_possible_moves.push((file_index + 2, rank_index + 1));
            }
        }

        if file_index > 0 {
            if rank_index > 1 {
                all_possible_moves.push((file_index - 1, rank_index - 2));
            }

            if rank_index < 6 {
                all_possible_moves.push((file_index - 1, rank_index + 2));
            }
        }

        if file_index > 1 {
            if rank_index > 0 {
                all_possible_moves.push((file_index - 2, rank_index - 1));
            }

            if rank_index < 6 {
                all_possible_moves.push((file_index - 2, rank_index + 1));
            }
        }

        for i in all_possible_moves {
            if self.get(&position_as_string(i.0, i.1)).colour != *colour {
                possible_moves.push(i);
            }
        }

        possible_moves
    }

    fn possible_bishop_moves(&self, from: &str, colour: &PieceColour) -> Vec<(usize, usize)> {
        let mut possible_moves = Vec::with_capacity(MAX_POSSIBLE_BISHOP_MOVES);
        let (file_index, rank_index) = position_as_index(from.as_bytes());

        if file_index < 7 {
            if rank_index > 0 {
                let (mut t_file, mut t_rank) = (file_index + 1, rank_index - 1);

                while t_file < 8 {
                    let piece_colour = &self.get(&position_as_string(t_file, t_rank)).colour;

                    if piece_colour == colour {
                        break;
                    }

                    possible_moves.push((t_file, t_rank));

                    if *piece_colour == !colour.to_owned() {
                        break;
                    }

                    if t_rank == 0 {
                        break;
                    }

                    t_file += 1;
                    t_rank -= 1;
                }
            }

            if rank_index < 7 {
                let (mut t_file, mut t_rank) = (file_index + 1, rank_index + 1);

                while t_file < 8 && t_rank < 8 {
                    let piece_colour = &self.get(&position_as_string(t_file, t_rank)).colour;

                    if piece_colour == colour {
                        break;
                    }

                    possible_moves.push((t_file, t_rank));

                    if *piece_colour == !colour.to_owned() {
                        break;
                    }

                    t_file += 1;
                    t_rank += 1;
                }
            }
        }

        if file_index > 0 {
            if rank_index > 0 {
                let (mut t_file, mut t_rank) = (file_index - 1, rank_index - 1);

                loop {
                    let piece_colour = &self.get(&position_as_string(t_file, t_rank)).colour;

                    if piece_colour == colour {
                        break;
                    }

                    possible_moves.push((t_file, t_rank));

                    if *piece_colour == !colour.to_owned() {
                        break;
                    }

                    if t_file == 0 || t_rank == 0 {
                        break;
                    }

                    t_file -= 1;
                    t_rank -= 1;
                }
            }

            if rank_index < 7 {
                let (mut t_file, mut t_rank) = (file_index - 1, rank_index + 1);

                while t_rank < 8 {
                    let piece_colour = &self.get(&position_as_string(t_file, t_rank)).colour;

                    if piece_colour == colour {
                        break;
                    }

                    possible_moves.push((t_file, t_rank));

                    if *piece_colour == !colour.to_owned() {
                        break;
                    }

                    if t_file == 0 {
                        break;
                    }

                    t_file -= 1;
                    t_rank += 1;
                }
            }
        }

        possible_moves
    }

    fn possible_rook_moves(&self, from: &str, colour: &PieceColour) -> Vec<(usize, usize)> {
        let mut possible_moves = Vec::with_capacity(MAX_POSSIBLE_ROOK_MOVES);
        let (file_index, rank_index) = position_as_index(from.as_bytes());

        if file_index < 7 {
            let mut t_file = file_index + 1;
            while t_file < 8 {
                let piece_colour = &self.get(&position_as_string(t_file, rank_index)).colour;

                if piece_colour == colour {
                    break;
                }

                possible_moves.push((t_file, rank_index));

                if *piece_colour == !colour.to_owned() {
                    break;
                }

                t_file += 1;
            }
        }

        if file_index > 0 {
            let mut t_file = file_index - 1;
            loop {
                let piece_colour = &self.get(&position_as_string(t_file, rank_index)).colour;

                if piece_colour == colour {
                    break;
                }

                possible_moves.push((t_file, rank_index));

                if *piece_colour == !colour.to_owned() {
                    break;
                }

                if t_file == 0 {
                    break;
                }

                t_file -= 1;
            }
        }

        if rank_index < 7 {
            let mut t_rank = rank_index + 1;
            while t_rank < 8 {
                let piece_colour = &self.get(&position_as_string(file_index, t_rank)).colour;

                if piece_colour == colour {
                    break;
                }

                possible_moves.push((file_index, t_rank));

                if *piece_colour == !colour.to_owned() {
                    break;
                }

                t_rank += 1;
            }
        }

        if rank_index > 0 {
            let mut t_rank = rank_index - 1;
            loop {
                let piece_colour = &self.get(&position_as_string(file_index, t_rank)).colour;

                if piece_colour == colour {
                    break;
                }

                possible_moves.push((file_index, t_rank));

                if *piece_colour == !colour.to_owned() {
                    break;
                }

                if t_rank == 0 {
                    break;
                }

                t_rank -= 1;
            }
        }

        possible_moves
    }

    fn possible_queen_moves(&self, from: &str, colour: &PieceColour) -> Vec<(usize, usize)> {
        let mut possible_moves = self.possible_rook_moves(from, colour);
        possible_moves.append(&mut self.possible_bishop_moves(from, colour));

        possible_moves
    }

    fn possible_king_moves(
        &self,
        from: &str,
        colour: &PieceColour,
        can_castle: bool,
    ) -> Vec<(usize, usize)> {
        let mut all_possible_moves = Vec::with_capacity(MAX_POSSIBLE_KING_MOVES);
        let mut possible_moves = Vec::with_capacity(MAX_POSSIBLE_KING_MOVES);
        let (file_index, rank_index) = position_as_index(from.as_bytes());

        if rank_index > 0 {
            all_possible_moves.push((file_index, rank_index - 1));

            if file_index < 7 {
                all_possible_moves.push((file_index + 1, rank_index - 1));
            }

            if file_index > 0 {
                all_possible_moves.push((file_index - 1, rank_index - 1));
            }
        }

        if rank_index < 7 {
            all_possible_moves.push((file_index, rank_index + 1));

            if file_index < 7 {
                all_possible_moves.push((file_index + 1, rank_index + 1));
            }

            if file_index > 0 {
                all_possible_moves.push((file_index - 1, rank_index + 1));
            }
        }

        if file_index > 0 {
            all_possible_moves.push((file_index - 1, rank_index));
        }

        if file_index < 7 {
            all_possible_moves.push((file_index + 1, rank_index));
        }

        for i in all_possible_moves {
            if self.get(&position_as_string(i.0, i.1)).colour != *colour {
                possible_moves.push(i);
            }
        }

        if can_castle {
            match self
                .get(&position_as_string(file_index - 4, rank_index))
                .piece_type
            {
                PieceType::Rook(i) => {
                    if i && self
                        .get(&position_as_string(file_index - 1, rank_index))
                        .colour
                        == PieceColour::None
                        && self
                            .get(&position_as_string(file_index - 2, rank_index))
                            .colour
                            == PieceColour::None
                        && self
                            .get(&position_as_string(file_index - 3, rank_index))
                            .colour
                            == PieceColour::None
                    {
                        possible_moves.push((file_index - 2, rank_index));
                    }
                }
                _ => (),
            }

            match self
                .get(&position_as_string(file_index + 3, rank_index))
                .piece_type
            {
                PieceType::Rook(i) => {
                    if i && self
                        .get(&position_as_string(file_index + 1, rank_index))
                        .colour
                        == PieceColour::None
                        && self
                            .get(&position_as_string(file_index + 2, rank_index))
                            .colour
                            == PieceColour::None
                    {
                        possible_moves.push((file_index + 2, rank_index));
                    }
                }
                _ => (),
            }
        }

        possible_moves
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let letters = "    a   b   c   d   e   f   g   h\n";
        let sep_line_top = "  ┌───┬───┬───┬───┬───┬───┬───┬───┐\n";
        let sep_line = "  ├───┼───┼───┼───┼───┼───┼───┼───┤\n";
        let sep_line_bottom = "  └───┴───┴───┴───┴───┴───┴───┴───┘\n";

        let mut board_str = letters.to_string() + sep_line_top;

        for (idx, i) in self.board.iter().enumerate() {
            board_str += format!("{} ", BOARD_SIZE - idx).as_str();

            for j in i {
                board_str += format!("│ {} ", j.to_string()).as_str();
            }

            board_str += format!("│ {}\n", BOARD_SIZE - idx).as_str();
            board_str += sep_line;
        }

        board_str = board_str[..board_str.len() - sep_line.len()].to_string();
        board_str += (sep_line_bottom.to_owned() + letters).as_str();

        write!(f, "{board_str}")
    }
}

impl fmt::Debug for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let letters = "    h   g   f   e   d   c   b   a\n";
        let sep_line_top = "  ┌───┬───┬───┬───┬───┬───┬───┬───┐\n";
        let sep_line = "  ├───┼───┼───┼───┼───┼───┼───┼───┤\n";
        let sep_line_bottom = "  └───┴───┴───┴───┴───┴───┴───┴───┘\n";

        let mut board_str = letters.to_string() + sep_line_top;

        for (idx, i) in self.board.iter().rev().enumerate() {
            board_str += format!("{} ", idx + 1).as_str();

            for j in i.iter().rev() {
                board_str += format!("│ {} ", j.to_string()).as_str();
            }

            board_str += format!("│ {}\n", idx + 1).as_str();
            board_str += sep_line;
        }

        board_str = board_str[..board_str.len() - sep_line.len()].to_string();
        board_str += (sep_line_bottom.to_owned() + letters).as_str();

        write!(f, "{board_str}")
    }
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
                    PieceColour::White => board.white_pieces.push((file_index, rank_index)),
                    PieceColour::Black => board.black_pieces.push((file_index, rank_index)),
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

fn position_as_index(pos: &[u8]) -> (usize, usize) {
    ((pos[0] - 97) as usize, (8 - (pos[1] - 48)) as usize)
}

fn position_as_string(file: usize, rank: usize) -> String {
    format!(
        "{}{}",
        match file {
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
        BOARD_SIZE - rank
    )
}

fn split_move(moves: &str) -> ((usize, usize), (usize, usize)) {
    let moves_vec: Vec<&str> = moves.split(' ').collect();
    (
        position_as_index(moves_vec[0].as_bytes()),
        position_as_index(moves_vec[1].as_bytes()),
    )
}

fn safe_subtract(num1: usize, num2: usize) -> usize {
    if num1 >= num2 {
        num1 - num2
    } else {
        num2 - num1
    }
}

fn get_move_type(
    from_file: usize,
    to_file: usize,
    to_rank: usize,
    from_piece: &PieceType,
    to_piece: &PieceType,
    turn_colour: &PieceColour,
) -> MoveType {
    if (to_rank == 0 || to_rank == 7) && *from_piece == PieceType::Pawn {
        MoveType::PromotePawn(PieceType::Queen)
    } else if *to_piece == PieceType::None && *from_piece == PieceType::Pawn {
        MoveType::Capture(
            to_file,
            (to_rank as isize
                + if *turn_colour == PieceColour::White {
                    -1
                } else {
                    1
                }) as usize,
        )
    } else if *to_piece != PieceType::None {
        MoveType::Capture(to_file, to_rank)
    } else if safe_subtract(to_file, from_file) == 2 && *from_piece == PieceType::King(true) {
        MoveType::Castling(match from_file.checked_sub(to_file) {
            Some(_) => false,
            None => true,
        })
    } else {
        MoveType::Normal
    }
}
