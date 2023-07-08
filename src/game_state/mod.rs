#[derive(PartialEq)]
pub enum GameState {
    OnGoing,
    WhiteInCheck(usize, usize),
    BlackInCheck(usize, usize),
    WhiteWins,
    BlackWins,
    Stalemate,
}
