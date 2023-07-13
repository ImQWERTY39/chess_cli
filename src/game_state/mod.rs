#[derive(PartialEq)]
pub enum GameState {
    OnGoing,
    WhiteInCheck,
    BlackInCheck,
    WhiteWins,
    BlackWins,
    Stalemate,
}
