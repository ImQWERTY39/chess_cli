#[derive(PartialEq, Clone)]
pub enum GameState {
    OnGoing,
    Check,
    WhiteWins,
    BlackWins,
    Stalemate,
}
