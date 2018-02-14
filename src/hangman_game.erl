-module(hangman_game).

-export([new/2,
         guess/2,
         info/1]).

-export_type([game/0,
              game_result/0]).

-record(game, {word,
               word_chars,
               tries,
               state=continue,
               guessed=sets:new()}).

-opaque game() :: #game{}.
-type game_result() :: won | lost | continue.

%%%===================================================================
%%% API
%%%===================================================================

-spec new(pos_integer(), nonempty_string()) -> game().
new(Tries, Word) when Tries > 0, length(Word) > 0 ->
    WordChars = sets:from_list(Word),
    #game{word = Word, word_chars = WordChars, tries = Tries}.

-spec guess(game(), char()) -> {game_result(), game()}.
guess(Game = #game{state = continue}, Char) ->
    NewGame = update_game(Char, Game),
    calculate_result(NewGame, Game);
guess(Game = #game{state = _}, _Char) ->
    calculate_result(Game, Game).

-spec info(game()) -> {game_result(), pos_integer(), nonempty_string()}.
info(#game{word = Word, state = State, tries = LivesLeft}) ->
    {State, LivesLeft, Word}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

calculate_result(NextGame = #game{state = continue}, PrevGame) ->
    NextTries = NextGame#game.tries,
    PrevTries = PrevGame#game.tries,
    if
        NextTries < PrevTries ->
            {miss, NextGame};
        NextTries == PrevTries ->
            {hit, NextGame}
    end;
calculate_result(NextGame = #game{state = State}, _) ->
    {State, NextGame}.

update_game(Char, Game = #game{guessed = Guessed, word_chars = WordChars}) ->
    % Always add new character to guessed
    % If the character is not part of the word, guessed will not be
    % a subset of word_chars
    NewGuessed = sets:add_element(Char, Guessed),
    case sets:is_subset(NewGuessed, WordChars) of
        true ->
            AllWordsMatch = WordChars == NewGuessed,
            check_won(AllWordsMatch, Game#game{guessed = NewGuessed});
        false ->
            NewTries = Game#game.tries - 1,
            check_lost(NewTries, Game)
    end.

check_won(true, Game) ->
    update_state(won, Game);
check_won(false, Game) ->
    update_state(continue, Game).

check_lost(0, Game) ->
    update_state(lost, Game#game{tries = 0});
check_lost(N, Game) ->
    update_state(continue, Game#game{tries = N}).

update_state(State, Game) ->
    Game#game{state = State}.

