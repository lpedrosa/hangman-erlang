-module(hangman_game).

-export([new/2,
         guess/2]).

-record(hangman, {word,
                  word_chars,
                  tries,
                  state=continue,
                  guessed=sets:new()}).

%%%===================================================================
%%% API
%%%===================================================================

new(Tries, Word) when Tries > 0, length(Word) > 0 ->
    WordChars = sets:from_list(Word),
    #hangman{word = Word, word_chars = WordChars, tries = Tries}.

guess(Char, Game = #hangman{state = continue}) ->
    NewGame = update_game(Char, Game),
    {NewGame#hangman.state, NewGame};
guess(_Char, Game = #hangman{state = GameState}) ->
    {GameState, Game}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

update_game(Char, Game = #hangman{guessed = Guessed, word_chars = WordChars}) ->
    % Always add new character to guessed
    % If the character is not part of the word, guessed will not be
    % a subset of word_chars
    NewGuessed = sets:add_element(Char, Guessed),
    case sets:is_subset(NewGuessed, WordChars) of
        true ->
            AllWordsMatch = WordChars == NewGuessed,
            check_won(AllWordsMatch, Game#hangman{guessed = NewGuessed});
        false ->
            NewTries = Game#hangman.tries - 1,
            check_lost(NewTries, Game)
    end.


check_won(true, Game) ->
    update_state(won, Game);
check_won(false, Game) ->
    update_state(continue, Game).

check_lost(0, Game) ->
    update_state(lost, Game#hangman{tries = 0});
check_lost(N, Game) ->
    update_state(continue, Game#hangman{tries = N}).

update_state(State, Game) ->
    Game#hangman{state = State}.


% Word functions

random_word(Words) when length(Words) > 0 ->
    Index = rand:uniform(length(Words)),
    lists:nth(Index, Words).

words() ->
    ["abacus", "pinapple", "orthopedic", "stuff"].
