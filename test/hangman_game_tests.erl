-module(hangman_game_tests).
-include_lib("eunit/include/eunit.hrl").

run_test_() ->
    [{"Exhausting all tries should result in a loss", fail_till_lost()},
     {"Failing to guess and having remaining tries should continue the game", fail_continue()},
     {"Guessing correct char should continue the game", guess_char()},
     {"Guessing last correct char should result in a win", guess_and_win()},
     {"Guessing on a finished game should be a noop", guess_finished()},
     {"Retrieving information for a new game should match creation params", retrieve_info_new()}].

fail_till_lost() ->
    Word = "someWord",
    Game = hangman_game:new(1, Word),
    Res = hangman_game:guess(Game, $c),
    ?_assertMatch({lost, _}, Res).

fail_continue() ->
    Game = hangman_game:new(2, "someWord"),
    %% should have one try left
    Res = hangman_game:guess(Game, $c),
    ?_assertMatch({miss, _}, Res).

guess_char() ->
    Game = hangman_game:new(1, "has_a_c"),
    Res = hangman_game:guess(Game, $c),
    ?_assertMatch({hit, _}, Res).

guess_and_win() ->
    Word = "c",
    Game = hangman_game:new(1, Word),
    Res = hangman_game:guess(Game, $c),
    ?_assertMatch({won, _}, Res).

guess_finished() ->
    Game = hangman_game:new(1, "c"),
    {FinishState, FinishedGame} = hangman_game:guess(Game, $c),
    Res = hangman_game:guess(FinishedGame, $a),
    ?_assertMatch({FinishState, FinishedGame}, Res).

retrieve_info_new() ->
    Lives = 3,
    Word = "Hello",
    Game = hangman_game:new(Lives, Word),
    {InfoWord, InfoLives, _} = hangman_game:info(Game),
    ?_assertMatch({InfoWord, InfoLives}, {Word, Lives}).

give_up_result() ->
    Game = hangman_game:new(3, "some_word"),
    Res = hangman_game:give_up(Game),
    ?_assertMatch({lost, word, _}, Res).
