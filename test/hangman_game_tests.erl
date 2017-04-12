-module(hangman_game_tests).
-include_lib("eunit/include/eunit.hrl").

run_test_() ->
    [{"Exhausting all tries should result in a loss", fail_till_lost()},
     {"Failing to guess and having remaining tries should continue the game", fail_continue()},
     {"Guessing correct char should continue the game", guess_char()},
     {"Guessing last correct char should result in a win", guess_and_win()},
     {"Guessing on a finished game should be a noop", guess_finished()}].

fail_till_lost() ->
    Game = hangman_game:new(1, "someWord"),
    Res = hangman_game:guess(Game, $c),
    ?_assertMatch({lost, _}, Res).

fail_continue() ->
    Game = hangman_game:new(2, "someWord"),
    %% should have one try left
    Res = hangman_game:guess(Game, $c),
    ?_assertMatch({continue, _}, Res).

guess_char() ->
    Game = hangman_game:new(1, "has_a_c"),
    Res = hangman_game:guess(Game, $c),
    ?_assertMatch({continue, _}, Res).

guess_and_win() ->
    Game = hangman_game:new(1, "c"),
    Res = hangman_game:guess(Game, $c),
    ?_assertMatch({won, _}, Res).

guess_finished() ->
    Game = hangman_game:new(1, "c"),
    {_, FinishedGame} = hangman_game:guess(Game, $c),
    Res = hangman_game:guess(FinishedGame, $a),
    ?_assertMatch({_, FinishedGame}, Res).
