%%% 'antonius' chess engine
%%% Copyright (C) 2013 Rabbe Fogelholm
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.



-module(test_SUITE).


-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([castling_short_positive/1]).
-export([castling_short_negative_1/1]).
-export([castling_short_negative_2/1]).
-export([castling_short_negative_3/1]).
-export([castling_short_negative_4/1]).
-export([castling_short_negative_5/1]).
-export([castling_long_positive/1]).
-export([castling_long_positive_2/1]).
-export([castling_long_negative_5/1]).
-export([castling_possible_move_rook/1]).
-export([castling_possible_move_king/1]).
-export([castling_when_checked/1]).
-export([depth_3_evaluation/1]).
-export([promotion_1/1]).
-export([promotion_2/1]).
-export([promotion_3/1]).
-export([simple_opening_game/1]).
-export([simple_opening_game_4/1]).
-export([black_does_immediate_checkmate/1]).
-export([immediate_checkmate_2/1]).
-export([en_passant_1/1]).
-export([en_passant_2/1]).
-export([en_passant_3/1]).
-export([engine_plays_white/1]).
-export([black_claims_repetition_1/1]).
-export([black_claims_repetition_1g/1]).
-export([black_claims_repetition_2/1]).
-export([black_claims_repetition_2g/1]).
-export([black_claims_repetition_9/1]).
-export([stalemate_avoidance/1]).
-export([reference_test/1]).
-export([ref8_test/1]).
-export([ref10_test/1]).
-export([ref10_timing_test/1]).
-export([ref11_single_move_depth2_test/1]).
-export([ref11_single_move_depth3_test/1]).
-export([ref11_single_move_test/1]).
-export([ref11_test/1]).
-export([ref12_test/1]).
-export([ref16_test/1]).


all() ->
	[
	 {group, quickTests},
	 {group, longTests}
	].



groups() ->
	[
	 {quickTests, [], [
					   castling_short_positive,
					   castling_short_negative_1,
					   castling_short_negative_2,
					   castling_short_negative_3,
					   castling_short_negative_4,
					   castling_short_negative_5,
					   castling_long_positive,
					   castling_long_positive_2,
					   castling_long_negative_5,
					   castling_possible_move_rook,
					   castling_possible_move_king,
					   castling_when_checked,
					   depth_3_evaluation,
					   promotion_1,
					   promotion_2,
					   promotion_3,
					   simple_opening_game,
					   simple_opening_game_4,
					   black_does_immediate_checkmate,
					   immediate_checkmate_2,
					   en_passant_1,
					   en_passant_2,
					   en_passant_3,
					   engine_plays_white,
					   black_claims_repetition_1,
					   black_claims_repetition_1g,
					   black_claims_repetition_2,
					   black_claims_repetition_2g,
					   black_claims_repetition_9,
					   stalemate_avoidance
					  ]},
	 {longTests, [], [
					  reference_test,
					  ref8_test,
					  ref10_test,
					  ref10_timing_test,
					  ref11_single_move_depth2_test,
					  ref11_single_move_depth3_test,
					  ref11_single_move_test,
					  ref11_test,
					  ref12_test,
					  ref16_test
					 ]},
	 {performanceTests, [], [
					  ref10_timing_test,
					  ref16_test
					 ]}
	].


castling_short_positive(C) ->        test_adapter:run(fileInfo(C), castling_short_positive).
castling_short_negative_1(C) ->      test_adapter:run(fileInfo(C), castling_short_negative_1).
castling_short_negative_2(C) ->      test_adapter:run(fileInfo(C), castling_short_negative_2).
castling_short_negative_3(C) ->      test_adapter:run(fileInfo(C), castling_short_negative_3).
castling_short_negative_4(C) ->      test_adapter:run(fileInfo(C), castling_short_negative_4).
castling_short_negative_5(C) ->      test_adapter:run(fileInfo(C), castling_short_negative_5).
castling_long_positive(C) ->         test_adapter:run(fileInfo(C), castling_long_positive).
castling_long_positive_2(C) ->       test_adapter:run(fileInfo(C), castling_long_positive_2).
castling_long_negative_5(C) ->       test_adapter:run(fileInfo(C), castling_long_negative_5).
castling_possible_move_rook(C) ->    test_adapter:run(fileInfo(C), castling_possible_move_rook).
castling_possible_move_king(C) ->    test_adapter:run(fileInfo(C), castling_possible_move_king).
castling_when_checked(C) ->          test_adapter:run(fileInfo(C), castling_when_checked).
depth_3_evaluation(C) ->             test_adapter:run(fileInfo(C), depth_3_evaluation).
promotion_1(C) ->                    test_adapter:run(fileInfo(C), promotion_1).
promotion_2(C) ->                    test_adapter:run(fileInfo(C), promotion_2).
promotion_3(C) ->                    test_adapter:run(fileInfo(C), promotion_3).
simple_opening_game(C) ->            test_adapter:run(fileInfo(C), simple_opening_game).
simple_opening_game_4(C) ->          test_adapter:run(fileInfo(C), simple_opening_game_4).
black_does_immediate_checkmate(C) -> test_adapter:run(fileInfo(C), black_does_immediate_checkmate).
immediate_checkmate_2(C) ->          test_adapter:run(fileInfo(C), immediate_checkmate_2).
en_passant_1(C) ->                   test_adapter:run(fileInfo(C), en_passant_1).
en_passant_2(C) ->                   test_adapter:run(fileInfo(C), en_passant_2).
en_passant_3(C) ->                   test_adapter:run(fileInfo(C), en_passant_3).
engine_plays_white(C) ->             test_adapter:run(fileInfo(C), engine_plays_white).
black_claims_repetition_1(C) ->      test_adapter:run(fileInfo(C), black_claims_repetition_1).
black_claims_repetition_1g(C) ->     test_adapter:run(fileInfo(C), black_claims_repetition_1g).
black_claims_repetition_2(C) ->      test_adapter:run(fileInfo(C), black_claims_repetition_2).
black_claims_repetition_2g(C) ->     test_adapter:run(fileInfo(C), black_claims_repetition_2g).
black_claims_repetition_9(C) ->      test_adapter:run(fileInfo(C), black_claims_repetition_9).
stalemate_avoidance(C) ->            test_adapter:run(fileInfo(C), stalemate_avoidance).

reference_test(C) ->                 test_adapter:run(fileInfo(C), reference_test).
ref8_test(C) ->                      test_adapter:run(fileInfo(C), ref8_test).
ref10_test(C) ->                     test_adapter:run(fileInfo(C), ref10_test).
ref10_timing_test(C) ->              test_adapter:run(fileInfo(C), ref10_timing_test).
ref11_single_move_depth2_test(C) ->  test_adapter:run(fileInfo(C), ref11_single_move_depth2_test).
ref11_single_move_depth3_test(C) ->  test_adapter:run(fileInfo(C), ref11_single_move_depth3_test).
ref11_single_move_test(C) ->         test_adapter:run(fileInfo(C), ref11_single_move_test).
ref11_test(C) ->                     test_adapter:run(fileInfo(C), ref11_test).
ref12_test(C) ->                     test_adapter:run(fileInfo(C), ref12_test).
ref16_test(C) ->                     test_adapter:run(fileInfo(C), ref16_test).



init_per_suite(Config) ->
    cli_game:initVM(),
    ResultFile = ct:get_config(resultFile),
    {ok, ResultDevice} = file:open(ResultFile, [write]),
    ok = file:close(ResultDevice),
    [{resultFile, ResultFile}|Config].

init_per_testcase(_T, C) ->
    {resultFile, ResultFile} = lists:keyfind(resultFile, 1, C),
    {ok, ResultDevice} = file:open(ResultFile, [append]),
    [{resultDevice, ResultDevice}|C].
    
end_per_testcase(_T, C) ->
    {resultDevice, ResultDevice} = lists:keyfind(resultDevice, 1, C),
    file:close(ResultDevice).


fileInfo(Config) ->
	{resultDevice, ResultDevice} = lists:keyfind(resultDevice, 1, Config),
	{ct:get_config(testDir), ResultDevice}.

