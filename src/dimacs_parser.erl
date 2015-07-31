%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc DIMACS file parser for Erlang
%%% @end
%%% @author Markus Ekholm <markus@botten.org>
%%% @copyright 2014 (c) Markus Ekholm <markus@botten.org>
%%% @license Copyright (c) 2014, Markus Ekholm
%%% All rights reserved.
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of the <organization> nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL MARKUS EKHOLM BE LIABLE FOR ANY
%%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Module declaration

-module(dimacs_parser).

%%=============================================================================
%% Exports

-export([ parse_file/1 ]).

%%=============================================================================
%% Include

-include_lib("eunit/include/eunit.hrl").

%%=============================================================================
%% Types

%% Generic types
-type dimacs_file()  :: {problem_type(), comments(), problem()}.
-type problem_type() :: cnf.
-type comments()     :: string().
-type problem()      :: cnf_formula().

%% CNF types
-type cnf_formula()  :: [cnf_clause()]. %% Clauses joined by AND
-type cnf_clause()   :: [literal()]. %% Literals joined by OR
-type literal()      :: pos_atom() | neg_atom().
-type pos_atom()     :: pos_integer().
-type neg_atom()     :: neg_integer().

%%=============================================================================
%% Records

-record(cnf_meta, { num_clauses :: integer()
                  , num_vars    :: integer()
                  }).

-type cnf_meta() :: #cnf_meta{}.

-record(s, { rest              :: string()
           , problem_type      :: problem_type()
           , comments     = [] :: [string()]
           , problem           :: problem()
           , problem_meta      :: cnf_meta()
           }).

-type s() :: #s{}.

%%=============================================================================
%% Export types

%% Generic types
-export_type([ dimacs_file/0
             , problem_type/0
             , comments/0
             , problem/0
             ]).

%% CNF specific types
-export_type([ cnf_formula/0
             , cnf_clause/0
             , literal/0
             , pos_atom/0
             , neg_atom/0
             ]).

%%=============================================================================
%% API functions

%% @doc Parse file and return result. Limitations: only support CNF type
-spec parse_file(string()) -> dimacs_file().
parse_file(In) ->
  {ok, InFile}  = file:read_file(In),
  parse_str(binary_to_list(InFile)).

%%=============================================================================
%% Internal functions

%% @doc Parse string and return result
-spec parse_str(string()) -> dimacs_file().
parse_str(Str) ->
  Funs      = [ fun parse_comments/1
              , fun parse_problem_type/1
              , fun parse_problem/1
              , fun validate_s/1
              ],
  S = lists:foldl(fun(Fun, Cnf) -> Fun(Cnf) end, #s{rest = Str}, Funs),
  { S#s.problem_type
  , string:join(S#s.comments, "\n")
  , lists:reverse(S#s.problem)}.

%%------------------------------------------------------------------------------
%% Top-level parser functions

-spec parse_comments(s()) -> s().
parse_comments(#s{rest="c" ++  Rest} = S)   ->
  parse_comments(parse_comment(S#s{rest=Rest}));
parse_comments(#s{comments = Comments} = S) ->
  S#s{comments = lists:reverse(Comments)}.

-spec parse_problem_type(s()) -> s().
parse_problem_type(#s{rest = "p cnf " ++ Rest0} = S) ->
  parse_problem_description(cnf, Rest0, S).

-spec parse_problem(s()) -> s().
parse_problem(#s{rest=""} = S)            -> S;
parse_problem(S = #s{problem_type = cnf}) -> parse_problem(parse_clause(S, [])).

%% @hidden Assert the sanity of the final parser state (only run when
%%         the assert macros are defined.
-spec validate_s(s()) -> s().
validate_s(#s{problem_type = cnf, problem_meta = Meta} = S) ->
  ?assertEqual("",
               S#s.rest),
  ?assertEqual(Meta#cnf_meta.num_clauses,
               length(S#s.problem)),
  ?assertEqual(Meta#cnf_meta.num_vars,
               length(lists:usort(lists:map(fun abs/1,
                                            lists:flatten(S#s.problem))))),
  S.

%%------------------------------------------------------------------------------
%% Generic parsing functions

-spec parse_problem_description(problem_type(), string(), s()) -> s().
parse_problem_description(cnf, Rest0, S) ->
  {NumVars    , Rest1}        = parse_int("", Rest0),
  {NumClauses , "\n" ++ Rest} = parse_int("", Rest1),
  S#s{ problem_type = cnf
     , rest         = Rest
     , problem      = []
     , problem_meta = #cnf_meta{num_vars = NumVars, num_clauses = NumClauses}
     }.

-spec parse_comment(s()) -> s().
parse_comment(#s{rest = Rest0, comments = Comments} = S) ->
  {Comment, Rest} = parse_string(Rest0, ""),
  S#s{rest = Rest, comments = [lists:reverse(Comment) | Comments]}.

-spec parse_string(string(), string()) -> string().
parse_string([$\n | Rest], Str) -> {Str, Rest};
parse_string([C   | Rest], Str) -> parse_string(Rest, [C | Str]).

-spec parse_int(string(), string()) -> integer().
parse_int(Int, [$\n | _] = Rest) ->
  {list_to_integer(lists:reverse(Int)), Rest};
parse_int(Int, [$  | Rest]) ->
  {list_to_integer(lists:reverse(Int)), Rest};
parse_int(Int, [I | Rest])
  when I >= $0 andalso I =< $9;
       I == $-
       ->
  parse_int([I | Int], Rest).

%%------------------------------------------------------------------------------
%% CNF specific parsing functions

-spec parse_clause(s(), string()) -> s().
parse_clause(#s{rest = [$\n | Rest], problem=Cnf} = S, Clause) ->
  S#s{rest = Rest, problem = [lists:reverse(Clause) | Cnf]};
parse_clause(#s{rest = [$0 | Rest]} = S, Clause)               ->
  parse_clause(S#s{rest = Rest}, Clause);
parse_clause(#s{rest = [$  | Rest]} = S, Clause)               ->
  parse_clause(S#s{rest = Rest}, Clause);
parse_clause(#s{rest = Rest0} = S, Clause)                     ->
  {Int, Rest} = parse_int("", Rest0),
  parse_clause(S#s{rest = Rest}, [Int | Clause]).

%%=============================================================================
%% Test cases

%%------------------------------------------------------------------------------
%% CNF files

parse_cnf_file_test_() ->
  TestDir     = code:lib_dir(dimacs_parser) ++ "/test/files/",
  {ok, Files} = file:list_dir(TestDir),
  lists:map(fun(F) ->
                {F, ?_assertMatch({cnf, _, _}, parse_file(TestDir ++ F))}
            end,
            Files).

parse_file_test_() ->
  TestDir  = code:lib_dir(dimacs_parser) ++ "/test/files/",
  F        = TestDir ++ "simple_v3_c2.cnf",
  ?_assertEqual({cnf, "  simple_v3_c2.cnf\n", [[1, -3], [2, 3, -1]]},
                parse_file(F)).


%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
