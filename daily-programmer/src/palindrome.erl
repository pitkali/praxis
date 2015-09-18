%%%-------------------------------------------------------------------
%%% @author Piotr Kalinowski
%%% @doc
%%%
%%% @end
%%% Created : 18. sep 2015 19:18
%%%-------------------------------------------------------------------
-module(palindrome).
-author("Piotr Kalinowski").

%% API
-export([is_palindrome/1]).

is_palindrome(S) ->
  L = unicode:characters_to_list(S),
  R = lists:reverse(L),
  L == R.

