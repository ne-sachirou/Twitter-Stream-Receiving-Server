-module(unilib).
-vsn(1.0).
-author('Yuki Nitta <yuki@nit2a.com>').

-export([utf8list_to_binary/1, utf8binary_to_list/1, utf8_to_binary/1]).



%% Convert from unicode character to binary in each element of List
utf8list_to_binary(Unicodes) ->
	utf8list_to_binary(Unicodes, []).

utf8list_to_binary([], Converted) ->
	Converted;
utf8list_to_binary([Unicode | T], Converted) ->
	ConvertedUnicode = unicode:characters_to_binary(Unicode),
	NewConverted = [ConvertedUnicode | Converted],
	utf8list_to_binary(T, NewConverted).

%% Convert from unicode binary to list in each element of List
utf8binary_to_list(Unicodes) ->
	utf8binary_to_list(Unicodes, []).

utf8binary_to_list([], Converted) ->
	Converted;
utf8binary_to_list([Unicode | T], Converted) ->
	ConvertedUnicode = unicode:characters_to_list(Unicode),
	NewConverted = [ConvertedUnicode | Converted],
	utf8binary_to_list(T, NewConverted).

%% All binarization
utf8_to_binary(Unicodes) ->
	utf8_to_binary(Unicodes, []).

utf8_to_binary([], Converted) ->
	Converted;
utf8_to_binary([Unicode | T], Converted) ->
	ConvertedUnicode = list_to_binary(Unicode),
	NewConverted = [ConvertedUnicode | Converted],
	utf8_to_binary(T, NewConverted).
