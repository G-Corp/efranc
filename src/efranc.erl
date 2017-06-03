-module(efranc).
-include("../include/expressions.hrl").
-include("../include/data.hrl").

-export([detect/1, detect/2, detect_all/1, detect_all/2]).

-type options() :: #{min_length => integer(),
                     whitelist => [efranc_lang:code()],
                     blacklist => [efranc_lang:code()],
                     details => true | false}.

-define(MAX_DIFFERENCE, 300).
-define(MIN_LENGTH, 10).
-define(MAX_LENGTH, 2048).

% @equiv detect(Value, #{})
-spec detect(string()) -> efranc_lang:code().
detect(Value) ->
  detect(Value, #{}).

% @doc
% Detect the language of text. Return the ISO-639-3 code.
%
% Options:
% <ul>
% <li><tt>min_length: integer()</tt> : minimum length to accept (default: 10)</li>
% <li><tt>withlist: [string()]</tt> : allow languages (default: all)</li>
% <li><tt>blacklist: [string()]</tt> : disallow languages (default: none)</li>
% <li><tt>details: true | false</tt> : return ISO-639-3 details (default: false)</li>
% </ul>
% @end
-spec detect(string(), options()) -> efranc_lang:code() | efranc_lang:iso6393().
detect(Value, Options) ->
  case detect_all(Value, Options) of
    [{ISO, _}|_] ->
      ISO;
    Other ->
      Other
  end.

% @equiv detect_all(Value, #{})
-spec detect_all(string()) -> [{efranc_lang:code(), float()}].
detect_all(Value) ->
  detect_all(Value, #{}).

% @doc
% Detect the language of text. Return a list of ISO-639-3 codes with weight.
%
% Options:
% <ul>
% <li><tt>min_length: integer()</tt> : minimum length to accept (default: 10)</li>
% <li><tt>withlist: [string()]</tt> : allow languages (default: all)</li>
% <li><tt>blacklist: [string()]</tt> : disallow languages (default: none)</li>
% </ul>
% @end
-spec detect_all(string(), options()) -> [{efranc_lang:code() | efranc_lang:iso6393(), float()}].
detect_all(Value, Options) when is_list(Value) ->
  Options0 = #{min_length := MinLength} = maps:merge(#{min_length => ?MIN_LENGTH,
                                                       whitelist => [all],
                                                       blacklist => [],
                                                       details => false},
                                                     Options),
  All = filter_languages(
          case string:len(Value) < MinLength of
            true ->
              undefined;
            false ->
              Value0 = string:sub_string(Value, 1, ?MAX_LENGTH),
              {Script, Weight} = get_top_script(Value0),
              case maps:get(Script, ?DATA, undefined) of
                undefined ->
                  if
                    Weight == 0 -> undefined;
                    true -> [{Script, Weight}]
                  end;
                Script3Grams ->
                  normalize(
                    Value0,
                    get_distances(ngrams_to_map(trigrams(Value0)),
                                  Script3Grams))
              end
          end,
          Options0),
  case maps:get(details, Options0, false) of
    true ->
      [{efranc_lang:info(ISO), Weight} || {ISO, Weight} <- All];
    false ->
      All
  end.

normalize(_, []) ->
  undefined;
normalize(Value, [{_, Min}|_] = Distances) ->
  Max = (string:len(Value) * ?MAX_DIFFERENCE) - Min,
  case Max =< 0 of
    true ->
      undefined;
    _ ->
      normalize(Distances, Min, Max)
  end.

normalize([], _, _) ->
  [];
normalize([{ISO, Distance}|Rest], Min, Max) ->
  [{ISO, 1 - ((Distance - Min) / Max)}|normalize(Rest, Min, Max)].

get_distances(TriGrams, Languages) ->
  lists:sort(fun({_, A}, {_, B}) -> A < B end,
             maps:fold(fun(Lang, Grams, Acc) ->
                           [{Lang, get_distance(maps:to_list(TriGrams), Grams)}|Acc]
                       end,
                       [],
                       Languages)).

filter_languages(undefined, _) ->
  undefined;
filter_languages(Languages, #{whitelist := [all], blacklist := []}) ->
  Languages;
filter_languages(Languages, #{whitelist := WhiteList, blacklist := BlackList}) ->
  case lists:foldr(fun({K, _} = Lng, Acc) ->
                case (lists:member(all, WhiteList) orelse
                      lists:member(K, WhiteList)) andalso
                     not lists:member(K, BlackList) of
                  true ->
                    [Lng|Acc];
                  false ->
                    Acc
                end
            end, [], Languages) of
    [] -> undefined;
    Else -> Else
  end.


get_distance(TriGrams, Grams) ->
  X = string:tokens(Grams, "|"),
  Y = lists:seq(0, length(X) - 1),
  Model = lists:zip(X, Y),
  get_distance(TriGrams, Model, 0).

get_distance([], _, Distance) ->
  Distance;
get_distance([{TriGram, Count}|Rest], Model, Distance) ->
  Difference = case lists:keyfind(TriGram, 1, Model) of
                 false -> ?MAX_DIFFERENCE;
                 {_, Value} -> erlang:abs(Count - Value - 1)
               end,
  get_distance(Rest, Model, Distance + Difference).

get_top_script(Value) ->
  maps:fold(fun(Script, RE, {S, W}) ->
                Weight = case re:run(Value, RE, [unicode, global]) of
                           {match, Matchs} ->
                             length(Matchs) / string:len(Value);
                           nomatch ->
                             0
                         end,
                if
                  Weight > W -> {Script, Weight};
                  true -> {S, W}
                end
            end, {undefined, -1}, ?EXPRESSIONS).

ngrams_to_map(NGrams) ->
  lists:foldl(fun(Grams, Acc) ->
                  case maps:get(Grams, Acc, undefined) of
                    undefined ->
                      maps:put(Grams, 1, Acc);
                    N ->
                      maps:put(Grams, N + 1, Acc)
                  end
              end, #{}, NGrams).

trigrams(Value) ->
  ngrams(Value, 3).

ngrams(Value, N) ->
  Value0 = " " ++ clean(Value) ++ " ",
  Index = string:len(Value0) - N + 1,
  grams(Index, N, Value0, []).

grams(0, _, _, Acc) ->
  Acc;
grams(Index, N, Value, Acc) ->
  grams(Index - 1, N, Value, [string:sub_string(Value, Index, Index + N - 1)|Acc]).

clean(Value) ->
  string:to_lower(
    string:strip(
      lists:flatten(
        io_lib:format("~ts", [
                              re:replace(
                                re:replace(Value, "[\\x{0021}-\\x{0040}]+", " ", [global, unicode]),
                                "\\s+", " ", [global, unicode])
                             ])),
      both, 32)).

