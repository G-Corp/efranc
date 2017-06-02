% @hidden
-module(efranc_lang).
-include("../include/iso-639-3.hrl").

-export([lang/1]).

lang(Code) ->
  case lists:keyfind(Code, 1, ?ISO_639_3) of
    {Code, Part2B, Part2T, Part1, Scope, LanguageType, RefName, Comment} ->
      maps:fold(fun
                  (_, undefined, Acc) -> Acc;
                  (K, V, Acc) -> maps:put(K, V, Acc)
                end,
                #{},
                #{code => Code,
                  part_2b => Part2B,
                  part_2t => Part2T,
                  part_1 => Part1,
                  scope => Scope,
                  type => LanguageType,
                  ref => RefName,
                  comment => Comment});
    false ->
      undefined
  end.
