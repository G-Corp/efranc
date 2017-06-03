-module(efranc_lang).
-include("../include/iso-639-3.hrl").

-export([info/1]).
-export_type([code/0, iso6393/0]).

-type code() :: string().
-type iso6393() :: #{code => string(),
                     part_2b => string(),
                     part_2t => string(),
                     part_1 => string(),
                     scope => individual | macrolanguage | special,
                     type => ancient | constructed | extinct | historical | living | special,
                     ref => string(),
                     comment => string()}.

% @doc
% Return details for an ISO-369-3 code.
% @end
-spec info(code()) -> iso6393().
info(Code) ->
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
