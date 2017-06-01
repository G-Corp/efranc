-module(efranc_app).

-export([main/1]).

main(Args) ->
  case getopt:parse(opts(), Args) of
    {ok, {Options, Remain}} ->
      case lists:member(help, Options) of
        false ->
          run(Remain, create_options(Options));
        _ ->
          getopt:usage(opts(), "efranc")
      end;
    {error, {Reason, Data}} ->
      getopt:usage(opts(), "efranc", io_lib:format("Error (~p): ~s", [Reason, Data]))
  end,
  erlang:halt(0).

run([], _) ->
  ok;
run([Text|Rest], Options) ->
  io:format("~s | ~ts...~n",
            [
             color:red(efranc:detect(Text, Options)),
             color:cyan(unicode:characters_to_binary(string:sub_string(Text, 1, 77)))
            ]),
  run(Rest, Options).

create_options(Options) ->
  create_options(Options, #{}).
create_options([], Acc) ->
  Acc;
create_options([{whitelist, Lang}|Rest], #{whitelist := Whitelist} = Acc) ->
  create_options(Rest, Acc#{whitelist => [Lang|Whitelist]});
create_options([{whitelist, Lang}|Rest], Acc) ->
  create_options(Rest, Acc#{whitelist => [Lang]});
create_options([{blacklist, Lang}|Rest], #{blacklist := Blacklist} = Acc) ->
  create_options(Rest, Acc#{blacklist => [Lang|Blacklist]});
create_options([{blacklist, Lang}|Rest], Acc) ->
  create_options(Rest, Acc#{blacklist => [Lang]});
create_options([{min_size, Size}|Rest], Acc) ->
  create_options(Rest, Acc#{min_length => Size});
create_options([_|Rest], Acc) ->
  create_options(Rest, Acc).

opts() ->
  [
   {whitelist, $w, "whitelist", string,    "Whitelist"},
   {blacklist, $b, "blacklist", string,    "Blacklist"},
   {min_size,  $m, "min_size",  integer,   "Minimum size"},
   {help,      $h, "help",      undefined, "Display this help"}
  ].
