# File: EFranc.ex
# This file was generated from efranc.beam
# Using rebar3_elixir (https://github.com/botsunit/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule EFranc do
  def unquote(:"detect")(arg1) do
    :erlang.apply(:"efranc", :"detect", [arg1])
  end
  def unquote(:"detect")(arg1, arg2) do
    :erlang.apply(:"efranc", :"detect", [arg1, arg2])
  end
  def unquote(:"detect_all")(arg1) do
    :erlang.apply(:"efranc", :"detect_all", [arg1])
  end
  def unquote(:"detect_all")(arg1, arg2) do
    :erlang.apply(:"efranc", :"detect_all", [arg1, arg2])
  end
end
