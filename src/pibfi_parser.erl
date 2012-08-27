%%% BEGIN pibfi_parser.erl %%%
%%%
%%% pibfi - Platonic Ideal Brainf*ck Interpreter
%%% Copyright (c)2003 Cat's Eye Technologies.  All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%   Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%%   Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in
%%%   the documentation and/or other materials provided with the
%%%   distribution.
%%%
%%%   Neither the name of Cat's Eye Technologies nor the names of its
%%%   contributors may be used to endorse or promote products derived
%%%   from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
%%% OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
%%% OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE. 

%% @doc Brainf*ck parser for <code>pibfi</code>.
%%
%% @end

-module(pibfi_parser).
-vsn('2003.0426').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([parse/2]).

%% @spec parse(string() | binary(), Options) -> {tuple(), string()}
%% @doc Transforms a string into a nested tuple data structure
%% suitable for interpretation.

parse(Binary, Options) when is_binary(Binary) ->
  parse(binary_to_list(Binary), Options);
parse(Source, Options) ->
  {DocBody, Source0} =
   case pibfi_options:get_option(Options, heredoc, undefined) of
    undefined ->
      {"", Source};
    [Marker] ->
      extract_heredoc(Source, Marker, false, [], [])
  end,
  Program = parse0(Source0),
  Exclude =  pibfi_options:get_option(Options, dontstrip, ""),
  Program0 = pibfi_stripper:strip(Program, Exclude),
  Program1 = case pibfi_options:get_option(Options, optimize, 1) of
    0 ->
      Program0;
    1 ->
      pibfi_optimizer:optimize(Program0)
  end,
  % ce_log:write("~p", [Program1]),
  {Program1, DocBody}.
  
parse0(String) ->
  TupleList = annotate(String),
  parse0({}, TupleList).

parse0(Tuple, []) -> Tuple;
parse0(Tuple, [{$], R, C} | Tail]) -> {Tuple, Tail};
parse0(Tuple, [{$[, R, C} | Tail]) ->
  {NewTuple, NewTail} = parse0({}, Tail),
  parse0(erlang:append_element(Tuple, {while, R, C, NewTuple}), NewTail);
parse0(Tuple, [{Head, R, C} | Tail]) ->
  parse0(erlang:append_element(Tuple, {instruction, R, C, Head}), Tail).

annotate(String) ->
  annotate(String, 1, 1, []).

annotate("", R, C, Acc) ->
  lists:reverse(Acc);
annotate("\n" ++ Tail, R, C, Acc) ->
  annotate(Tail, R+1, 1, Acc);
annotate([Head | Tail], R, C, Acc) ->
  annotate(Tail, R, C+1, [{Head, R, C} | Acc]).

extract_heredoc("", Marker, Found, AccD, AccS) ->
  {lists:reverse(AccD), lists:reverse(AccS)};
extract_heredoc([Marker | Tail], Marker, Found, AccD, AccS) ->
  extract_heredoc(Tail, Marker, true, AccD, AccS);
extract_heredoc([Head | Tail], Marker, true, AccD, AccS) ->
  extract_heredoc(Tail, Marker, true, [Head | AccD], AccS);
extract_heredoc([Head | Tail], Marker, false, AccD, AccS) ->
  extract_heredoc(Tail, Marker, false, AccD, [Head | AccS]).

%%% END of pibfi_parser.erl %%%
