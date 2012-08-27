%%% BEGIN pibfi_tape_dynarray.erl %%%
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

%% @doc dynarray backend for tape ADT for <code>pibfi</code>.
%%
%% @end

-module(pibfi_tape_dynarray).
-vsn('2003.0427').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-behaviour(pibfi_tape).

-export([new/1]).
-export([left/2, right/2, increment/2, decrement/2]).
-export([read/1, write/2, peek/2, poke/3, head/1]).

%% @spec new([option()]) -> tape()
%%         option() = {atom(), term()}
%%         tape() = tape()
%% @doc Creates and returns a new dynarray-backed tape.

new(Options) ->
  0 = pibfi_options:get_option(Options, mintape, 0),
  {1, dynarray:new()}.

%% @spec left(tape(), N::integer()) -> {tape(), integer()}
%% @doc Moves the read/write head N positions left on the tape.

left({Position, DynArray}, N) ->
  {{Position - N, DynArray}, lookup(Position - N, DynArray)}.

%% @spec right(tape(), N::integer()) -> {tape(), integer()}
%% @doc Moves the read/write head N positions right on the tape.

right({Position, DynArray}, N) ->
  {{Position + N, DynArray}, lookup(Position + N, DynArray)}.

%% @spec read(tape()) -> integer()
%% @doc Returns the value at the current position on the tape.

read({Position, DynArray}) -> lookup(Position, DynArray).

%% @spec write(tape(), integer()) -> {tape(), integer()}
%% @doc Places the given value at the current position on the tape.

write({Position, DynArray}, Value) ->
  {{Position, dynarray:insert(Position, DynArray, Value)}, Value}.

%% @spec increment(tape(), N::integer()) -> tape()
%% @doc Increments the value at the current position on the tape N times.

increment(Tape, N) ->
  write(Tape, read(Tape) + N).

%% @spec decrement(tape(), N::integer()) -> tape()
%% @doc Decrements the value at the current position on the tape N times.

decrement(Tape, N) ->
  write(Tape, read(Tape) - N).

peek({Position, DynArray}, Addr) ->
  lookup(Addr + 1, DynArray).

poke({Position, DynArray}, Addr, Value) ->
  {Position, dynarray:insert(Addr + 1, DynArray, Value)}.

head({Position, DynArray}) ->
  Position - 1.

lookup(Position, DynArray) ->
  % ce_log:write("looking up ~p ~p", [Position, DynArray]),
  case dynarray:lookup(Position, DynArray) of
    [] ->
      0;
    Else when is_integer(Else) ->
      Else
  end.

%%% END of pibfi_tape_dynarray.erl %%%
