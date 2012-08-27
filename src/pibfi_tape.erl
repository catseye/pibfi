%%% BEGIN pibfi_tape.erl %%%
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

%% @doc Tape ADT for the Platonic Ideal Brainf*ck Interpreter.
%%
%% <p>Now a behaviour.</p>
%%
%% @end

-module(pibfi_tape).
-vsn('2003.0505').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([behaviour_info/1]).
-export([start/3, server/2]).
-export([left/1, right/1, increment/1, decrement/1]).
-export([left/2, right/2, increment/2, decrement/2]).
-export([read/1, write/2]).
-export([examine/1]).

behaviour_info(callbacks) ->
  [
    {new, 1},
    % Brainf*ck semantics
    {left, 2},  % (tape(), delta()) -> {tape(), cell()}
    {right, 2}, % (tape(), delta()) -> {tape(), cell()}
    % Efficiency extensions
    {read, 1},  % (tape()) -> cell()
    {write, 2}, % (tape(), cell()) -> {tape(), cell()}
    {peek, 2},  % (tape(), position()) -> cell()
    {poke, 3},  % (tape(), position, cell()) -> tape()
    {head, 1}   % (tape()) -> position()
  ].
  
%% @spec start(module(), Supervisor::pid(), [option()]) -> pid()
%%         option() = {atom(), term()}
%% @doc Starts and returns the pid of a new tape server.
%% For a description of the allowed options, see the documentation for
%% the <code><a href="pibfi.html">pibfi</a></code> module.

start(Module, Supervisor, Options) ->
  pibfi_supervisor:spawn_link(Supervisor, "tape server", noncritical,
    ?MODULE, server, [Module, Options]).

server(Module, Options) ->
  NewTape = Module:new(Options),
  Position = Module:head(NewTape),
  Value = Module:read(NewTape),
  WrapTape = pibfi_options:get_option(Options, wraptape, false),
  MaxTape = pibfi_options:get_option(Options, maxtape, infinity),
  MinTape = pibfi_options:get_option(Options, mintape, 0),
  WrapCell = pibfi_options:get_option(Options, wrapcell, false),
  MaxCell = pibfi_options:get_option(Options, maxcell, infinity),
  MinCell = pibfi_options:get_option(Options, mincell, 0),
  case {WrapTape, MaxTape, MinTape, WrapCell, MaxCell, MinCell} of
    {true, infinity, _, _, _, _} ->
      exit(maxtape_must_be_finite_when_wraptape_is_true);
    {true, _, infinity, _, _, _} ->
      exit(mintape_must_be_finite_when_wraptape_is_true);
    {_, _, _, true, infinity, _} ->
      exit(maxcell_must_be_finite_when_wrapcell_is_true);
    {_, _, _, true, _, infinity} ->
      exit(mincell_must_be_finite_when_wrapcell_is_true);

    {_, A, B, _, _, _} when B =/= infinity, A =/= infinity, B > A ->
      exit(mintape_cannot_exceed_maxtape);
    {_, A, infinity, _, _, _} when A =/= infinity, A < 0 ->
      exit(mintape_maxtape_range_must_include_start_cell_0);
    {_, infinity, B, _, _, _} when B =/= infinity, B > 0 ->
      exit(mintape_maxtape_range_must_include_start_cell_0);
    {_, A, B, _, _, _} when B =/= infinity, A =/= infinity, B > 0, A > 0 ->
      exit(mintape_maxtape_range_must_include_start_cell_0);
    {_, A, B, _, _, _} when B =/= infinity, A =/= infinity, B < 0, A < 0 ->
      exit(mintape_maxtape_range_must_include_start_cell_0);

    {_, _, _, _, A, B} when B =/= infinity, A =/= infinity, B > A ->
      exit(mincell_cannot_exceed_maxcell);

    _ ->
      Value = 0,
      loop(Module, NewTape, Position, Value,
           0, 0, 0, 0,
           MinTape, MaxTape, WrapTape,
           MinCell, MaxCell, WrapCell)
  end.

% only read and examine are synchronous

loop(Module, Tape, Position, Value,
     LowPos, HighPos, LowValue, HighValue,
     MinTape, MaxTape, WrapTape, MinCell, MaxCell, WrapCell) ->
  {LowPos0, HighPos0} = case Position of
    L when L < LowPos -> {L, HighPos};
    H when H > HighPos -> {LowPos, H};
    _ -> {LowPos, HighPos}
  end,
  {LowValue0, HighValue0} = case Value of
    L0 when L0 < LowValue -> {L0, HighValue};
    H0 when H0 > HighValue -> {LowValue, H0};
    _ -> {LowValue, HighValue}
  end,
  receive
    {Pid, increment, N} ->
      % Pid ! {self(), {increment, N}, ok},
      Value0 = case WrapCell of
        true ->
          pibfi:wrap(Value + N, MinCell, MaxCell);
        false ->
          pibfi:assert_in_bounds(tape_cell, MinCell, Value, MaxCell),
          Value + N
      end,    
      {Tape0, Value1} = Module:write(Tape, Value0),
      loop(Module, Tape0, Position, Value1,
           LowPos0, HighPos0, LowValue0, HighValue0,
	   MinTape, MaxTape, WrapTape, MinCell, MaxCell, WrapCell);
    {Pid, decrement, N} ->
      % Pid ! {self(), {decrement, N}, ok},
      Value0 = case WrapCell of
        true ->
          pibfi:wrap(Value - N, MinCell, MaxCell);
        false ->
          pibfi:assert_in_bounds(tape_cell, MinCell, Value, MaxCell),
          Value - N
      end,    
      {Tape0, Value1} = Module:write(Tape, Value0),
      loop(Module, Tape0, Position, Value1,
           LowPos0, HighPos0, LowValue0, HighValue0,
           MinTape, MaxTape, WrapTape, MinCell, MaxCell, WrapCell);
    {Pid, left, N} ->
      % Pid ! {self(), {left, N}, ok},
      NewPosition = case Position - N of
        P when MinTape == infinity ->
          P;
        P when P < MinTape ->
          pibfi:assert(WrapTape, {tape_out_of_bounds, P}),
          pibfi:wrap(P, MinTape, MaxTape);
        P ->
          P
      end,
      {Tape0, Value0} = Module:left(Tape, N),
      % ce_log:write("left, newval ~p", [Value0]),
      loop(Module, Tape0, NewPosition, Value0,
           LowPos0, HighPos0, LowValue0, HighValue0,
           MinTape, MaxTape, WrapTape, MinCell, MaxCell, WrapCell);
    {Pid, right, N} ->
      % Pid ! {self(), {right, N}, ok},
      NewPosition = case Position + N of
        P when MaxTape == infinity ->
          P;
        P when P > MaxTape ->
          pibfi:assert(WrapTape, {tape_out_of_bounds, P}),
          pibfi:wrap(P, MinTape, MaxTape);
        P ->
          P
      end,
      {Tape0, Value0} = Module:right(Tape, N),
      % ce_log:write("right, newval ~p", [Value0]),
      loop(Module, Tape0, NewPosition, Value0,
           LowPos0, HighPos0, LowValue0, HighValue0,
           MinTape, MaxTape, WrapTape, MinCell, MaxCell, WrapCell);
    {Pid, read} ->
      Pid ! {self(), read, Module:read(Tape)},
      loop(Module, Tape, Position, Value,
           LowPos0, HighPos0, LowValue0, HighValue0,
           MinTape, MaxTape, WrapTape, MinCell, MaxCell, WrapCell);
    {Pid, write, Value0} ->
      % ce_log:write("write: ~p", [Value0]),
      Value1 = case WrapCell of
        true ->
          pibfi:wrap(Value0, MinCell, MaxCell);
        false ->
          pibfi:assert_in_bounds(tape_cell, MinCell, Value0, MaxCell),
          Value0
      end,    
      {Tape0, Value2} = Module:write(Tape, Value1),
      % ce_log:write("writing ~p", [Value2]),
      % Pid ! {self(), {write, Value}, Value2},
      loop(Module, Tape0, Position, Value2,
           LowPos0, HighPos0, LowValue0, HighValue0,
           MinTape, MaxTape, WrapTape, MinCell, MaxCell, WrapCell);
    {Pid, examine} ->
      server_examine(Module, Tape, Position, Value,
                     LowPos0, HighPos0, LowValue0, HighValue0,
                     MinTape, MaxTape, WrapTape, MinCell, MaxCell, WrapCell),
      Pid ! {self(), examine, ok},
      loop(Module, Tape, Position, Value,
           LowPos0, HighPos0, LowValue0, HighValue0,
           MinTape, MaxTape, WrapTape, MinCell, MaxCell, WrapCell)
  end.

%% @spec left(pid()) -> ok
%% @doc Moves the read/write head one position left on the tape.

left(TapePid) -> left(TapePid, 1).

%% @spec left(pid(), N::integer()) -> ok
%% @doc Moves the read/write head N positions left on the tape.

left(TapePid, N) ->
  TapePid ! {self(), left, N}. % , waitfor({TapePid, {left, N}, ok}).

%% @spec right(pid()) -> ok
%% @doc Moves the read/write head one position right on the tape.

right(TapePid) -> right(TapePid, 1).

%% @spec right(pid(), N::integer()) -> ok
%% @doc Moves the read/write head N positions right on the tape.

right(TapePid, N) ->
  TapePid ! {self(), right, N}. % , waitfor({TapePid, {right, N}, ok}).

%% @spec read(pid()) -> integer()
%% @doc Returns the value at the current position on the tape.

read(TapePid) ->
  TapePid ! {self(), read},
  receive
    {TapePid, read, X} ->
      X
  end.

waitfor(Thing) ->
  receive
    Thing ->
      ok
    after 1000 ->
      {error, timeout}
  end.

%% @spec write(tape(), integer()) -> {ok, integer()} | {error, Reason}
%% @doc Places the given value at the current position on the tape.

write(TapePid, Value) ->
  TapePid ! {self(), write, Value}.

%% @spec increment(tape()) -> ok | {error, Reason}
%% @doc Increments the value at the current position on the tape.

increment(TapePid) -> increment(TapePid, 1).

%% @spec increment(tape(), N::integer()) -> ok | {error, Reason}
%% @doc Increments the value at the current position on the tape N times.

increment(TapePid, N) ->
  TapePid ! {self(), increment, N}.

%% @spec decrement(tape()) -> ok | {error, Reason}
%% @doc Decrements the value at the current position on the tape.

decrement(TapePid) -> decrement(TapePid, 1).

%% @spec decrement(tape(), N::integer()) -> ok | {error, Reason}
%% @doc Decrements the value at the current position on the tape N times.

decrement(TapePid, N) ->
  TapePid ! {self(), decrement, N}.

%% @spec examine(TapePid::pid()) -> ok | {error, Reason}
%% @doc Examines the state of the tape.

examine(TapePid) ->
  TapePid ! {self(), examine}, waitfor({TapePid, examine, ok}).


server_examine(Module, Tape, Position, Value,
 LowPos, HighPos, LowValue, HighValue,
 MinTape, MaxTape, WrapTape,
 MinCell, MaxCell, WrapCell) ->
  case Position of
    0 ->
     io:fwrite("+ Tape head position: at start");
    1 ->
     io:fwrite("+ Tape head position: 1 cell right of start");
    -1 ->
     io:fwrite("+ Tape head position: 1 cell left of start");
    P when P > 1 ->
     io:fwrite("+ Tape head position: ~p cells right of start",
       [Position]);
    P when P < -1 ->
     io:fwrite("+ Tape head position: ~p cells left of start",
       [-1 * Position])
  end,
  io:fwrite(", cell contents: ~p~n", [Value]),
  io:fwrite("+ Tape observed head position: min ~p, max ~p, range ~p~n",
    [LowPos, HighPos, (HighPos - LowPos) + 1]),
  io:fwrite("+ Tape observed cell contents: min ~p, max ~p, range ~p~n",
    [LowValue, HighValue, (HighValue - LowValue) + 1]),
  Low = case Position - 5 of
    L when MinTape =/= infinity, L < MinTape ->
      MinTape;
    L ->
      L
  end,
  High = case Position + 4 of
    H when MaxTape =/= infinity, H > MaxTape ->
      MaxTape;
    H ->
      H
  end,
  io:fwrite("+ Tape contents (~p - ~p cells right of start):~n", [Low, High]),
  Range = lists:seq(Low, High),
  lists:foreach(fun(X) ->
    A = case X of
      Position ->
        "->#" ++ integer_to_list(X);
      _ ->
        "#" ++ integer_to_list(X)
    end,
    io:fwrite("~8s", [A])
  end, Range),
  io:nl(),
  TapeContext = lists:reverse(lists:foldl(fun(X, A) ->
    [Module:peek(Tape, X) | A]
  end, [], Range)),
  lists:foreach(fun(X) ->
    io:fwrite("~8w", [X])
  end, TapeContext),
  io:nl().

%%% END of pibfi_tape.erl %%%
