%%% BEGIN pibfi.erl %%%
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

%% @doc Platonic Ideal Brainf*ck Interpreter (<code>pibfi</code>).
%%
%% <p>This application implements an interpreter for the language
%% Brainf*ck.  It does not by default impose any limit on the maximum
%% length of the tape, nor the maximum value that can entered into
%% any of the cells in the tape (beyond those limits inescapably
%% imposed upon it by the underlying operating system and hardware.)
%% It can, however, be configured to simulate the limits imposed upon
%% the language by many other implementations which sacrifice
%% scaleability in order to achieve other goals (usually, to build an
%% astonishingly small compiler or interpreter.)</p>
%%
%% <p>As such, <code>pibfi</code> may be in a position to one day
%% develop into a universal (or at least reference) interpreter for
%% the Brainf*ck language.</p>
%%
%% <p>For a synopsis of the command-line options that can be used
%% with <code>pibfi</code>, see the
%% <code><a href="pibfi_options.html">pibfi_options</a></code>
%% documentation.</p>
%%
%% <p>This module contains the interface to start <code>pibfi</code>
%% both from the command line, and programmatically.  It also contains
%% common functions used by other <code>pibfi_*</code> modules.</p>
%%
%% <p>Parts of this program were derived from the Erlang example
%% program <code>bf.erl</code>.</p>
%%
%% @end

-module(pibfi).
-vsn('2003.0505').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([run/1, startup/2, run/6]).
-export([wrap/3, assert/2, assert_in_bounds/4]).
-export([whisper/1, whisper/2]).
-export([os_eol/0]).

%% @spec run(Args::[string()]) -> halt()
%% @doc Starts <code>pibfi</code> for the purposes of running a Brainf*ck
%% program.  This function is intended to be invoked
%% from the command line.  When using <code>pibfi</code> from another Erlang
%% program, or from the Erlang shell, it is suggested you call
%% <code>run/6</code> instead.

run([Filename]) ->
  Supervisor = pibfi_supervisor:start(),
  pibfi_supervisor:spawn_link(Supervisor, "startup", critical,
    ?MODULE, startup, [Supervisor, Filename]).

startup(Supervisor, Filename) ->
  ParserOptions = pibfi_options:get_opts(
  [
    {dontstrip, ""},
    {optimize, 1},
    {statuscmd, "#"},
    {heredoc, undefined}
  ]),
  TapeOptions = pibfi_options:get_opts(
  [
    {tapemodule, pibfi_tape_ets},
    {maxcell, infinity},
    {mincell, 0},
    {wrapcell, false},
    {maxtape, infinity},
    {mintape, 0},
    {wraptape, false}
  ]),
  IOOptions = pibfi_options:get_opts(
  [
    {infile, tty},
    {outfile, tty},
    {maxout, infinity},
    {minout, 0},
    {wrapout, false},
    {maxin, infinity},
    {minin, 0},
    {wrapin, false},
    {eof, 0},
    {xlatout, [{"\n", os_eol()}]},
    {xlatin, [{os_eol(), "\n"}]}
  ]),
  DebugOptions = pibfi_options:get_opts(
  [
    {statusevery, undefined}
  ]),
  
  {ok, {B, [Version]}} = beam_lib:version(code:which(?MODULE)),
  VersionString = atom_to_list(Version),
  whisper("pibfi: Platonic Ideal Brainf*ck Interpreter v~s", [VersionString]),
  whisper("Copyright (c)2003 Cat's Eye Technologies. All rights reserved."),
  whisper("Program file name: ~s", [Filename]),
  whisper("Parser options: ~s", [map_format(ParserOptions)]),
  whisper("Tape options: ~s", [map_format(TapeOptions)]),
  whisper("I/O options: ~s", [map_format(IOOptions)]),
  whisper("Debug options: ~s", [map_format(DebugOptions)]),
  whisper("--------------BEGIN PROGRAM OUTPUT--------------"),
    
  Program = case read_source_file(Filename) of
    {ok, FileContents} ->
      pibfi_supervisor:spawn_link(Supervisor, "interpreter", critical,
        ?MODULE, run, [Supervisor, FileContents,
	  ParserOptions, TapeOptions, IOOptions, DebugOptions]);
    Else ->
      exit({could_not_read_file, Filename})
  end.

%% @spec run(Supervisor::pid(), ProgramSource::binary(),
%%         ParserOptions::[{atom(), term()}],
%%         TapeOptions::[{atom(), term()}],
%%         IOOptions::[{atom(), term()}],
%%         DebugOptions::[{atom(), term()}]) -> tape()
%%           program() = string() | binary() | tuple()
%% @doc Runs a Brainf*ck program.

run(Supervisor, ProgramSource, ParserOptions, TapeOptions, IOOptions, DebugOptions) ->
  {Program, HereDoc} = pibfi_parser:parse(ProgramSource, ParserOptions),
  CannedInput = case pibfi_options:get_option(IOOptions, infile, undefined) of
    heredoc ->
      HereDoc;
    _ ->
      ""
  end,
  IoPid = pibfi_io:start(Supervisor, IOOptions, CannedInput),
  Module = pibfi_options:get_option(TapeOptions, tapemodule, pibfi_tape_dict),
  TapePid = pibfi_tape:start(Module, Supervisor, TapeOptions),
  pibfi_statistics:start(Supervisor, TapePid, DebugOptions),
  pibfi_interpreter:interpret(Program, ParserOptions, TapePid, IoPid),
  pibfi_io:flush(IoPid),
  pibfi_io:stop(IoPid),
  TapePid.

map_format(List) ->
  lists:map(fun
    ({Atom, Term}) when Atom == xlatin; Atom == xlatout ->
      io_lib:fwrite("~p=[~s] ", [Atom, map_format(Term)]);
    ({Atom, Term}) ->
      io_lib:fwrite("~p=~p ", [Atom, Term])  
  end, List).

read_source_file("http://" ++ RestOfURL) ->
  case http:request_sync(get, {"http://" ++ RestOfURL, []}) of
    {200, Headers, Body} ->
      {ok, list_to_binary(Body)};
    {Response, Headers, Body} ->
      {error, {unexpected_response_code, Response}}
  end;
read_source_file(Filename) ->
  file:read_file(Filename).

%% @spec wrap(Value::integer(), Max::integer(), Min::integer()) -> integer()
%% @doc Implements a generic modulus function.  Both the top and bottom
%% modulus limits may be specified.

wrap(X, Min, Max) ->
  case (X - Min) rem ((Max - Min) + 1) of
    Y when Y < 0 ->
      Max + 1 + Y;
    Y when Y >= 0 ->
      Y + Min
  end.

%% @spec assert(Condition::boolean(), ErrorReason::term()) -> true
%% @doc Asserts that the condition is true.  If it is not, the
%% process crashes with the given reason.

assert(true, _) ->
  true;
assert(false, Reason) ->
  exit(Reason).

assert_in_bounds(Type, Min, Test, Max) ->
  Reason = {out_of_bounds, {{Type, Test}, {Min, Max}}},
  case {Max, Min} of
    {infinity, infinity} ->
      true;
    {infinity, _} ->
      assert(Test >= Min, Reason);
    {_, infinity} ->
      assert(Test =< Max, Reason);
    {_, _} ->
      assert(Test >= Min andalso Test =< Max, Reason)
  end.

%% @spec whisper(string()) -> ok
%% @equiv whisper(string(), [])

whisper(String) ->
  whisper(String, []).

%% @spec whisper(string(), [term()]) -> ok
%% @doc Displays extra information.  The user can shut this off with
%% <code>-quiet</code>.

whisper(FmtString, Args) ->
  case pibfi_options:get_flag(quiet) of
    false ->
      io:fwrite(FmtString ++ "~n", Args);
    true ->
      ok
  end.

%% @spec os_eol() -> string()
%% @doc Returns the native end-of-line convention, if it can be determined.
%% If it cannot be determined, linefeed (ASCII character 10) is assumed.

os_eol() ->
  case os:type() of
    {win32, _} ->
      [13, 10];
    _ ->
      [10]
  end.

%%% END of pibfi.erl %%%
