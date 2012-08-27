%%% BEGIN pibfi_io.erl %%%
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

%% @doc I/O subsystem of the Platonic Ideal Brainf*ck Interpreter.
%%
%% <p>Deals with the input and output servers, their filters, and
%% their translators.</p>
%%
%% @end

%          --->  input translator --->  input filter --->
% terminal                                                i/o    <-> Brainf*ck
%  or file                                                subsys     program
%          <--- output translator <--- output filter <---

-module(pibfi_io).
-vsn('2003.0427').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([start/3, stop/1, server/1]).
-export([input/1, output/2, flush/1]).

-record(io,
{
  device  = standard_io,
  maxout  = infinity,
  minout  = 0,
  wrapout = false,
  maxin   = infinity,
  minin   = 0,
  wrapin  = false,
  outxlat = [],
  inxlat  = [],
  eof     = 0
}).

%% @spec start(Supervisor::pid(), [option()], CannedInput::string()) -> pid()
%% @doc Creates and spawns a new I/O subsystem.
%% For a description of the allowed options, see the documentation for
%% the <code><a href="pibfi_options.html">pibfi_options</a></code> module.

start(Supervisor, Options, CannedInput) ->
  IO = config(#io{}, Options),

  % start i/o server
  IoPid = pibfi_supervisor:spawn_link(Supervisor, "i/o subsystem",
    critical,  ?MODULE, server, [IO]),

  % start filters & translators
  InFilter = pibfi_filter:start(Supervisor, input, IoPid,
    IO#io.maxin, IO#io.minin, IO#io.wrapin),

  OutXlat = pibfi_xlat:start(IO#io.outxlat, IoPid),
  pibfi_supervisor:link(Supervisor, "output translator", OutXlat),

  InXlat = pibfi_xlat:start(IO#io.inxlat, InFilter),
  pibfi_supervisor:link(Supervisor, "input translator", InXlat),

  % start input server
  InFile = pibfi_options:get_option(Options, infile, tty),
  Interactive = InFile == tty,
  IoDevice = case InFile of
    tty ->
      standard_io; % IO#io.device;
    heredoc ->
      undefined;
    Filename ->
      {ok, I} = file:open(Filename, [read, read_ahead]),
      I
  end,
  case IoDevice of
    undefined ->
      % don't start an input server, do send canned input
      lists:foreach(fun(Char) ->
        % ce_log:write("sending canned ~c", [Char]),
        pibfi_xlat:send(InXlat, Char)
      end, CannedInput),
      pibfi_xlat:notify(InXlat, IO#io.eof);
    _ ->
      InputPid = pibfi_input:start(Supervisor, IoPid, InXlat, Interactive,
        IoDevice, IO#io.eof)
  end,
  % notify I/O server of who to talk to
  IoPid ! {self(), hello, {OutXlat, InFilter}},
  IoPid.

%% @spec server(IO) -> never_returns()
%% @doc Spawned by <code>start/1</code>.
%% Should not be called directly by user code.

server(IO) ->
  receive  
    {Spawner, hello, {OutXlat, InFrom}} ->
      loop(IO, OutXlat, InFrom)
  end.

loop(IO, OutXlat, InFrom) ->
  #io{
       device  = Device,
       maxout  = MaxOut,
       minout  = MinOut,
       wrapout = WrapOut,
       maxin   = MaxIn,
       minin   = MinIn,
       wrapin  = WrapIn,
       eof     = EOF
     } = IO,
  % ce_log:write("looping"),
  receive  
    {Pid, output, [Output]} ->
      % ce_log:write("handling output request"),
      pibfi_xlat:send(OutXlat, Output),
      loop(IO, OutXlat, InFrom);
    {OutXlat, xlat_char, Char} ->
      % ce_log:write("handling xlated output ~c", [Char]),
      write(Device, MaxOut, MinOut, WrapOut, Char),
      loop(IO, OutXlat, InFrom);
    {Pid, input, []} ->
      case EOF of
        % first check to see if we noticed the input server go down.
        {eof, Char} when is_integer(Char) ->
          Pid ! {self(), input, Char},
	  loop(IO, OutXlat, InFrom);
        _ ->
          input_loop(Pid, IO, OutXlat, InFrom)
      end;
    {Pid, flush} ->
      pibfi_xlat:flush(OutXlat),
      Result = flush_loop(IO, OutXlat, InFrom),
      Pid ! {self(), flush, Result},
      loop(IO, OutXlat, InFrom);
    {Pid, stop} ->
      % ce_log:write("stopping"),
      Pid ! {self(), stop, ok}
  end.

input_loop(Pid, IO, OutXlat, InFrom) ->
  #io{
       device  = Device,
       maxout  = MaxOut,
       minout  = MinOut,
       wrapout = WrapOut,
       maxin   = MaxIn,
       minin   = MinIn,
       wrapin  = WrapIn,
       eof     = EOF
     } = IO,
  receive
    % in case the translation system decides to output something
    % while we're waiting for input, we have to react to the
    % output translator's messages here, too.
    {OutXlat, xlat_char, Char} ->
      % ce_log:write("handling xlated output ~c", [Char]),
      write(Device, MaxOut, MinOut, WrapOut, Char),
      input_loop(Pid, IO, OutXlat, InFrom);
    
    % Receive messages from the input_server (possibly via the
    % input translator / filter.)
    {InFrom, xlat_char, Char} ->
      % ce_log:write("input xlatted ~p", [Char]),
      Pid ! {self(), input, Char},
      loop(IO, OutXlat, InFrom);
    {InFrom, _, message, X} ->
      % direct, non-xlated msg from input server
      % this means the input server has kicked the bucket
      % and that we must know that for future input requests
      % ce_log:write("direct input ~p", [X]),
      Pid ! {self(), input, X},
      IO0 = IO#io{ eof = {eof, IO#io.eof}},
      loop(IO0, OutXlat, InFrom)
  end.

flush_loop(IO, OutXlat, InXlat) ->
  #io{
       device  = Device,
       maxout  = MaxOut,
       minout  = MinOut,
       wrapout = WrapOut,
       maxin   = MaxIn,
       minin   = MinIn,
       wrapin  = WrapIn,
       eof     = EOF
     } = IO,
  receive
    {OutXlat, xlat_char, Char} ->
      % ce_log:write("flushing xlated output"),
      write(Device, MaxOut, MinOut, WrapOut, Char),
      flush_loop(IO, OutXlat, InXlat)
    after 200 ->
      ok
  end.

%% @spec write(iodevice(), Max::integer(), Min::integer(), Wrap::boolean(),
%%         char()) -> ok
%% @doc Writes a character to the output, within the given constraints.

write(Device, MaxOut, MinOut, false, Char) ->
  pibfi:assert_in_bounds(output_character, MinOut, Char, MaxOut),
  put_char(Device, Char);
write(Device, MaxOut, MinOut, true, Char) ->
  put_char(Device, pibfi:wrap(Char, MinOut, MaxOut)).

put_char(Device, Char) when Char >= 0, Char < 256 ->
  io:put_chars(Device, [Char]);
put_char(Device, Char) ->
  CharString = "&#" ++ integer_to_list(Char) ++ ";",
  io:put_chars(Device, CharString).

%% @spec config([option()], tape()) -> tape()
%% @doc Sets the various options of an I/O subsystem.

config(IO, []) ->
  IO;
config(IO, [Head | Tail]) ->
  config(config(IO, Head), Tail);

config(IO, {outfile, tty}) ->
  IO#io{ device = standard_io };
config(IO, {outfile, OutFile}) ->
  {ok, Device} = file:open(OutFile, [write, delayed_write]),
  IO#io{ device = Device };
config(IO, {infile, InFile}) ->
  IO;
config(IO, {device, Device})
 when is_pid(Device); Device == standard_io ->
  IO#io{ device = Device };
config(IO, {eof, EOF})
 when is_integer(EOF); EOF == halt; EOF == nop; EOF == stop ->
  IO#io{ eof = EOF };
config(IO, {maxout, MaxOut})
 when is_integer(MaxOut); MaxOut == infinity ->
  IO#io{ maxout = MaxOut };
config(IO, {minout, MinOut})
 when is_integer(MinOut); MinOut == infinity ->
  IO#io{ minout = MinOut };
config(IO, {wrapout, WrapOut})
 when WrapOut == true; WrapOut == false ->
  IO#io{ wrapout = WrapOut };
config(IO, {maxin, MaxIn})
 when is_integer(MaxIn); MaxIn == infinity ->
  IO#io{ maxin = MaxIn };
config(IO, {minin, MinIn})
 when is_integer(MinIn); MinIn == infinity ->
  IO#io{ minin = MinIn };
config(IO, {wrapin, WrapIn})
 when WrapIn == true; WrapIn == false ->
  IO#io{ wrapin = WrapIn };
config(IO, {xlatin, InXlat}) when is_list(InXlat) ->
  IO#io{ inxlat = InXlat };
config(IO, {xlatout, OutXlat}) when is_list(OutXlat) ->
  IO#io{ outxlat = OutXlat }.

%%% interface

%% @spec output(pid(), integer()) -> ok
%% @doc Sends the given character value to the output stream.

output(IoPid, Output) ->
  % ce_log:write("~p", [Output]),
  IoPid ! {self(), output, [Output]},
  ok.

%% @spec input(pid()) -> integer() | nop
%% @doc Retrieves the next character value from the input stream.

input(IoPid) ->
  % ce_log:write("input"),
  IoPid ! {self(), input, []},
  Result = receive
    {IoPid, input, nop} ->
      nop;
    {IoPid, input, stop} ->
      stop;
    {IoPid, input, Input} when is_integer(Input) ->
      Input
  end,
  % ce_log:write("input ~p", [Result]),
  Result.

%% @spec flush(pid()) -> ok
%% @doc Flushes any pending output, even if it has not been translated yet.

flush(IoPid) ->
  IoPid ! {self(), flush},
  receive
    {IoPid, flush, ok} ->
      ok
  end.

%% @spec stop(pid()) -> ok
%% @doc Tells the I/O server to stop.

stop(IoPid) ->
  IoPid ! {self(), stop},
  receive
    {IoPid, stop, ok} ->
      ok
  end.

%%% END of pibfi_io.erl %%%
