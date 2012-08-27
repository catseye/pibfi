%%% BEGIN pibfi_input.erl %%%
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

%% @doc Input subsystem of the Platonic Ideal Brainf*ck Interpreter.
%%
%% @end

-module(pibfi_input).
-vsn('2003.0427').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([start/6, server/5]).

%% @spec start(Supervisor::pid(), IoPid::pid(), Dest::pid(),
%%               Interactive::boolean(),
%%               Device, EOF) -> pid()
%% @doc Creates and spawns a new input subsystem.

start(Supervisor, IoPid, Dest, Interactive, Device, EOF) ->
  pibfi_supervisor:spawn_link(Supervisor, "input", noncritical,
    ?MODULE, server, [IoPid, Dest, Interactive, Device, EOF]).

server(IoPid, Dest, Interactive, Device, EOF) ->
  % note that this is synchronous - we are essentially a send-only server.
  % we can't react to messages until the user enters input - so we won't.
  case io:get_chars(Device, '', 1) of
    [10] ->
      % we will always get 10 for an interactive incoming EOL, no matter
      % what the operating system above us thinks
      case Interactive of
        false ->
          pibfi_xlat:send(Dest, 10);
	true ->
	  % so we actually have to simulate the action of the OS here
	  lists:foreach(fun(EOLChar) ->
            pibfi_xlat:send(Dest, EOLChar)
	  end, pibfi:os_eol())
      end,
      server(IoPid, Dest, Interactive, Device, EOF);
    [Char] ->
      pibfi_xlat:send(Dest, Char),
      server(IoPid, Dest, Interactive, Device, EOF);
    eof ->
      % ce_log:write("~p", [eof]),
      case EOF of
        halt ->
          exit(halted_due_to_eof);
        stop ->
	  pibfi_xlat:notify(Dest, stop);
        nop ->
	  pibfi_xlat:notify(Dest, nop);
        _ ->
	  pibfi_xlat:notify(Dest, EOF)
      end,
      exit(normal);
    {error, terminated} ->
      % IoPid ! {self(), direct, stop},
      exit(normal);
    Else ->
      exit({halted_due_to_input_error, Else})
  end.

%%% END of pibfi_input.erl %%%
