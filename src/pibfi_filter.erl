%%% BEGIN pibfi_fliter.erl %%%
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

%% @doc Stream filtering for the Platonic Ideal Brainf*ck Interpreter.
%%
%% @end

-module(pibfi_filter).
-vsn('2003.0427').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([start/6, server/5]).
-export([send/2, notify/2]).

%% @spec start(Supervisor::pid(), Role::atom(), Dest::pid(),
%%       MaxThru::integer(), MinThru::integer(), WrapThru::integer()) -> pid()
%% @doc Creates and spawns a new stream filter.

start(Supervisor, Role, Dest, MaxThru, MinThru, WrapThru) ->
  Name = atom_to_list(Role) ++ " filter",
  pibfi_supervisor:spawn_link(Supervisor, Name, noncritical,
    ?MODULE, server, [Dest, Role, MaxThru, MinThru, WrapThru]).

server(Dest, Role, MaxThru, MinThru, WrapThru) ->
  receive
    {Pid, Originator, message, Term} ->
      Dest ! {self(), Originator, message, Term},
      server(Dest, Role, MaxThru, MinThru, WrapThru);
    {Pid, notify, Term} ->
      Dest ! {self(), Pid, message, Term},
      server(Dest, Role, MaxThru, MinThru, WrapThru);
    {Pid, xlat_char, Char} ->
      Dest ! {self(), xlat_char, filter(Role, MaxThru, MinThru, WrapThru, Char)},
      server(Dest, Role, MaxThru, MinThru, WrapThru)
  end.

filter(Role, MaxIn, MinIn, false, Char) ->
  pibfi:assert_in_bounds({Role, character}, MinIn, Char, MaxIn),
  Char;
filter(Role, MaxIn, MinIn, true, Char) ->
  pibfi:wrap(Char, MinIn, MaxIn).

%% @spec send(xlat(), char() | string()) -> ok
%% @doc Sends a character or characters to a filter server for filtration.

send(Pid, Chars) when is_list(Chars) ->
  lists:foreach(fun(Char) ->
    % timer:sleep(100),
    Pid ! {self(), xlat_char, Char}
  end, Chars),
  ok;

send(Pid, Char) ->
  Pid ! {self(), xlat_char, Char},
  ok.

%% @spec notify(xlat(), term()) -> ok
%% @doc Notifies the other end of the connection.
%% They will receive a <code>{Filter::pid(), Notifier::pid(), message, term()}</code> message.

notify(Pid, Term) ->
  Pid ! {self(), notify, Term},
  ok.

%%% END of pibfi_input.erl %%%
