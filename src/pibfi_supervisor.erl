%%% BEGIN pibfi_supervisor.erl %%%
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

%% @doc Supervisor process for <code>pibfi</code>.
%%
%% <p>All error handling is done here.  This allows the other processes
%% to be coded in a more direct, laissez-crash style.  The centralization
%% of error-handling code in this process allows for minimal
%% error-handling code in the other processes, which makes their code
%% clearer and easier to follow.</p>
%%
%% @end

-module(pibfi_supervisor).
-vsn('2003.0505').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([start/0, server/0, spawn_link/6, link/3]).

start() ->
  spawn(?MODULE, server, []).

server() ->
  process_flag(trap_exit, true),
  loop(dict:new(), dict:new(), 0).
  
loop(CriticalPids, NonCriticalPids, Count) ->
  receive
    {Pid, spawn, Name, critical, Module, Function, Args} ->
      NewPid = spawn_link(Module, Function, Args),
      CriticalPids0 = dict:store(NewPid, Name, CriticalPids),
      whisper("~p->~p critical ~s started ~p", [Count, Count + 1, Name, NewPid]),
      Pid ! {self(), spawned, Module, Function, Args, NewPid},
      loop(CriticalPids0, NonCriticalPids, Count + 1);
    {Pid, spawn, Name, noncritical, Module, Function, Args} ->
      NewPid = spawn_link(Module, Function, Args),
      NonCriticalPids0 = dict:store(NewPid, Name, NonCriticalPids),
      whisper("~p->~p noncritical ~s started ~p", [Count, Count + 1, Name, NewPid]),
      Pid ! {self(), spawned, Module, Function, Args, NewPid},
      loop(CriticalPids, NonCriticalPids0, Count + 1);
    {Pid, link, Name, LPid} ->
      link(LPid),
      NonCriticalPids0 = dict:store(LPid, Name, NonCriticalPids),
      whisper("~p->~p noncritical ~s linked to ~p", [Count, Count + 1, Name, LPid]),
      Pid ! {self(), linked, LPid},
      loop(CriticalPids, NonCriticalPids0, Count + 1);
    {'EXIT', From, OK} when OK == normal; OK == shutdown; OK == terminated ->
      case dict:find(From, CriticalPids) of
        {ok, Name} ->
          whisper("~p->~p critical ~s exited normally", [Count, Count - 1, Name]),
          CriticalPids0 = dict:erase(From, CriticalPids),
          case dict:to_list(CriticalPids0) of
            [] ->
              whisper("all critical processes exited normally, exiting", []),
              pibfi:whisper("---------------END PROGRAM OUTPUT---------------"),
	      case pibfi_options:get_flag(autopsy) of
                true ->
	          pibfi_statistics:dump(autopsy);
	        false ->
		  ok
              end,
              exeunt(NonCriticalPids),
	      erlang:halt();
            _ ->
              loop(CriticalPids0, NonCriticalPids, Count - 1)
          end;
	_ ->
	  case dict:find(From, NonCriticalPids) of
	    {ok, Name} ->
	      whisper("~p->~p noncritical ~s exited normally",
	        [Count, Count - 1, Name]),
	      NonCriticalPids0 = dict:erase(From, NonCriticalPids),
	      loop(CriticalPids, NonCriticalPids0, Count - 1);
	    error ->
	      whisper("??? unknown pid ~p quitting", [From]),
	      loop(CriticalPids, NonCriticalPids, Count)
	  end
      end;
    {'EXIT', From, Reason} ->
      Name = case dict:find(From, CriticalPids) of
        {ok, N} ->
	  N;
	_ ->
	  case dict:find(From, NonCriticalPids) of
	    {ok, N} ->
	      N;
	    _ ->
	      "unknown"
	  end
      end,
      pibfi:whisper("---------------END PROGRAM OUTPUT---------------"),
      io:fwrite("~n***** HALTED~nAn error occurred in the ~s process:~n~p~n",
        [Name, Reason]),
      pibfi_statistics:dump(),
      exeunt(CriticalPids),
      exeunt(NonCriticalPids),
      erlang:halt(1)
  end.

exeunt(Dict) ->
  exeunt0(dict:to_list(Dict)).
  
exeunt0(List) when is_list(List) ->
  lists:foreach(fun({P,N}) ->
    whisper("shutting down ~s ~p", [N, P]),
    exit(P, shutdown)
  end, List).

whisper(FmtString, Args) ->
  case pibfi_options:get_flag(bearflowers) of
    true ->
      io:fwrite(">>> " ++ FmtString ++ "~n", Args);
    false ->
      ok
  end.

%%% interface

spawn_link(Supervisor, Name, Class, Module, Function, Args) ->
  Supervisor ! {self(), spawn, Name, Class, Module, Function, Args},
  receive
    {Supervisor, spawned, Module, Function, Args, Pid} ->
      Pid
  end.

link(Supervisor, Name, Pid) ->
  Supervisor ! {self(), link, Name, Pid},
  receive
    {Supervisor, linked, Pid} ->
      Pid
  end.

%%% END of pibfi_supervisor.erl %%%
