%%% BEGIN pibfi_xlat.erl %%%
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

%% @doc Character translation server for <code>pibfi</code>.
%%
%% <p>This is not efficient.  An efficient implementation would compile
%% the candidates into a finite-state machine first.  This doesn't do that.</p>
%%
%% @end

-module(pibfi_xlat).
-vsn('2003.0427').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([start/2, server/2]).
-export([send/2, notify/2, flush/1]).
-export([test/0]).

%% @spec start(candidates(), Dest::pid()) -> xlat()
%%         candidates() = [{string(), string()}]
%%         xlat() = pid()
%%         string() = [char()]
%%         char() = integer()
%% @doc Starts an xlat server.  Candidates is a list of pairs of strings.
%% Characters are sent to the xlat server with the <code>send/2</code>
%% function.  When they match the left
%% string of a candidate, the right string is sent to Dest instead.
%% If they do not match any candidates, they are sent through unaltered.
%% Characters are sent to Dest in the form
%% <code>{xlat(), xlat_char, char()}</code>.
%% Note that if two candidates have the same left string, the result of
%% the translation is undefined.  Also note that if one candidate has
%% a left string that is a prefix of another candidate's left string,
%% that second candidate will never match (the shorter one will always
%% be matched first.)

start(Candidates, Dest) ->
  spawn_link(?MODULE, server, [Candidates, Dest]).

%% @spec server(candidates(), Dest::pid()) -> never_returns()
%% @doc Spawned by <code>start/2</code>, should not be called
%% directly by user code.

server(Candidates, Dest) ->
  loop(Candidates, Candidates, Dest, []).

loop(Candidates, Working, Dest, Chars) ->
  receive
    {Pid, flush} ->
      gen_string(lists:reverse(Chars), Dest),
      Pid ! {self(), flush, ok},
      loop(Candidates, Candidates, Dest, []);
    {Pid, notify, Term} ->
      Dest ! {self(), Pid, message, Term},
      loop(Candidates, Working, Dest, Chars);
    {Pid, xlat_char, Char} ->
      % find all the things in our candidates that start with char
      sub_loop(Candidates, Working, Dest, Chars, Char)
  end.

sub_loop(Candidates, Working, Dest, Chars, Char) ->
  case get_candidates(Working, Char) of
    [] ->
      % ce_log:write("got nothin ~p", [{Char, Chars}]),
      % we got nothin.  just send the chars they sent so far
      case Chars of
        [] ->
	  Dest ! {self(), xlat_char, Char},
	  loop(Candidates, Candidates, Dest, []);
	_ ->
	  % we now have to invalidate our assumptions about these
          gen_string(lists:reverse(Chars), Dest),
	  % and immediately re-check this char from the beginning
          sub_loop(Candidates, Candidates, Dest, [], Char)
      end;
    [{"", To} | Tail] ->
      % we got a match.
      gen_string(To, Dest),
      loop(Candidates, Candidates, Dest, []);
    NewCandidates ->
      % we got more than one partially matching candidate.
      loop(Candidates, NewCandidates, Dest, [Char | Chars])
 end.

get_candidates(L, Char) ->
  lists:sort(lists:foldl(fun
    ({[Ch | Tail], To}, Acc) when Ch == Char->
      [{Tail, To} | Acc];
    (_, Acc) ->
      Acc
  end, [], L)).

gen_string(String, Dest) ->
  lists:foreach(fun(Z) ->
    Dest ! {self(), xlat_char, Z}
  end, String).

%%% interface

%% @spec send(xlat(), char() | string()) -> ok
%% @doc Sends a character or characters to an xlat server for translation.

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
%% They will receive a <code>{Xlat::pid(), Notifier::pid(), message, term()}</code> message.

notify(Pid, Term) ->
  Pid ! {self(), notify, Term},
  ok.

%% @spec flush(xlat()) -> ok
%% @doc Causes the xlat server to flush all its characters to the output,
%% even ones in the process of matching candidate strings.  Typically
%% this is called before the output is closed.

flush(Pid) ->
  Pid ! {self(), flush},
  receive
    {Pid, flush, Result} ->
      Result
  end.

test() ->
  lists:foreach(fun({S, P}) ->
    test(S, P),
    io:fwrite("\n\n")
  end,
  [
    {"Oh look, a here-doc.",
      [{"er", "e"}]},
    {"Oh look, a here-doc.",
      [{"re", "e"}]},
    {"Oh look, a here-doc.",
      [{"er", "!"}]},
    {"Oh look, a here-doc.",
      [{"re", "!"}]},
    {"Brak and Zorak were both eaten by a bear.",
      [{"Brak", "Zorak"}, {"Zorak", "Brak"}]},
    {"Hello, world!",
      [{"lo", "LO"}, {"ld", "KPLACE"}, {" ", ""}, {"!", "?"}]}
  ]).
  
test(String, XlatList) ->
  Xlat = start(XlatList, self()),
  send(Xlat, lists:flatten(String)),
  flush(Xlat),
  flush().
flush() ->
  receive
    {Xlat, xlat_char, Char} ->
      io:fwrite("~c", [Char]),
      flush();
    X ->
      io:fwrite("~p~n", [X]),
      flush()
    after 2000 ->
      ok
  end.

%%% END of ce_xlat.erl %%%
