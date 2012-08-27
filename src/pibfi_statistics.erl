%%% BEGIN pibfi_statistics.erl %%%
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

%% @doc Statistics collector for <code>pibfi</code>.
%%
%% @end

-module(pibfi_statistics).
-vsn('2003.0505').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([start/3, server/2, dump/0, dump/1]).
-export([update_program/3]).

start(Supervisor, TapePid, Options) ->
  pibfi_supervisor:spawn_link(Supervisor, "statistics collector",
    noncritical, ?MODULE, server, [TapePid, Options]).

server(TapePid, Options) ->
  register(?MODULE, self()),
  case pibfi_options:get_option(Options, statusevery, undefined) of
    I when is_integer(I) ->
      timer:apply_interval(I, ?MODULE, dump, []);
    _ ->
      ok
  end,   
  Start = calendar:local_time(),
  loop({TapePid, 1, 1, $?, Start, {0,0,0,0,0,0,0,0,0}}).

loop({TapePid, ProgramRow, ProgramColumn, Instruction, Start,
      {PlusT, MinusT, LeftT, RightT,
       InT, OutT, WhileT, WendT, WhileLevel}}) ->
  receive
    {program, Row, Col, {Ins, N}} ->
      PlusT1 = case Ins of $+ -> PlusT + N; _ -> PlusT end,
      MinusT1 = case Ins of $- -> MinusT + N; _ -> MinusT end,
      LeftT1 = case Ins of $< -> LeftT + N; _ -> LeftT end,
      RightT1 = case Ins of $> -> RightT + N; _ -> RightT end,
      InT1 = case Ins of $, -> InT + N; _ -> InT end,
      OutT1 = case Ins of $. -> OutT + N; _ -> OutT end,
      WhileT1 = case Ins of $[ -> WhileT + N; _ -> WhileT end,
      WendT1 = case Ins of $] -> WendT + N; _ -> WendT end,
      WhileLevel1 = case Ins of
        $[ ->
	  WhileLevel + 1;
	$] ->
	  WhileLevel - 1;
	_ ->
	  WhileLevel
      end,
      % ce_log:write("~c -> ~c ~p", [Instruction, Ins, N]),
      loop({TapePid, Row, Col, Ins, Start,
        {PlusT1, MinusT1, LeftT1, RightT1,
	 InT1, OutT1, WhileT1, WendT1, WhileLevel1}});
    {program, Row, Col, Ins} ->
      PlusT1 = case Ins of $+ -> PlusT + 1; _ -> PlusT end,
      MinusT1 = case Ins of $- -> MinusT + 1; _ -> MinusT end,
      LeftT1 = case Ins of $< -> LeftT + 1; _ -> LeftT end,
      RightT1 = case Ins of $> -> RightT + 1; _ -> RightT end,
      InT1 = case Ins of $, -> InT + 1; _ -> InT end,
      OutT1 = case Ins of $. -> OutT + 1; _ -> OutT end,
      WhileT1 = case Ins of $[ -> WhileT + 1; _ -> WhileT end,
      WendT1 = case Ins of $] -> WendT + 1; _ -> WendT end,
      WhileLevel1 = case Ins of
        $[ ->
	  WhileLevel + 1;
	$] ->
	  WhileLevel - 1;
	_ ->
	  WhileLevel
      end,
      % ce_log:write("~c -> ~c", [Instruction, Ins]),
      loop({TapePid, Row, Col, Ins, Start,
        {PlusT1, MinusT1, LeftT1, RightT1,
	 InT1, OutT1, WhileT1, WendT1, WhileLevel1}});
    {Pid, dump, Type} ->
      % flush mailbox here (?)
      Stop = calendar:local_time(),
      StartGD = calendar:datetime_to_gregorian_seconds(Start),
      StopGD = calendar:datetime_to_gregorian_seconds(Stop),
      Dur = StopGD - StartGD,
      DurString = case Dur of
        D when D < 60 ->
	  io_lib:format("~p sec", [D]);
        D when D < 3600 ->
	  io_lib:format("~p min ~p sec", [D div 60, D rem 60]);
        D ->
	  M = D rem 3600,
	  io_lib:format("~p hr ~p min ~p sec",
	   [D div 3600, M div 60, M rem 60])
      end,
      TotalT = PlusT + MinusT + LeftT + RightT + InT + OutT + WhileT + WendT,
      KIPS = round((TotalT / Dur) / 1000),
      io:fwrite("===== STATUS REPORT ===== ~s =====~n", [format_datetime(Stop)]),
      io:fwrite("= Run began ~s duration ~s at mean kips ~p~n",
        [format_datetime(Start), DurString, KIPS]),
      State = case Type of
        autopsy -> "Stopped";
        normal -> "Currently"
      end,
      io:fwrite("= ~s at line ~p, col ~p, instruction '~c', whilelevel ~p~n",
        [State, ProgramRow, ProgramColumn, Instruction, WhileLevel]),
      io:fwrite("= Exec tally:~12wx(+)~12wx(-)~12wx(<)~12wx(>)~n",
        [PlusT, MinusT, LeftT, RightT]),
      io:fwrite("= Exec tally:~12wx(,)~12wx(.)~12wx([)~12wx(])~n",
        [InT, OutT, WhileT, WendT]),
      pibfi_tape:examine(TapePid),
      Pid ! {?MODULE, dump, ok},
      loop({TapePid, ProgramRow, ProgramColumn, Instruction, Start,
            {PlusT, MinusT, LeftT, RightT,
	     InT, OutT, WhileT, WendT,
	     WhileLevel}})
  end.

format_datetime({{Y, M, D}, {H, I, S}}) ->
  DoW = element(calendar:day_of_the_week({Y,M,D}),
    {"Mon","Tue","Wed","Thu","Fri","Sat","Sun"}),
  MoY = element(M,
    {"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"}),
  T = io_lib:format("~s ~s ~w ~w, ~2.2.0w:~2.2.0w:~2.2.0w",
    [DoW, MoY, D, Y, H, I, S]),
  lists:flatten(T).

dump() ->
  dump(normal).
  
dump(Type) ->
  case catch ?MODULE ! {self(), dump, Type} of
    {'EXIT', Reason} ->
      {error, Reason};
    _ ->
      receive
        {?MODULE, dump, ok} ->
	  ok
	after 1000 ->
	  {error, timeout}
      end
  end.

update_program(Row, Col, Ins) ->
  catch ?MODULE ! {program, Row, Col, Ins}.

%%% END of pibfi_statistics.erl %%%
