%%% BEGIN pibfi_interpreter.erl %%%
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

%% @doc Interpreter for <code>pibfi</code>.
%%
%% @end

-module(pibfi_interpreter).
-vsn('2003.0427').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([interpret/4]).

%% @spec interpret(program(), ParserOptions::[{atom(), term()}],
%%         Tape::pid(), IoPid::pid()) -> ok
%% @doc Interprets a Brainf*ck program.  If the program is given in
%% an unparsed (list or binary) form, it will be parsed into a
%% tuple form before proceeding.

interpret(Program, ParserOptions, TapePid, IoPid) when is_binary(Program) ->
  interpret(pibfi_parser:parse(Program, ParserOptions), 1, TapePid, IoPid);
interpret(Program, ParserOptions, TapePid, IoPid) when is_list(Program) ->
  interpret(pibfi_parser:parse(Program, ParserOptions), 1, TapePid, IoPid);
interpret(Program, ParserOptions, TapePid, IoPid) ->
  [StatusCmd] = pibfi_options:get_option(ParserOptions, statuscmd, "#"),
  interpret(Program, 1, TapePid, IoPid, StatusCmd).

interpret(Program, IP, TapePid, IoPid, StatusCmd) when IP > size(Program) ->
  ok;
interpret(Program, IP, TapePid, IoPid, StatusCmd) ->
  Instruction = element(IP, Program),
  case Instruction of
    {instruction, Row, Col, Ins} ->
      pibfi_statistics:update_program(Row, Col, Ins);
    {while, Row, Col, Block} ->
      ok
  end,
  case execute(Instruction, TapePid, IoPid, StatusCmd) of
    stop ->
      stop;
    _ ->
      interpret(Program, IP + 1, TapePid, IoPid, StatusCmd)
  end.

%% @spec execute(instruction(), Tape::pid(), IO::pid(), StatusCmd) -> ok
%% @doc Executes a single Brainf*ck instruction.

execute({instruction, Row, Column, $>}=I, TapePid, IoPid, StatusCmd) ->
  pibfi_tape:right(TapePid);
execute({instruction, Row, Column, $<}=I, TapePid, IoPid, StatusCmd) ->
  pibfi_tape:left(TapePid);
execute({instruction, Row, Column, $+}=I, TapePid, IoPid, StatusCmd) ->
  pibfi_tape:increment(TapePid);
execute({instruction, Row, Column, $-}=I, TapePid, IoPid, StatusCmd) ->
  pibfi_tape:decrement(TapePid);

execute({instruction, Row, Column, {$>, N}}=I, TapePid, IoPid, StatusCmd) ->
  pibfi_tape:right(TapePid, N);
execute({instruction, Row, Column, {$<, N}}=I, TapePid, IoPid, StatusCmd) ->
  pibfi_tape:left(TapePid, N);
execute({instruction, Row, Column, {$+, N}}=I, TapePid, IoPid, StatusCmd) ->
  pibfi_tape:increment(TapePid, N);
execute({instruction, Row, Column, {$-, N}}=I, TapePid, IoPid, StatusCmd) ->
  pibfi_tape:decrement(TapePid, N);

execute({instruction, Row, Column, $.}=I, TapePid, IoPid, StatusCmd) ->
  Cell = pibfi_tape:read(TapePid),
  pibfi_io:output(IoPid, Cell),
  ok;
execute({instruction, Row, Column, $,}=I, TapePid, IoPid, StatusCmd) ->
  % ce_log:write("input"),
  case pibfi_io:input(IoPid) of
    nop ->
      ok;
    stop ->
      stop;
    Character when is_integer(Character) ->
      % ce_log:write("input ~c", [Character]),
      pibfi_tape:write(TapePid, Character),
      % ce_log:write("input stored as ~p", [pibfi_tape:read(TapePid)]),
      ok
  end;
execute({instruction, Row, Column, StatusCmd}=I, TapePid, IoPid, StatusCmd) ->
  pibfi_statistics:dump(),
  ok;
execute({while, Row, Column, SubProgram}=I, TapePid, IoPid, StatusCmd)
 when tuple(SubProgram) ->
  pibfi_statistics:update_program(Row, Column, $[),
  case pibfi_tape:read(TapePid) of
    0 ->
      pibfi_statistics:update_program(Row, Column, $]),
      ok;
    _ ->
      case interpret(SubProgram, 1, TapePid, IoPid, StatusCmd) of
        stop ->
	  stop;
	_ ->
          pibfi_statistics:update_program(Row, Column, $]),
          execute(I, TapePid, IoPid, StatusCmd)
      end
  end;
execute(_, TapePid, IoPid, StatusCmd) ->
  ok.

%%% END of pibfi_interpreter.erl %%%
