%%% BEGIN bf.erl %%%
%%%
%%% bf - pedantic Brainf*ck interpreter in Erlang
%%% Copyright (c)2002 Cat's Eye Technologies.  All rights reserved.
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

%% @doc Cat's Eye Technologies' Erlang Brainf*ck Interpreter.
%%
%% <p>An implementation of the world's most beautifulest langugage
%% in the world's second most beautifulest language.</p>
%% <p>This program demonstrates:</p>
%% <ul>
%% <li> sequential programming in Erlang </li>
%% <li> simulating Brainf*ck's updatable store by passing arguments </li>
%% </ul>
%% <p>Variable names used in this program:</p>
%% <ul>
%% <li> I = Instruction pointer </li>
%% <li> B = Brainf*ck program </li>
%% <li> D = Data pointer (tape head position) </li>
%% <li> M = Memory (Brainf*ck tape) </li>
%% </ul>
%%
%% @end

-module(bf).
-vsn('2002.1208').
-copyright('Copyright (c)2002 Cat`s Eye Technologies. All rights reserved.').

-export([interpret/1, interpret/2, test/1]).

%% @spec interpret(B::[instruction()], StorageSize::integer()) ->
%%   {integer(), tuple()}
%%         instruction() = integer()
%% @doc The main user interface to the Brainf*ck interpreter.
%% The user generally passes a list, which is parsed into a tuple.
%% In this tuple, each Brainf*ck instruction represented by
%% an atom, except for <code>[</code> and <code>]</code>,
%% which are represented by a nested tuple to make interpretation simpler.
%% The return value is a tuple containing the data pointer and
%% another tuple representing the state of the Brainf*ck tape
%% after all is said and done.

interpret(B, N) when tuple(B) -> interpret(1, B, 1, erlang:make_tuple(N, 0));
interpret(B, N) when list(B)  -> interpret(parse(B), N).

%% @spec interpret(B::[instruction()]) -> {integer(), tuple()}
%% @equiv interpret(Program, 512)

interpret(B) -> interpret(B, 512).  % default memsize, use interpret/2 to specify

%% @spec interpret(I::integer(), B::[instruction()], D::integer(), M::tuple) -> {integer(), tuple()}
%% @doc The internal driver which implements the execution loop.
%% When the I pointer is at the end of the program, processing is finished.
%% But more usually, I will be travelling forward through B.

interpret(I, B, D, M) when I > size(B) -> {D, M};
interpret(I, B, D, M) ->
  {D2, M2} = execute(element(I, B), D, M),
  interpret(I + 1, B, D2, M2).

%% @spec execute(instruction(), D::integer(), M::tuple) -> {integer(), tuple()}
%% @doc Executes specific, individual instructions.  Erlang doesn't have an
%% updatable store like Brainf*ck (unless you count the process dictionary
%% (which you probably shouldn't,)) so instead we approach the problem by
%% continually deriving new stores from old stores, returning the new
%% store to the caller each time this function is called.

execute($>, D, M) -> {D + 1, M};
execute($<, D, M) -> {D - 1, M};
execute($+, D, M) -> {D, setelement(D, M, element(D, M) + 1)};
execute($-, D, M) -> {D, setelement(D, M, element(D, M) - 1)};

%% <p>I/O is fairly crude, and could stand to be improved.</p>

execute($., D, M) -> io:put_chars([element(D, M)]), {D, M};
execute($,, D, M) -> {D, setelement(D, M, hd(io:get_chars('bf> ', 1)))};

%% <p>The 'while' loop.  A tuple represents a [...] structure; if
%% the data pointer points to a non-zero, the nested Brainf*ck
%% subprogram is executed, and the check is repeated.</p>

execute(B,  D, M) when tuple(B), element(D, M) == 0 -> {D, M};
execute(B,  D, M) when tuple(B) ->
  {D2, M2} = interpret(1, B, D, M),
  execute(B, D2, M2);

%% <p>Finally, comments and other line noise are ignored.</p>

execute(_, D, M) -> {D, M}.

%% @spec parse([instruction()]) -> tuple()
%% @doc Takes a string (list of ASCII values) and butchers
%% it into a nested tuple data structure, suitable for interpretation.
%% Writing this function elegantly
%% was much trickier than writing the imperative processing engine above.

parse(L) -> parse({}, L).  % default is to add to fresh tuple

parse(U, []) -> U;
parse(U, [$]|T]) -> {U, T};
parse(U, [$[|T]) ->
  {V, L} = parse({}, T),
  parse(erlang:append_element(U, V), L);
parse(U, [H |T]) ->
  parse(erlang:append_element(U, H), T).

%% @spec test(Test::integer()) -> {integer(), tuple()}
%% @doc Test functions, numbered from 1 upwards.  They implement two of the
%% programs from the original Brainf*ck archive (hello and atoi).

test(1) -> test(hello);
test(hello) -> interpret("
>+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]
<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[
<++++>-]<+.[-]++++++++++."
);

test(2) -> test(atoi);
test(atoi) -> interpret("
==== ==== ====
cont digi num
==== ==== ====

+
[
 -                         cont=0
 >,
 ======SUB10======
 ----------
 
 [                         not 10
  <+>                      cont=1
  =====SUB38======
  ----------
  ----------
  ----------
  --------

  >
  =====MUL10=======
  [>+>+<<-]>>[<<+>>-]<     dup

  >>>+++++++++
  [
   <<<
   [>+>+<<-]>>[<<+>>-]<    dup
   [<<+>>-]
   >>-
  ]
  <<<[-]<
  ======RMOVE1======
  <
  [>+<-]
 ]
 <
]
>>[<<+>>-]<<
#"
).

%%% END of bf.erl %%%
