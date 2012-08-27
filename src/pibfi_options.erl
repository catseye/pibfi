%%% BEGIN pibfi_options.erl %%%
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

%% @doc Options parser for <code>pibfi</code>.
%%
%% <h3>Synopsis</h3>
%%
%% <p>This module implements collecting options
%% from the command line.</p>
%%
%% <p>When invoking <code>pibfi</code> from the command line, the
%% following syntax can be used:</p>
%%
%% <ul><code>erl -noshell <i>options</i> -run pibfi run <i>filename</i></code></ul>
%%
%% <p>The following options are recognized.</p>
%%
%% <ul>
%% <li>Options pertaining to the parser:
%%
%% <ul>
%% <li><code>-dontstrip <i>s</i></code>, where <i>s</i> is a string of
%% characters, default null (empty), specifies the set of characters
%% (beyond the 8 basic Brainf*ck instructions)
%% which
%% will not be stripped from the parsed source prior to optimization.
%% Note that if this option if given without an argument, it is assumed
%% to be the set of all applicable characters - in other words, nothing
%% at all will be stripped.</li>
%%
%% <li><code>-optimize <i>n</i></code>, where <i>n</i> is an integer,
%% default 1, sets the optimization level of the parser.
%% At optimization level 0, no optimization occurs.
%% At optimization level 1, run-length encoding is performed on the
%% instructions.  Further optimization levels may be defined in the
%% future.  Note that if this option if given without an argument,
%% it is assumed to be asking for the most thorough yet quickly
%% available optimization.</li>
%%
%% <li><code>-statuscmd <i>c</i></code>, where <i>c</i> may be
%% any single-character string of the term <code>undefined</code>,
%% default "#" (octalthorpe), specifies
%% which symbol, when encountered, is treated
%% as a debugging instruction which displays a status report.</li>
%%
%% <li><code>-heredoc <i>c</i></code>, where <i>c</i> may be
%% a single-character string or the term <code>undefined</code>,
%% default <code>undefined</code>, specifies which symbol when encountered,
%% is treated as the marker that signifies that input is embedded in
%% the program source code following it.  This input will be stripped
%% from the source (even if it contains valid Brainf*ck instructions)
%% and will be made available to <code>-infile heredoc</code> (see
%% below.)  Note that if this option is given with no argument,
%% "!" (exclamation point) is assumed.</li>
%%
%% </ul>
%% </li>
%% <li>Options pertaining to the Brainf*ck tape:
%%
%% <ul>
%% <li><code>-tapemodule <i>m</i></code>, where <i>n</i> is the name of
%% an Erlang module conforming to the <code>pibfi_tape</code> behaviour,
%% defaulting to whatever <code>pibfi</code> determines would be optimal
%% for the given sourcecode, (currently always <code>pibfi_tape_ets</code>),
%% names the module which implements the backing for the simulated tape.</li>
%%
%% <li><code>-maxcell <i>n</i></code>, where <i>n</i> is any integer or the
%% term <code>infinity</code>, default <code>infinity</code>, sets the
%% highest value that can be placed into a cell on the tape.</li>
%%
%% <li><code>-mincell <i>n</i></code>, where <i>n</i> is any integer or the
%% term <code>infinity</code>, default 0, sets the
%% lowest value that can be placed into a cell on the tape.
%% (Note that <code>infinity</code> actually represents negative infinity
%% here.)</li>
%%
%% <li><code>-wrapcell <i>b</i></code>, where <i>b</i> is a boolean
%% (<code>true</code> or <code>false</code>), default <code>false</code>,
%% determines what happens when either limit of any cell in the tape is
%% exceeded.  When <code>-wrapcell</code> is <code>true</code>, the limits
%% will be taken as modulus boundaries, and the value will 'wrap around'
%% to the opposite limit.  When <code>-wrapcell</code> is <code>false</code>,
%% an exception will be generated.  Note that if <i>b</i> is omitted
%% after <code>-wrapcell</code>, <code>true</code> is assumed.
%% Also note that <code>-wrapcell true</code> is not compatible with
%% either <code>-maxcell infinity</code> or <code>-mincell infinity</code>
%% (for what should be obvious reasons.)</li>
%%
%% <li><code>-maxtape <i>n</i></code>, where <i>n</i> is any integer or the
%% term <code>infinity</code>, default <code>infinity</code>, sets the
%% rightmost position to which the tape head can move.  Note that the initial
%% position of the tape head is considered position 0.</li>
%%
%% <li><code>-mintape <i>n</i></code>, where <i>n</i> is any integer or the
%% term <code>infinity</code>, default 0, sets the
%% leftmost position to which the tape head can move.
%% (Note that <code>infinity</code> actually represents negative infinity
%% here.)</li>
%%
%% <li><code>-wraptape <i>b</i></code>, where <i>b</i> is a boolean,
%% default <code>false</code>, determines what happens when either limit
%% of the tape head is exceeded.  With <code>-wraptape true</code>,
%% the limits will be taken as modulus boundaries,
%% and the position of the tape head will 'wrap around' to the
%% opposite limit.  With <code>-wraptape false</code>,
%% an exception will be generated when this happens.
%% Note that if <i>b</i> is omitted
%% after <code>-wraptape</code>, <code>true</code> is assumed. Also note
%% that <code>-wraptape true</code> is not compatible with either
%% <code>-maxtape infinity</code> or <code>-mintape infinity</code>.</li>
%% </ul>
%% </li>
%%
%% <li>Options pertaining to input and output:
%%
%% <ul>
%% <li><code>-infile <i>s</i></code>, where <i>s</i> is a filename or
%% one of the terms <code>tty</code> or <code>heredoc</code>,
%% default <code>tty</code>, sets the
%% source of the input to the Brainf*ck program.
%% <code>tty</code> indicates an interactive terminal session with
%% "standard input".
%% <code>heredoc</code> indicates input will come from the
%% "here-doc" portion of the program source code (note that the
%% <code>-heredoc</code> option must also be given to parse the
%% source code.)
%% Using this option is preferred
%% over redirecting standard I/O with the shell, as it is a hint to the
%% interpreter that the program is not being run interactively.</li>
%%
%% <li><code>-outfile <i>s</i></code>, where <i>s</i> is a filename or
%% the term <code>tty</code>, default <code>tty</code>, sets the
%% destination of the output of the Brainf*ck program.  This is preferred
%% to redirecting standard I/O with the shell, as it is a hint to the
%% interpreter that the program is not being run interactively.</li>
%%
%% <li><code>-maxout <i>n</i></code>, where <i>n</i> is any integer or the
%% term <code>infinity</code>, default <code>infinity</code>, sets the
%% maximum character value which can be output.</li>
%%
%% <li><code>-minout <i>n</i></code>, where <i>n</i> is any integer or the
%% term <code>infinity</code>, default 0, sets the minimum character
%% value which can be output.
%% (Note that <code>infinity</code> actually represents negative infinity
%% here.)</li>
%%
%% <li><code>-wrapout <i>b</i></code>, where <i>b</i> is a boolean,
%% default <code>false</code>, determines what happens when either limit
%% of character output is exceeded.  With <code>-wrapout true</code>,
%% the limits will be taken as modulus boundaries,
%% and the actual character output will be computed by 'wrapping around'
%% the rquested value to the opposite limit.
%% With <code>-wrapout false</code>,
%% an exception will be generated.  Note that if <i>b</i> is omitted
%% after <code>-wrapout</code>, <code>true</code> is assumed. Also note
%% that <code>-wrapout true</code> is not compatible with either
%% <code>-maxout infinity</code> or <code>-minout infinity</code>.</li>
%%
%% <li><code>-maxin <i>n</i></code>, where <i>n</i> is any integer or the
%% term <code>infinity</code>, default <code>infinity</code>, sets the
%% maximum character value which can be input.</li>
%%
%% <li><code>-minin <i>n</i></code>, where <i>n</i> is any integer or the
%% term <code>infinity</code>, default 0, sets the minimum character
%% value which can be input.
%% (Note that <code>infinity</code> actually represents negative infinity
%% here.)</li>
%%
%% <li><code>-wrapin <i>b</i></code>, where <i>b</i> is a boolean,
%% default <code>false</code>, determines what happens when either limit
%% of character input is exceeded.  With <code>-wrapin true</code>,
%% the limits will be taken as modulus boundaries,
%% and the actual character input will be computed by 'wrapping around'
%% the rquested value to the opposite limit.
%% With <code>-wrapin false</code>,
%% an exception will be generated.  Note that if <i>b</i> is omitted
%% after <code>-wrapin</code>, <code>true</code> is assumed. Also note
%% that <code>-wrapin true</code> is not compatible with either
%% <code>-maxin infinity</code> or <code>-minin infinity</code>.</li>
%%
%% <li><code>-xlatout <i>s</i></code>, where <i>s</i> is string
%% in the form given below,
%% default value <tt>#10=#nl</tt>, specifies a mapping between
%% characters the Brainf*ck program sees itself as sending to output,
%% and the characters that the operating system actually receives.
%% The syntax for the mapping specification is described by this mini-grammar:
%% <ul><li>
%% <tt><i>string</i> "=" <i>string</i> ["," <i>string</i> "=" <i>string</i> ]</tt>
%% </li></ul>
%% Control and other characters can be embedded in this string by
%% giving their ASCII values in decimal preceded by a <tt>#</tt>
%% symbol.  A single <tt>#</tt> symbol can be represented by the
%% sequence <tt>#35</tt>.  A single <tt>=</tt> symbol can be
%% represented by the sequence <tt>#61</tt>.
%% A single <tt>,</tt> symbol can be
%% represented by the sequence <tt>#44</tt>.
%% The current operating system newline
%% sequence can be represented by <tt>#nl</tt>.
%% Note that whitespace is not allowed in this grammar,
%% and must be represented by <tt>#32</tt>, etc.
%% Note that if the <code>-xlatout</code> option is present
%% with no argument, this means that there should be <i>no</i> translation
%% mapping between Brainf*ck output and operating system output.</li>
%%
%% <li><code>-xlatin <i>s</i></code>, where <i>s</i> is a string
%% in the form given above,
%% default value <tt>#nl=#10</tt>, specifies a complementary mapping between
%% characters on the operating system's input and
%% the Brainf*ck program's input.
%% Note that if the <code>-xlatin</code> option is present
%% with no argument, this means that there should be no translation
%% mapping of input.</li>
%%
%% <li><code>-eof <i>i</i></code>, where <i>i</i> is any integer
%% or one of the terms <code>halt</code>, <code>stop</code>,
%% or <code>nop</code>, default 0,
%% determines the single ASCII character that will be given repeatedly to
%% the Brainf*ck program during execution of the <code>,</code> instruction
%% at and after the end of input is encountered.
%% If <code>halt</code> is given for <code>eof</code>, the Brainf*ck
%% program will be halted with an error
%% if it attempts to read past the end of user input.
%% If <code>stop</code> is given for <code>eof</code>, the Brainf*ck
%% program will be terminated normally if it attempts to read past
%% the end of user input.
%% If <code>nop</code> is given for <code>eof</code>, the Brainf*ck
%% program will act as if nothing at all happened if it attempts
%% to read past the end of user input (the tape will not be altered in
%% any way.)</li>
%% </ul>
%% </li>
%%
%% <li>Debugging and other options:</li>
%% <ul>
%% <li><code>-quiet</code>, if given, suppresses all startup
%% output generated by the interpreter.  It does not suppress
%% status reports.</li>
%% <li><code>-autopsy</code>, if given, causes the interpreter to
%% issue a status report after the program terminates normally.</li>
%% <li><code>-statusevery <i>n</i></code>, where <i>n</i> is an
%% integer specifying a duration or the term <code>undefined</code>,
%% default <code>undefined</code>, tells <code>pibfi</code>
%% to generate periodic status reports at the given interval,
%% if it is not <code>undefined</code>.  If the duration is given
%% as a plain integer, units of milliseconds are assumed.  The
%% duration may also be given as an integer followed immediately
%% by <code>s</code>, <code>m</code>, or <code>h</code>, in which
%% case the units of measurement will be taken to be seconds,
%% minutes, or hours, respectively.</li>
%% </ul>
%% </ul>
%%
%% @end

-module(pibfi_options).
-vsn('2003.0505').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([get_flag/1, get_opts/1]).
-export([get_option/3]).

%% @spec get_flag(Switch::atom()) -> true | false
%% @doc Gets a switch from the command line.

get_flag(Option) ->
  case init:get_argument(Option) of
    {ok, [X]} ->
      true;
    error ->
      false
  end.

%% @spec get_opts([atom()]) -> [{atom(), term()}]
%% @doc Gets a set of options from the command line.

get_opts(Options) ->
  lists:foldl(fun(Option, Acc) when is_atom(Option) ->
    case init:get_argument(Option) of
      error ->
        Acc;
      {ok, [[]]} ->
        [{Option, missing(Option)} | Acc];
      {ok, [[Arg | _] | _]} ->
        [{Option, convert(Option, Arg)} | Acc]
    end;
    ({Option, Default}, Acc) ->
    case init:get_argument(Option) of
      error ->
        [{Option, Default} | Acc];
      {ok, [[]]} ->
        [{Option, missing(Option)} | Acc];
      {ok, [[Arg | _] | _]} ->
        [{Option, convert(Option, Arg)} | Acc]
    end
  end, [], Options).

%% @spec missing(atom()) -> term()
%% @doc Gets the default value for an option specified with no value
%% on the command line.

missing(wrapcell) -> true;
missing(wraptape) -> true;
missing(wrapout)  -> true;
missing(wrapin)   -> true;
missing(xlatin)   -> [];
missing(xlatout)  -> [];
missing(dontstrip) -> lists:seq(0, 255);
missing(optimize) -> 1;
missing(statuscmd) -> "#";
missing(heredoc)  -> "!";
missing(Else)     -> exit({missing_parameter_for, Else}).

%% @spec convert(atom(), string()) -> term()
%% @doc Converts an option value specified on the command line to a
%% term that <code>pibfi</code> can work with internally.

convert(dontstrip, String)       -> unescape(String);
convert(optimize, Integer)       -> list_to_integer(Integer);
convert(statuscmd, "undefined")  -> undefined;
convert(statuscmd, String)       -> unescape(String);
convert(heredoc, "undefined")    -> undefined;
convert(heredoc, String)         -> unescape(String);

convert(tapemodule, String)  -> list_to_atom(String);
convert(maxcell, "infinity") -> infinity;
convert(maxcell, Else)       -> list_to_integer(Else);
convert(mincell, "infinity") -> infinity;
convert(mincell, Else)       -> list_to_integer(Else);
convert(wrapcell, "true")    -> true;
convert(wrapcell, "false")   -> false;
convert(maxtape, "infinity") -> infinity;
convert(maxtape, Else)       -> list_to_integer(Else);
convert(mintape, "infinity") -> infinity;
convert(mintape, Else)       -> list_to_integer(Else);
convert(wraptape, "true")    -> true;
convert(wraptape, "false")   -> false;

convert(infile, "tty")       -> tty;
convert(infile, "heredoc")   -> heredoc;
convert(infile, Else)        -> Else;
convert(outfile, "tty")      -> tty;
convert(outfile, Else)       -> Else;
convert(maxout, "infinity")  -> infinity;
convert(maxout, Else)        -> list_to_integer(Else);
convert(minout, "infinity")  -> infinity;
convert(minout, Else)        -> list_to_integer(Else);
convert(wrapout, "true")     -> true;
convert(wrapout, "false")    -> false;
convert(maxin, "infinity")   -> infinity;
convert(maxin, Else)         -> list_to_integer(Else);
convert(minin, "infinity")   -> infinity;
convert(minin, Else)         -> list_to_integer(Else);
convert(wrapin, "true")      -> true;
convert(wrapin, "false")     -> false;
convert(xlatin, String)      -> parse_xlat(String, []);
convert(xlatout, String)     -> parse_xlat(String, []);
convert(eof, "halt")         -> halt;
convert(eof, "nop")          -> nop;
convert(eof, "stop")         -> stop;
convert(eof, Else)           -> list_to_integer(Else);

convert(statusevery, "undefined") -> undefined;
convert(statusevery, Else) ->
  Factor = case lists:last(Else) of
    $s -> 1000;
    $m -> 60*1000;
    $h -> 60*60*1000;
    _ -> 1
  end,
  Root = case Factor of
    1 -> Else;
    _ -> lists:reverse(tl(lists:reverse(Else)))
  end,
  % ce_log:write("~p ~p", [Root, Factor]),
  list_to_integer(Root) * Factor.

parse_xlat(String, Acc) ->
  {First,  String0} = parse_lstring(String, ""),
  {Second, String1} = parse_rstring(String0, ""),
  pibfi:assert(First =/= "", {lefthand_string_in_xlat_may_not_be_null, String}),
  Acc0 = [{unescape(First), unescape(Second)} | Acc],
  case String1 of
    "," ++ String2 ->
      parse_xlat(String2, Acc0);
    _ ->
      Acc0
  end.
  
parse_lstring("=" ++ Tail, Acc) ->
  {lists:reverse(Acc), Tail};
parse_lstring([Head | Tail], Acc) ->
  parse_lstring(Tail, [Head | Acc]).

parse_rstring("", Acc) ->
  {lists:reverse(Acc), ""};
parse_rstring("," ++ Tail, Acc) ->
  {lists:reverse(Acc), "," ++ Tail};
parse_rstring([Head | Tail], Acc) ->
  parse_rstring(Tail, [Head | Acc]).

%% @spec unescape(string()) -> string()
%% @doc Transforms escape codes in a string into embedded characters.
%% The escape code <tt>#<i>i</i></tt> where <tt><i>i</i></tt> is a
%% decimal integer of from one to three digits is converted into an
%% ASCII character of that value.

unescape(String) ->
  unescape(String, []).
unescape([], Acc) ->
  lists:reverse(Acc);
unescape([$#, $n, $l | Tail], Acc) ->
 unescape(Tail, lists:reverse(pibfi:os_eol()) ++ Acc);
unescape([$#, D1, D2, D3 | Tail], Acc)
 when D1 >= $0, D1 =< $9, D2 >= $0, D2 =< $9, D3 >= $0, D3 =< $9 ->
  unescape(Tail, [(D1 - $0) * 100 + (D2 - $0) * 10 + (D3 - $0) | Acc]);
unescape([$#, D1, D2 | Tail], Acc)
 when D1 >= $0, D1 =< $9, D2 >= $0, D2 =< $9 ->
  unescape(Tail, [(D1 - $0) * 10 + (D2 - $0) | Acc]);
unescape([$#, D | Tail], Acc)
 when D >= $0, D =< $9 ->
  unescape(Tail, [(D - $0) | Acc]);
unescape([Head | Tail], Acc) ->
  unescape(Tail, [Head | Acc]).

%% @spec get_option(Options::[{atom(), term()}], Option::atom(), Default::term()) -> term()
%% @doc Gets an option from a list of (already parsed) option tuples.

get_option(TupleList, Option, Default) ->
  case lists:keysearch(Option, 1, TupleList) of
    {value, {Option, Value}} ->
      Value;
    _ ->
      Default
  end.

%%% END of pibfi_options.erl %%%
