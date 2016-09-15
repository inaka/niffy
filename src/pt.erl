-module(pt).
-export([parse_transform/2]).

%%==============================================================================
%% API
%%==============================================================================
parse_transform(Forms, _Options) ->
  case config(Forms) of
    undefined ->
      % If no configuration was found, we have nothing to do on this file
      Forms;
    Config ->
      % Get the configuration values
      Functions = maps:get(functions, Config, []),
      Includes = maps:get(includes, Config, []),
      Flags = maps:get(flags, Config, []),
      % Find the module name
      Module = module(Forms),
      % Now, parse the Forms and get the C code
      {CFunctions, Forms2} = funtions_from_forms(Functions, Forms),
      % Build the C file
      NIFId = get_nif_id(Module),
      CFileName = NIFId ++ ".c",
      CCode = build_c_code(Module, Includes, CFunctions),
      ok = file:write_file(CFileName, CCode),
      % Compile the C file
      _ = os:cmd(["gcc -o ", NIFId, ".so ", flags(Flags), " ", CFileName]),
      % Make sure we have an init function
      {InitFunc, Forms3} = ensure_init_function(Module, Forms2),
      % Add the nif loading code to the init function
      Forms4 = add_nif_loader(Forms3, InitFunc, NIFId),
      % Return the new forms
      Forms4
  end.

%%==============================================================================
%% Utils
%%==============================================================================
% Retrieve the configuration by checking every attribute until we find the niffy
% one; if none is found, return undefined
config([]) ->
  undefined;
% There is the full map configuration for niffy
config([{attribute, _, niffy, Config} | _]) when is_map(Config) ->
  Config#{functions := normalize_functions(maps:get(functions, Config))};
% And also a simple function list, in this case, we need to build the map
config([{attribute, _, niffy, Functions} | _]) when is_list(Functions) ->
  #{functions => normalize_functions(Functions)};
config([_ | T]) ->
  config(T).

% Make sure all the function specifications have the optional DirtyMode value
normalize_functions(Functions) ->
  normalize_functions(Functions, []).

normalize_functions([], Acc) ->
  Acc;
normalize_functions([{{F, A}, DirtyMode} | T], Acc) ->
  normalize_functions(T, [{F, A, DirtyMode} | Acc]);
normalize_functions([{F, A} | T], Acc) ->
  normalize_functions(T, [{F, A, none} | Acc]).

% Retrieve the module name
module([{attribute, _, module, Module} | _]) ->
  Module;
module([_ | T]) ->
  module(T).

% Get the C code of each of the functions on the niffy config
funtions_from_forms(Functions, Forms) ->
  funtions_from_forms(Functions, Forms, [], []).

funtions_from_forms([], _, Functions, Forms) ->
  {Functions, lists:reverse(Forms)};
funtions_from_forms(_, [], Functions, Forms) ->
  {Functions, lists:reverse(Forms)};
funtions_from_forms(FunctionNames,
                    [{function, L1, F, A,
                      [{clause, _, Args, [], [{string, L2, CCode}]}]} = H | T],
                    Functions, Forms) ->
  case take_function(F, A, FunctionNames) of
    false ->
      funtions_from_forms(FunctionNames, T, Functions, [H | Forms]);
    {{F, A, DirtyMode}, NewFunctionNames} ->
      Stub = get_function_stub(L1, L2, F, A, Args),
      NewFunctions = [{F, A, DirtyMode, CCode} | Functions],
      funtions_from_forms(NewFunctionNames, T, NewFunctions, [Stub | Forms])
  end;
funtions_from_forms(FunctionNames, [H | T], Functions, Forms) ->
  funtions_from_forms(FunctionNames, T, Functions, [H | Forms]).

% Get the function specification from the function list
take_function(F, A, Functions) ->
  take_function(F, A, Functions, []).

take_function(_F, _A, [], _Acc) ->
  false;
take_function(F, A, [{F, A, _} = H | T], Acc) ->
  {H, lists:reverse(Acc, T)};
take_function(F, A, [H | T], Acc) ->
  take_function(F, A, T, [H | Acc]).

get_nif_id(Module) ->
  atom_to_list(Module) ++ "_nif".

% The list of compiler flags used when calling gcc
flags(UserFlags) ->
  DefaultFlags = ["-flat_namespace", "-undefined suppress", "-fpic", "-shared"],
  string:join(lists:usort(DefaultFlags ++ UserFlags), " ").

% Make sure we have an init function
ensure_init_function(Module, Forms) ->
  ensure_init_function(Module, Forms, []).

ensure_init_function(Module, [], Acc) ->
  FName = list_to_atom(atom_to_list(Module) ++ "nif_init"),
  L = lists:reverse([{function, 0, FName, 0, [{clause, 0, [], [], []}]} | Acc]),
  {P, S} = lists:split(3, L),
  {{FName, 0}, P ++ [{attribute, 0, on_load, {FName, 0}}] ++ S};
ensure_init_function(_Module, [{attribute, _, on_load, F} | _] = L, Acc) ->
  {F, lists:reverse(Acc, L)};
ensure_init_function(Module, [H | T], Acc) ->
  ensure_init_function(Module, T, [H | Acc]).

% Assuming we have an init function, add the code to load the nif
add_nif_loader(Forms, Func, NIFName) ->
  add_nif_loader(Forms, Func, NIFName, []).

add_nif_loader([{function, L, F, A, [Clause]} | T], {F, A}, NIFName, Acc) ->
  {clause, L2, Args, [], Body} = Clause,
  NewClause = {clause, L2, Args, [], [get_nif_loader(L, NIFName) | Body]},
  NewF = {function, L, F, A, [NewClause]},
  lists:reverse([NewF | Acc], T);
add_nif_loader([H | T], Func, NIFName, Acc) ->
  add_nif_loader(T, Func, NIFName, [H | Acc]).

%%==============================================================================
%% Code generators
%%==============================================================================
get_function_stub(Line1, Line2, Name, Arity, Args) ->
  Body = {call, Line2, {atom, Line2, throw}, [{atom, Line2, nif_not_loaded}]},
  {function, Line1, Name, Arity, [{clause, Line1, Args, [], [Body]}]}.

get_nif_loader(Line, NIFName) ->
  M = {atom, Line, erlang},
  F = {atom, Line, load_nif},
  A = [{string, Line, "./" ++ NIFName ++ ""}, {integer, Line, 0}],
  {match, Line, {atom, Line, ok}, {call, Line, {remote, Line, M, F}, A}}.

build_c_code(Mod, Included, Functions) ->
  {Includes, NewFunctions} = get_included(Functions, ["erl_nif.h" | Included]),
  NIFIds = [["{\"", atom_to_list(F), "\", ",
             integer_to_list(A), ", ",
             atom_to_list(F), "_nif, ",
             func_flags(DirtyMode),
             "}"] || {F, A, DirtyMode, _} <- NewFunctions],
  [string:join(Includes, "\n"), "\n\n",
   string:join([build_c_function(F) || F <- NewFunctions], "\n"), "\n",
   "static ErlNifFunc niffuncs[] = {",
   string:join(NIFIds, [",\n", lists:duplicate(32, 32)]), "};\n\n",
   "ERL_NIF_INIT(", atom_to_list(Mod), ", niffuncs, NULL, NULL, NULL, NULL)"].

build_c_function({F, _, _, Function}) ->
  Name = atom_to_list(F),
  Args = args(Function),
  Body = body(Function),
  % Get the function that transforms the return value to an erlang term
  MakeType = get_make_type(return_type(Function, Name)),
  % And apply it (via regex) to the function body
  FixedBody = re:replace(Body,
                         "return (?!enif_make)([^;]*);",
                         "return " ++ MakeType ++ "(\\1);"),
  ["static ERL_NIF_TERM ", atom_to_list(F),
   "_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])\n",
   "{\n",
   string:join([["  ", T, " ", N, $;] || {T, N} <- Args], "\n"), "\n",
   make_enif_gets(Args), "\n",
   FixedBody,
   "}\n"].

func_flags(cpu_bound) ->
  "ERL_NIF_DIRTY_JOB_CPU_BOUND";
func_flags(io_bound) ->
  "ERL_NIF_DIRTY_JOB_IO_BOUND";
func_flags(_None) ->
  $0.

%%==============================================================================
%% C code utils
%%==============================================================================
get_get_type(Type) ->
  ["enif_get_", proplists:get_value(Type, type_mapping())].

get_make_type(Type) ->
  ["enif_make_", proplists:get_value(Type, type_mapping())].

type_mapping() ->
  [{"int", "int"},
   {"ErlNifSInt64", "int64"},
   {"double", "double"},
   {"long int", "long"},
   etcetera].

args(Function) ->
  ArgsStr = string:sub_string(Function, string:chr(Function,  $() + 1,
                                        string:chr(Function, $)) - 1),
  [list_to_tuple(string:tokens(A, " ")) || A <- string:tokens(ArgsStr, ",")].

% The body of the function is assumed to be anything inside the outmost brackets
body(Function) ->
  % Get the position of the outmost bracket
  OpeningBracket = string:chr(Function, ${),
  ClosingBracket = string:rchr(Function, $}),
  % Get the spaces we have in front of the first bracket
  BeforeOpeningBracket = string:sub_string(Function, 1, OpeningBracket - 1),
  Tab = string:sub_string(Function,
                          string:rchr(BeforeOpeningBracket, $\n) + 1,
                          OpeningBracket - 1),
  % Get the body
  Body = string:sub_string(Function, OpeningBracket + 1, ClosingBracket - 1),
  % Divide the body in lines and remove all extra spaces on each line
  Lines = [Line -- Tab || Line <- string:tokens(Body, "\n")],
  % Put the newline characters back
  string:join(Lines, "\n").

return_type(Body, FName) ->
  string:strip(string:substr(Body, 1, string:str(Body, FName) - 1), both).

make_enif_gets(Args) ->
  make_enif_gets(lists:reverse(Args), 0, []).

make_enif_gets([], _, Acc) ->
  string:join(Acc, "\n");
make_enif_gets([{Type, Name} | T], I, Acc) ->
  make_enif_gets(T, I + 1, [make_enif_get(I, Type, Name) | Acc]).

make_enif_get(I, Type, Name) ->
  ["  if (!", get_get_type(Type),
   "(env, argv[", integer_to_list(I), "], &", Name, "))\n",
   "    return enif_make_badarg(env);"].

get_included(Functions, Included) ->
  lists:foldr(fun(Function, {NewIncludes, NewFunctions}) ->
                {Includes, NewFunction} = get_included(Function),
                {lists:usort(Includes ++ NewIncludes),
                 [NewFunction | NewFunctions]}
              end, {[format_include(I) || I <- Included], []}, Functions).

get_included({F, A, DirtyMode, Body}) ->
  {Includes, Rest} = lists:partition(fun([$# | _]) -> true;
                                        (_) -> false
                                     end, string:tokens(Body, "\n")),
  {Includes, {F, A, DirtyMode, string:join(Rest, "\n")}}.

format_include([$< | _] = Include) -> "#include " ++ Include;
format_include(Include)            -> "#include \"" ++ Include ++ "\"".