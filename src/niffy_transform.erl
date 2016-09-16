-module(niffy_transform).
-export([parse_transform/2]).

%%==============================================================================
%% API
%%==============================================================================
parse_transform(Forms, Options) ->
  case config(Forms) of
    undefined ->
      Forms;
    Config ->
      % Get the functions to niffify
      Functions = maps:get(functions, Config, []),
      % Get the configuration
      NiffyOptions = options(maps:get(options, Config, []),
                             proplists:get_value(niffy_options, Options, [])),

      Module = module(Forms),
      NIFId = nif_id(Module),
      {ok, AppDir} = file:get_cwd(),
      AppName = app_name(Options, AppDir),
      CDir = proplists:get_value(c_dir, NiffyOptions),

      % Parse the Forms and get the C code
      {CFunctions, Forms2} = functions_from_forms(Functions, Forms),

      % Build the C file
      CFileName = filename:join([AppDir, CDir, NIFId ++ ".c"]),
      CCode = build_c_code(Module, CFunctions, NiffyOptions),
      ok = filelib:ensure_dir(CFileName),
      ok = file:write_file(CFileName, CCode),

      % Make sure we have an init function where we can add the nif loading code
      {InitFunc, Forms3} = ensure_init_function(Module, Forms2),

      % Compile the C file to the priv folder
      SOName = filename:join([AppDir, "priv", NIFId ++ ".so"]),
      ok = filelib:ensure_dir(SOName),
      Flags = flags(NiffyOptions),
      Compiler = proplists:get_value(compiler, NiffyOptions),
      Command = [Compiler, " -o ", SOName, " ", Flags, " ", CFileName],
      _ = case os:cmd(Command) of
            [] ->
              ignore;
            Error ->
              io:format("Compiler Error:~n$~s~n> ~s", [Command, Error])
          end,
      % Add the call to load the nif
      add_nif_loader(Forms3, InitFunc, {AppName, NIFId})
  end.

%%==============================================================================
%% Utils
%%==============================================================================
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

options(ModuleOpts, GlobalOpts) ->
  [get_option(Type, Name, ModuleOpts, GlobalOpts, Default) ||
   {Name, Type, Default} <- options_spec()].

options_spec() ->
  [{flags, merge, ["-flat_namespace", "-undefined suppress", "-fpic",
                   "-shared"]},
   {compiler, module_first, "gcc"},
   {includes, module_only, []},
   {strict_types, module_first, true},
   {c_dir, global_only, "_generated/c_src"}].

get_option(merge, K, ModuleOpts, GlobalOpts, Def) ->
  {K, proplists:get_value(K, ModuleOpts, []) ++
      proplists:get_value(K, GlobalOpts, []) ++
      Def};
get_option(module_only, K, ModuleOpts, _GlobalOpts, Def) ->
  {K, proplists:get_value(K, ModuleOpts, Def)};
get_option(global_only, K, _ModuleOpts, GlobalOpts, Def) ->
  {K, proplists:get_value(K, GlobalOpts, Def)};
get_option(module_first, K, ModuleOpts, GlobalOpts, Def) ->
  {K,
   proplists:get_value(K, ModuleOpts, proplists:get_value(K, GlobalOpts, Def))}.

% Retrieve the module name
module([{attribute, _, module, Module} | _]) ->
  Module;
module([_ | T]) ->
  module(T).

% Get the C code of each of the functions on the niffy config
functions_from_forms(Functions, Forms) ->
  functions_from_forms(Functions, Forms, [], []).

functions_from_forms([], _, Functions, Forms) ->
  {Functions, lists:reverse(Forms)};
functions_from_forms(_, [], Functions, Forms) ->
  {Functions, lists:reverse(Forms)};
functions_from_forms(FunctionNames,
                    [{function, L1, F, A,
                      [{clause, _, Args, [], [{string, L2, CCode}]}]} = H | T],
                    Functions, Forms) ->
  case take_function(F, A, FunctionNames) of
    false ->
      functions_from_forms(FunctionNames, T, Functions, [H | Forms]);
    {{F, A, DirtyMode}, NewFunctionNames} ->
      Stub = get_function_stub(L1, L2, F, A, Args),
      NewFunctions = [{F, A, DirtyMode, CCode} | Functions],
      functions_from_forms(NewFunctionNames, T, NewFunctions, [Stub | Forms])
  end;
functions_from_forms(FunctionNames, [H | T], Functions, Forms) ->
  functions_from_forms(FunctionNames, T, Functions, [H | Forms]).

% Get the function specification from the function list
take_function(F, A, Functions) ->
  take_function(F, A, Functions, []).

take_function(_F, _A, [], _Acc) ->
  false;
take_function(F, A, [{F, A, _} = H | T], Acc) ->
  {H, lists:reverse(Acc, T)};
take_function(F, A, [H | T], Acc) ->
  take_function(F, A, T, [H | Acc]).

nif_id(Module) ->
  atom_to_list(Module) ++ "_nif".

% Get the app name, this is needed at runtime to retrieve the priv_dir
app_name(Options, AppDir) ->
  case proplists:get_value(application, Options) of
    undefined ->
      % If the user didn't specify an application name, we have to get one
      AppSrc = filename:join([AppDir, "src", "*.app.src"]),
      {ok, [{_, AppName, _}]} = file:consult(hd(filelib:wildcard(AppSrc))),
      AppName;
    Value ->
      Value
  end.

% The list of compiler flags used when calling gcc
flags(NiffyOptions) ->
  string:join(lists:usort(proplists:get_value(flags, NiffyOptions)), " ").

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
add_nif_loader(Forms, Func, NIF) ->
  add_nif_loader(Forms, Func, NIF, []).

add_nif_loader([{function, L, F, A, [Clause]} | T], {F, A}, NIF, Acc) ->
  {clause, L2, Args, [], Body} = Clause,
  NewClause = {clause, L2, Args, [], [make_nif_loader(L, NIF) | Body]},
  NewF = {function, L, F, A, [NewClause]},
  lists:reverse([NewF | Acc], T);
add_nif_loader([H | T], Func, NIF, Acc) ->
  add_nif_loader(T, Func, NIF, [H | Acc]).

%%==============================================================================
%% Code generators
%%==============================================================================
get_function_stub(Line1, Line2, Name, Arity, Args) ->
  Body = {call, Line2, {atom, Line2, throw}, [{atom, Line2, nif_not_loaded}]},
  {function, Line1, Name, Arity, [{clause, Line1, Args, [], [Body]}]}.

make_nif_loader(L, {AppName, NIF}) ->
  % The code this is generating is basically:
  % erlang:load_nif(niffy:nif_filename(AppName, NIF), 0)
  Args = [{call, L,
           {remote, L, {atom, L, niffy}, {atom, L, nif_filename}},
           [{atom, L, AppName}, {string, L, NIF}]},
          {integer, L, 0}],
  {match, L, {atom, L, ok},
             {call, L, {remote, L, {atom, L, erlang},
                                   {atom, L, load_nif}}, Args}}.

build_c_code(Mod, Functions, Options) ->
  Includes = proplists:get_value(includes, Options),
  {NewIncludes, NewFunctions} = includes(Functions, ["erl_nif.h" | Includes]),
  NIFIds = [["{\"", atom_to_list(F), "\", ",
             integer_to_list(A), ", ",
             atom_to_list(F), "_nif, ",
             func_flags(DirtyMode),
             "}"] || {F, A, DirtyMode, _} <- NewFunctions],
  [string:join(NewIncludes, "\n"), "\n\n",
   string:join([build_c_function(F, Options) || F <- NewFunctions], "\n"), "\n",
   "static ErlNifFunc niffuncs[] = {",
   string:join(NIFIds, [",\n", lists:duplicate(32, 32)]), "};\n\n",
   "ERL_NIF_INIT(", atom_to_list(Mod), ", niffuncs, NULL, NULL, NULL, NULL)\n"].

build_c_function({F, _, _, Function}, _Options) ->
  Name = atom_to_list(F),
  Args = args(Function),
  Body = body(Function),
  % Get the function that transforms the return value to an erlang term
  MakeType = build_make_type(return_type(Function, Name)),
  % And apply it (via regex) to all returns on the function body
  FixedBody = re:replace(Body,
                         "return\s+(?!enif_make)([^;\n]*);",
                         "return " ++ MakeType ++ "(env, \\1);",
                         [global]),
  ["static ERL_NIF_TERM ", atom_to_list(F),
   "_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])\n",
   "{\n",
   string:join([["  ", T, " ", N, $;] || {T, N} <- Args], "\n"), "\n",
   build_enif_gets(Args), "\n",
   FixedBody, "}\n"].

func_flags(cpu_bound) ->
  "ERL_NIF_DIRTY_JOB_CPU_BOUND";
func_flags(io_bound) ->
  "ERL_NIF_DIRTY_JOB_IO_BOUND";
func_flags(_None) ->
  $0.

%%==============================================================================
%% C code utils
%%==============================================================================
build_make_type(Type) ->
  ["enif_make_", proplists:get_value(Type, type_mapping())].

type_mapping() ->
  [{"int", "int"},
   {"ErlNifSInt64", "int64"},
   {"double", "double"},
   {"long int", "long"}].

args(Function) ->
  ArgsStr = string:sub_string(Function, string:chr(Function, $() + 1,
                                        string:chr(Function, $)) - 1),
  [list_to_tuple(string:tokens(A, " ")) || A <- string:tokens(ArgsStr, ",")].

% The body of the function is assumed to be anything within the outmost brackets
body(Function) ->
  % Get the position of the outmost brackets
  OpeningBracket = string:chr(Function,  ${),
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

build_enif_gets(Args) ->
  build_enif_gets(lists:reverse(Args), 0, []).

build_enif_gets([], _, Acc) ->
  string:join(Acc, "\n");
build_enif_gets([{Type, Name} | T], I, Acc) ->
  build_enif_gets(T, I + 1, [build_enif_get(I, Type, Name) | Acc]).

build_enif_get(I, Type, Name) ->
  ["  if (!", ["enif_get_", proplists:get_value(Type, type_mapping())],
   "(env, argv[", integer_to_list(I), "], &", Name, "))\n",
   "    return enif_make_badarg(env);"].

includes(Functions, Included) ->
  lists:foldr(fun(Function, {NewIncludes, NewFunctions}) ->
                {Includes, NewFunction} = includes(Function),
                {lists:usort(Includes ++ NewIncludes),
                 [NewFunction | NewFunctions]}
              end, {[format_include(I) || I <- Included], []}, Functions).

includes({F, A, DirtyMode, Body}) ->
  {Includes, Rest} = lists:partition(fun([$# | _]) -> true;
                                        (_) -> false
                                     end, string:tokens(Body, "\n")),
  {Includes, {F, A, DirtyMode, string:join(Rest, "\n")}}.

format_include([$< | _] = File) -> "#include " ++ File;
format_include(File)            -> "#include \"" ++ File ++ "\"".