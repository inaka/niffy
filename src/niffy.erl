-module(niffy).
-export([nif_filename/2]).

%%==============================================================================
%% API
%%==============================================================================
nif_filename(AppName, NIF) ->
  Dir = case code:priv_dir(AppName) of
          {error, bad_name} ->
            case filelib:is_dir(filename:join("..", "priv")) of
              true ->
                filename:join("..", "priv");
              _ ->
                "priv"
            end;
          PrivDir ->
            PrivDir
        end,
  filename:join(Dir, NIF).