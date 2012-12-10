-module(erlgcm_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  ensure_deps_started(),
  erlgcm_sup:start_link().

stop(_State) ->
  ok.

ensure_deps_started() ->
  Deps = [inets, crypto, public_key, ssl],
  ok = lists:foldl(fun (App, _) -> ensure_started(App) end, not_started, Deps).

ensure_started(App) ->
  case application:start(App) of
    ok -> ok;
    {error, {already_started, App}} -> ok
  end.
