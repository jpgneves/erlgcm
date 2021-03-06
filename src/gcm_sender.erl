%%%==============================================================================
%%% @doc High level implementation of a Google Cloud Messaging message sender
%%%
%%% Copyright 2012 Joao Neves
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @copyright 2012 Joao Neves <sevenjp@gmail.com>
%%%
%%% @end
%%%==============================================================================
-module(gcm_sender).

-export([ send/2,
          send/3,
          update_status/3
	]).

-include("erlgcm.hrl").
-include("utils.hrl").

-define(API_KEY, application:get_env(erlgcm, api_key)).

-define(BACKOFF_INITIAL_DELAY, 1000).
-define(MAX_BACKOFF_DELAY, 1024000).

-type registration_id() :: string().

-export_type([ registration_id/0 ]).

%%==============================================================================
%% API functions
%%==============================================================================

-spec send(gcm_message:message(), [registration_id()]) -> {ok, gcm_result:result()} | {error, no_more_retries}.
send(Message, RegistrationIds) ->
  send(Message, RegistrationIds, 1).

-spec send(gcm_message:message(), [registration_id()], non_neg_integer()) ->
              {ok, gcm_result:result()} | {error, Error :: atom()}.
send(Message, RegistrationIds, Attempts) ->
  send(Message, RegistrationIds, Attempts, ?BACKOFF_INITIAL_DELAY).

-spec send(gcm_message:message(), [registration_id()], non_neg_integer(), integer()) ->
              {ok, gcm_result:result()} | {error, Error :: atom()}.
send(_Message, _RegistrationIds, 0, _Backoff) ->
  {error, no_more_retries};
send(Message, RegistrationIds, Attempts, Backoff) ->
  try do_send(Message, RegistrationIds) of
      Result -> Result
  catch
    throw:_Error ->
      SleepTime = round(Backoff / (2 + random:uniform(Backoff))),
      timer:sleep(SleepTime),
      NewBackoff = ?ternary((2 * Backoff) < ?MAX_BACKOFF_DELAY,
                            Backoff * 2, Backoff),
      send(Message, RegistrationIds, Attempts - 1, NewBackoff)
  end.

-spec update_status(_, _, _) -> no_return().
update_status(_UnsentRegIds, _AllResults, _MulticastResult) ->
  throw(not_implemented).

%%==============================================================================
%% Internal functions
%%==============================================================================

do_send(Message, RegistrationIds) ->
  Body = lists:flatten(io_lib:format("~s", [gcm_message:serialize(json, Message, RegistrationIds)])),

  {ok, Key} = ?API_KEY,

  case httpc:request(post, {?GCM_SEND_ENDPOINT,
                            [{"Authorization", "key=" ++ Key}
                            ],
                            ?CONTENT_TYPE_JSON,
                            Body}, [], []) of
    {ok, {{_, 200, _}, _, ReplyBody}}  -> handle_reply(ReplyBody);
    {ok, {{_, 503, _}, _, _}}          -> throw(?ERROR_UNAVAILABLE);
    {ok, {{_, Code, _}, _, ReplyBody}} -> throw({http_error, {Code, ReplyBody}});
    {error, _} = E                     -> throw(E)
  end.

handle_reply("") ->
  throw(empty_response);
handle_reply(Reply) ->
  {ok, gcm_multicast_result:parse(Reply)}.
