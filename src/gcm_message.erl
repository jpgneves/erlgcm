%%%==============================================================================
%%% @doc High level implementation of Google Cloud Messaging messages
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

-module(gcm_message).

%% Exports
-export([ new/1,
	  collapse_key/1,
	  is_delay_while_idle/1,
	  time_to_live/1,
	  data/1,
	  serialize/3
	]).

%% Includes
-include("erlgcm.hrl").

%% Types and definitions
-type collapse_key()     :: string() | undefined.
-type data()             :: [{atom(), integer() | string()}].
-type delay_while_idle() :: boolean().
-type ttl()              :: integer() | undefined.

-record(gcm_message,
	{
	  collapse_key     :: collapse_key(),
	  delay_while_idle :: delay_while_idle(),
	  time_to_live     :: ttl(),
	  data             :: data()
	}).

-type message() :: record(gcm_message).

-export_type([ data/0,
               delay_while_idle/0,
               collapse_key/0,
               message/0,
               ttl/0
             ]).

%% @doc Creates a new message with the given properties.
-spec new([{atom(), any()}]) -> message().
new(Props) ->
  #gcm_message{ collapse_key = get_default(collapse_key, Props, undefined),
		delay_while_idle = get_default(delay_while_idle, Props, false),
		time_to_live = get_default(time_to_live, Props, undefined),
		data = get_default(data, Props, [])
	      }.


-spec collapse_key(message()) -> collapse_key().
collapse_key(#gcm_message{collapse_key = Key}) ->
  Key.

-spec is_delay_while_idle(message()) -> delay_while_idle().
is_delay_while_idle(#gcm_message{delay_while_idle = Delay}) ->
  Delay.

-spec time_to_live(message()) -> ttl().
time_to_live(#gcm_message{time_to_live = TTL}) ->
  TTL.

-spec data(message()) -> data().
data(#gcm_message{data = Data}) ->
  Data.

-spec serialize(Format :: atom(), message(), [gcm_sender:registration_id()]) -> any().
serialize(json, Message, RegistrationIds) ->
  build_json(Message, RegistrationIds);
serialize(_Format, _Message, _RegistrationIds) ->
  throw(unsupported_format).

build_json(#gcm_message{} = Message, RegistrationIds) ->
  SetParamFun = fun({Param, Msg}, Acc) -> maybe_set(Param, Msg, Acc) end,

  Body0 = [make_param({?PARAM_REGISTRATION_IDS, {array, [list_to_binary(Id) || Id <- RegistrationIds]}})],

  Body = lists:foldl(SetParamFun,
                     Body0, [{delay_while_idle, Message},
                             {collapse_key, Message},
                             {time_to_live, Message},
                             {data, Message}]
                    ),
  mochijson2:encode({struct, Body}).


-spec maybe_set(atom(), message(), [{atom(), any()}]) -> [{atom(), any()}].
maybe_set(?PARAM_DELAY_WHILE_IDLE, #gcm_message{delay_while_idle = Delay}, Body) ->
  case Delay of
    undefined -> Body;
    true      -> append_param(make_param({?PARAM_DELAY_WHILE_IDLE, "1"}), Body);
    false     -> append_param(make_param({?PARAM_DELAY_WHILE_IDLE, "0"}), Body)
  end;
maybe_set(?PARAM_COLLAPSE_KEY, #gcm_message{collapse_key = CKey}, Body) ->
  case CKey of
    undefined -> Body;
    _         -> append_param(make_param({?PARAM_COLLAPSE_KEY, CKey}), Body)
  end;
maybe_set(?PARAM_TIME_TO_LIVE, #gcm_message{time_to_live = TTL}, Body) ->
  case TTL of
    undefined -> Body;
    _         -> append_param(make_param({?PARAM_TIME_TO_LIVE, integer_to_list(TTL)}), Body)
  end;
maybe_set(?PARAM_PAYLOAD, #gcm_message{data = Data}, Body) ->
  case Data of
    [] -> Body;
    _  -> append_param(make_param({?PARAM_PAYLOAD, {struct, encode(Data)}}), Body)
  end.

encode(Data) ->
  lists:map(fun({K,V}) when is_list(V) ->
                {K, list_to_binary(V)};
               (KV) -> KV
            end, Data).

make_param(KV) ->
  KV.

append_param(Param, Body) ->
  Body ++ [Param].

get_default(Key, KVs, Default) ->
  case lists:keyfind(Key, 1, KVs) of
    false        -> Default;
    {Key, Value} -> Value
  end.
