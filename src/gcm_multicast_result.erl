%%%==============================================================================
%%% @doc High level implementation of Google Cloud Messaging responses
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
-module(gcm_multicast_result).

-export([ parse/1 ]).

-record(gcm_multicast_result, { multicast_id = 0        :: integer(),
                                success_count = 0       :: integer(),
                                failure_count = 0       :: integer(),
                                canonical_id_count = 0  :: integer(),
                                results = []            :: [gcm_result:result()]
                              }).

parse(Message) ->
  {struct, Body} = mochijson2:decode(Message),
  #gcm_multicast_result{ multicast_id = get_default(<<"multicast_id">>, Body, 0),
                         success_count = get_default(<<"success">>, Body, 0),
                         failure_count = get_default(<<"failure">>, Body, 0),
                         canonical_id_count = get_default(<<"canonical_ids">>, Body, 0),
                         results = [gcm_result:parse(R) ||
                                     R <- get_default(<<"results">>, Body, [])]
                       }.

get_default(Key, KVs, Default) ->
  case lists:keyfind(Key, 1, KVs) of
    false -> Default;
    {Key, Value} -> Value
  end.
