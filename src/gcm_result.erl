%%%==============================================================================
%%% @doc High level implementation of Google Cloud Messaging results
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
-module(gcm_result).

-export([ new/0,
	  parse/1,
	  message_id/1,
	  canonical_registration_id/1,
	  error_code_name/1,
	  set_canonical_registration_id/2,
	  set_error_code/2,
	  set_message_id/2
	]).

-opaque message_id() :: string() | undefined.
-opaque crid()       :: string() | undefined.
-opaque error_code() :: string() | undefined.

-record(result,
	{
	  message_id   :: message_id(),
	  canonical_id :: crid(),
	  error_code   :: error_code()
	}).

-opaque result() :: record(result).

-spec new() -> result().
new() ->
  #result{}.

-spec parse(term()) -> result().
parse({struct, Body}) ->
  #result{ message_id = proplists:get_value(<<"message_id">>, Body),
           canonical_id = proplists:get_value(<<"registration_id">>, Body),
           error_code = proplists:get_value(<<"error">>, Body) }.

-spec message_id(result()) -> message_id().
message_id(#result{message_id = MsgId}) ->
  MsgId.

-spec canonical_registration_id(result()) -> crid().
canonical_registration_id(#result{canonical_id = CrId}) ->
  CrId.

-spec error_code_name(result()) -> error_code().
error_code_name(#result{error_code = Err}) ->
  Err.

-spec set_message_id(result(), message_id()) -> result().
set_message_id(#result{message_id = undefined} = Result, MessageId) ->
  Result#result{message_id = MessageId};
set_message_id(_, _) ->
  throw(attempt_to_modify_message_id).

-spec set_canonical_registration_id(result(), crid()) -> result().
set_canonical_registration_id(#result{canonical_id = undefined} = Result, CrId) ->
  Result#result{canonical_id = CrId};
set_canonical_registration_id(_, _) ->
  throw(attempt_to_modify_crid).

-spec set_error_code(result(), error_code()) -> result().
set_error_code(#result{error_code = undefined} = Result, ErrorCode) ->
  Result#result{error_code = ErrorCode};
set_error_code(_, _) ->
  throw(attempt_to_modify_error_code).
