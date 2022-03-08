%% Copyright (c) 2022 Exograd SAS.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(stripe).

-export([format_error_reason/1]).

-export_type([client_options/0, result/0, result/1, error_reason/0]).

-type client_options() ::
        #{mhttp_pool => mhttp:pool_id(),
          api_key => binary()}.

-type result() :: ok | {error, error_reason()}.
-type result(Result) :: {ok, Result} | {error, error_reason()}.

-type error_reason() ::
        missing_webhook_signature
      | {invalid_webhook_signature, stripe_webhooks:signature_error_reason()}
      | invalid_webhook_signature
      | invalid_webhook_timestamp
      | {invalid_event, {json, json:error()}}
      | {invalid_event, {jsv, [jsv:value_error()]}}
      | {missing_event_object_name, binary()}
      | {unknown_event_object_name, binary()}
      | {invalid_event_object, {jsv, [jsv:value_error()]}}
      | {client_error, stripe_client:error()}
      | {api_error, stripe_model:api_errors()}.

-spec format_error_reason(error_reason()) -> unicode:chardata().
format_error_reason(missing_webhook_signature) ->
  "missing webhook signature";
format_error_reason({invalid_webhook_signature, Reason}) ->
  io_lib:format("invalid webhook signature: ~ts",
                [stripe_webhooks:format_signature_error_reason(Reason)]);
format_error_reason(invalid_webhook_signature) ->
  "invalid webhook signature";
format_error_reason(invalid_webhook_timestamp) ->
  "invalid webhook timestamp";
format_error_reason({invalid_event, {json, Error}}) ->
  io_lib:format("invalid event: ~ts", [json:format_error(Error)]);
format_error_reason({invalid_event, {jsv, Errors}}) ->
  io_lib:format("invalid event:~n~ts", [jsv:format_value_errors(Errors)]);
format_error_reason(missing_event_object_name) ->
  "missing event object name";
format_error_reason({unknown_event_object_name, Name}) ->
  io_lib:format("unknown event object name '~ts'", [Name]);
format_error_reason({invalid_event_object, {jsv, Errors}}) ->
  io_lib:format("invalid event object:~n~ts",
                [jsv:format_value_errors(Errors)]);
format_error_reason({client_error, #{reason := Reason}}) ->
  %% TODO generate stripe_client:format_error_reason/1
  io_lib:format("client error: ~tp", [Reason]);
format_error_reason({api_error, Error}) ->
  case Error of
    #{code := Code, message := Message} ->
      io_lib:format("api error ~ts: ~ts", [Code, Message]);
    #{message := Message} ->
      io_lib:format("api error: ~ts", [Message]);
    #{type := Type} ->
      io_lib:format("api error: ~tp", [Type])
  end;
format_error_reason(Reason) ->
  io_lib:format("~0tp", [Reason]).
