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

-module(stripe_webhooks).

-export([validate_request/2, parse_signature/1,
         parse_event/1,
         format_signature_error_reason/1]).

-export_type([signature/0, signature_result/1, signature_error_reason/0]).

-type signature() ::
        #{t := integer(),
          v1 := binary()}.

-type signature_result(Result) ::
        {ok, Result} | {error, signature_error_reason()}.

-type signature_error_reason() ::
        invalid_format
      | {missing_field, atom()}
      | invalid_timestamp
      | invalid_v1_hmac.

-spec validate_request(mhttp:request(), binary()) -> stripe:result().
validate_request(Request, Secret) ->
  Header = mhttp_request:header(Request),
  case mhttp_header:find(Header, <<"Stripe-Signature">>) of
    {ok, Value} ->
      case parse_signature(Value) of
        {ok, Signature} ->
          validate_request(Request, Signature, Secret);
        {error, Reason} ->
          {error, {invalid_webhook_signature, Reason}}
      end;
    error ->
      {error, missing_webhook_signature}
  end.

-spec validate_request(mhttp:request(), signature(), binary()) ->
        stripe:result().
validate_request(Request, #{t := Timestamp, v1 := ExpectedHMAC}, Secret) ->
  Body = mhttp_request:body(Request),
  Data = [integer_to_binary(Timestamp), $., Body],
  HMAC0 = crypto:mac(hmac, sha256, Secret, Data),
  HMAC = string:lowercase(binary:encode_hex(HMAC0)),
  Now = os:system_time(second),
  TimestampDelta = abs(Now - Timestamp),
  %% XXX Wait for OTP 25 to be able to use crypto:hash_equals/2.
  if
    HMAC =/= ExpectedHMAC ->
      {error, invalid_webhook_signature};
    TimestampDelta > 60 ->
      {error, invalid_webhook_timestamp};
    true ->
      ok
  end.

-spec parse_signature(binary()) -> signature_result(signature()).
parse_signature(String) ->
  Fields = binary:split(String, <<",">>, [global]),
  parse_signature_fields(Fields, #{}).

-spec parse_signature_fields([binary()], map()) ->
        signature_result(signature()).
parse_signature_fields([], Signature) ->
  check_signature(Signature, [t, v1]);
parse_signature_fields([Field | Fields], Signature) ->
  case binary:split(Field, <<"=">>) of
    [<<"t">>, Value] ->
      try
        Timestamp = binary_to_integer(Value),
        parse_signature_fields(Fields, Signature#{t => Timestamp})
      catch
        error:_ ->
          {error, invalid_timestamp}
      end;
    [<<"v1">>, Value] when byte_size(Value) =:= 64 ->
      parse_signature_fields(Fields, Signature#{v1 => Value});
    [<<"v1">>, _] ->
      {error, invalid_v1_hmac};
    [_, _] ->
      %% Ignore unknown fields; we do not want to reject events if new fields
      %% are introduced.
      parse_signature_fields(Fields, Signature);
    [_] ->
      {error, invalid_format}
  end.

-spec check_signature(map(), [atom()]) -> signature_result(signature()).
check_signature(Signature, []) ->
  {ok, Signature};
check_signature(Signature, [Field | Fields]) ->
  case maps:is_key(Field, Signature) of
    true ->
      check_signature(Signature, Fields);
    false ->
      {error, {missing_field, Field}}
  end.

-spec parse_event(mhttp:request()) -> stripe:result(stripe_schemas:event()).
parse_event(Request) ->
  Body = mhttp_request:body(Request),
  case json:parse(Body) of
    {ok, Value} ->
      Options =
        #{disable_verification => true,
          unknown_member_handling => keep,
          null_member_handling => remove},
      case jsv:validate(Value, {ref, stripe, event}, Options) of
        {ok, Event} ->
          {ok, Event};
        {error, Errors} ->
          {error, {invalid_event, {jsv, Errors}}}
      end;
    {error, Error} ->
      {error, {invalid_event, {json, Error}}}
  end.

-spec format_signature_error_reason(signature_error_reason()) ->
        unicode:chardata().
format_signature_error_reason(invalid_format) ->
  <<"invalid format">>;
format_signature_error_reason({missing_field, Field}) ->
  <<"missing field ", (atom_to_binary(Field))/binary>>;
format_signature_error_reason(invalid_timestamp) ->
  <<"invalid timestamp">>;
format_signature_error_reason(invalid_v1_hmac) ->
  <<"invalid v1 hmac">>;
format_signature_error_reason(Reason) ->
  io_lib:format("~0tp", [Reason]).
