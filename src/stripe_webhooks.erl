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

-export([parse_signature/1]).

-export_type([signature/0, result/1, error_reason/0]).

-type signature() ::
        #{t := integer(),
          v1 := binary()}.

-type result(Result) :: {ok, Result} | {error, error_reason()}.

-type error_reason() ::
        invalid_format
      | {missing_field, atom()}
      | invalid_timestamp
      | invalid_v1_hmac.


-spec parse_signature(binary()) -> result(signature()).
parse_signature(String) ->
  Fields = binary:split(String, <<",">>, [global]),
  parse_signature_fields(Fields, #{}).

-spec parse_signature_fields([binary()], map()) -> result(signature()).
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

-spec check_signature(map(), [atom()]) -> result(signature()).
check_signature(Signature, []) ->
  {ok, Signature};
check_signature(Signature, [Field | Fields]) ->
  case maps:is_key(Field, Signature) of
    true ->
      check_signature(Signature, Fields);
    false ->
      {error, {missing_field, Field}}
  end.
