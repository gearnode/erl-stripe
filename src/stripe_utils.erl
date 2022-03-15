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

-module(stripe_utils).

-export([client_options/1]).

-spec client_options(stripe:client_options()) -> stripe_client:options().
client_options(Options) ->
  F = fun
        (mhttp_pool, Id, ClientOptions = #{request_options := ReqOptions}) ->
          ClientOptions#{request_options => ReqOptions#{pool => Id}};
        (api_key, Key, ClientOptions = #{header := Header}) ->
          %% "Authentication to the API is performed via HTTP Basic Auth.
          %% Provide your API key as the basic auth username value. You do not
          %% need to provide a password."
          Header2 = mhttp_header:add_basic_authorization(Header, Key, <<>>),
          ClientOptions#{header => Header2};
        (idempotency_key, Key, ClientOptions = #{header := Header}) ->
          Header2 = mhttp_header:add(Header, <<"Idempotency-Key">>, Key),
          ClientOptions#{header => Header2}
      end,
  ClientOptions0 =
    #{request_options => #{},
      header => mhttp_header:new()},
  maps:fold(F, ClientOptions0, Options).
