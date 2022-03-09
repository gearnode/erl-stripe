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

-module(stripe_checkout_sessions).

-export([create/2, expire/2, get/2]).

-export_type([create_data/0]).

-type create_data() ::
        stripe_client:post_checkout_sessions_request_body().

-spec create(create_data(), stripe:client_options()) ->
        stripe:result(stripe_model:checkout_session()).
create(Data, ClientOptions) ->
  ReqOptions = #{body => {<<"application/x-www-form-urlencoded">>, Data}},
  ClientOptions2 = stripe_utils:client_options(ClientOptions),
  case stripe_client:post_checkout_sessions(ReqOptions, ClientOptions2) of
    {ok, Session, #{status := S}} when S >= 200, S < 300 ->
      {ok, Session};
    {ok, #{error := Error}, _} ->
      {error, {api_error, Error}};
    {error, Error} ->
      {error, {client_error, Error}}
  end.

-spec expire(binary(), stripe:client_options()) -> stripe:result().
expire(Id, ClientOptions) ->
  ReqOptions = #{path => #{session => Id}},
  ClientOptions2 = stripe_utils:client_options(ClientOptions),
  case
    stripe_client:post_checkout_sessions_session_expire(ReqOptions,
                                                        ClientOptions2)
  of
    {ok, _, #{status := S}} when S >= 200, S < 300 ->
      ok;
    {ok, #{error := Error}, _} ->
      {error, {api_error, Error}};
    {error, Error} ->
      {error, {client_error, Error}}
  end.

-spec get(binary(), stripe:client_options()) -> stripe:result().
get(Id, ClientOptions) ->
  ReqOptions = #{path => #{session => Id}},
  ClientOptions2 = stripe_utils:client_options(ClientOptions),
  case
    stripe_client:post_checkout_sessions_session_expire(ReqOptions,
                                                        ClientOptions2)
  of
    {ok, Session, #{status := S}} when S >= 200, S < 300 ->
      {ok, Session};
    {ok, #{error := Error}, _} ->
      {error, {api_error, Error}};
    {error, Error} ->
      {error, {client_error, Error}}
  end.
