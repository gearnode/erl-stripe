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

-module(stripe_customers).

-export([create/2, update/3, delete/2]).

-export_type([create_data/0, update_data/0]).

-type create_data() ::
        stripe_client:post_customers_request_body().

-type update_data() ::
        stripe_client:post_customers_customer_request_body().

-spec create(create_data(), stripe:client_options()) ->
        stripe:result(stripe_model:customer()).
create(Data, ClientOptions) ->
  ReqOptions = #{body => {<<"application/x-www-form-urlencoded">>, Data}},
  ClientOptions2 = stripe_utils:client_options(ClientOptions),
  case stripe_client:post_customers(ReqOptions, ClientOptions2) of
    {ok, Customer, #{status := S}} when
        S >= 200, S < 300 ->
      {ok, Customer};
    {ok, #{error := Error}, _} ->
      {error, {api_error, Error}};
    {error, Error} ->
      {error, {client_error, Error}}
  end.

-spec update(binary(), update_data(), stripe:client_options()) ->
        stripe:result(stripe_model:customer()).
update(Id, Data, ClientOptions) ->
  ReqOptions = #{path => #{customer => Id},
                 body => {<<"application/x-www-form-urlencoded">>, Data}},
  ClientOptions2 = stripe_utils:client_options(ClientOptions),
  case stripe_client:post_customers_customer(ReqOptions, ClientOptions2) of
    {ok, Customer, #{status := S}} when
        S >= 200, S < 300 ->
      {ok, Customer};
    {ok, #{error := Error}, _} ->
      {error, {api_error, Error}};
    {error, Error} ->
      {error, {client_error, Error}}
  end.

-spec delete(binary(), stripe:client_options()) -> stripe:result().
delete(Id, ClientOptions) ->
  ReqOptions = #{path => #{customer => Id}},
  ClientOptions2 = stripe_utils:client_options(ClientOptions),
  case stripe_client:delete_customers_customer(ReqOptions, ClientOptions2) of
    {ok, _, #{status := S}} when
        S >= 200, S < 300 ->
      ok;
    {ok, #{error := Error}, _} ->
      {error, {api_error, Error}};
    {error, Error} ->
      {error, {client_error, Error}}
  end.
