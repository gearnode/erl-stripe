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

-module(stripe_subscriptions).

-export([get/2]).

-spec get(binary(), stripe:client_options()) ->
        stripe:result(stripe_model:subscription()).
get(Id, ClientOptions) ->
  ReqOptions = #{path => #{subscription_exposed_id => Id}},
  ClientOptions2 = stripe_utils:client_options(ClientOptions),
  case
    stripe_client:get_subscriptions_subscription_exposed_id(ReqOptions,
                                                            ClientOptions2)
  of
    {ok, Subscription, #{status := S}} when S >= 200, S < 300 ->
      {ok, Subscription};
    {ok, #{error := Error}, _} ->
      {error, {api_error, Error}};
    {error, Error} ->
      {error, {client_error, Error}}
  end.
