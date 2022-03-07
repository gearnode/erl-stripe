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

-module(stripe_events).

-export([parse_event/1, event_object_name/1, decode_event_object/1]).

-spec parse_event(binary()) -> stripe:result(stripe_model:event()).
parse_event(Data) ->
  case json:parse(Data) of
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

-spec event_object_name(stripe_model:event()) -> {ok, binary()} | error.
event_object_name(#{data := #{object := Object}}) ->
  maps:find(<<"object">>, Object).

-spec decode_event_object(stripe_model:event()) -> stripe:result(map()).
decode_event_object(Event = #{data := #{object := Object}}) ->
  %% See https://stripe.com/docs/api/events/types for the possible object
  %% types.
  case event_object_name(Event) of
    {ok, <<"account">>} ->
      decode_event_object(Object, {ref, stripe, account});
    {ok, <<"application">>} ->
      decode_event_object(Object, {ref, stripe, application});
    {ok, <<"application_fee">>} ->
      decode_event_object(Object, {ref, stripe, application_fee});
    {ok, <<"balance">>} ->
      decode_event_object(Object, {ref, stripe, balance});
    {ok, <<"bank_account">>} ->
      decode_event_object(Object, {ref, stripe, bank_account});
    {ok, <<"billing_portal.configuration">>} ->
      decode_event_object(Object, {ref, stripe, billing_portal_configuration});
    {ok, <<"capability">>} ->
      decode_event_object(Object, {ref, stripe, capability});
    {ok, <<"card">>} ->
      decode_event_object(Object, {ref, stripe, card});
    {ok, <<"charge">>} ->
      decode_event_object(Object, {ref, stripe, charge});
    {ok, <<"checkout_session">>} ->
      decode_event_object(Object, {ref, stripe, checkout_session});
    {ok, <<"coupon">>} ->
      decode_event_object(Object, {ref, stripe, coupon});
    {ok, <<"credit_note">>} ->
      decode_event_object(Object, {ref, stripe, credit_note});
    {ok, <<"customer">>} ->
      decode_event_object(Object, {ref, stripe, customer});
    {ok, <<"discount">>} ->
      decode_event_object(Object, {ref, stripe, discount});
    {ok, <<"dispute">>} ->
      decode_event_object(Object, {ref, stripe, dispute});
    {ok, <<"fee_refund">>} ->
      decode_event_object(Object, {ref, stripe, fee_refund});
    {ok, <<"file">>} ->
      decode_event_object(Object, {ref, stripe, file});
    {ok, <<"invoice">>} ->
      decode_event_object(Object, {ref, stripe, invoice});
    {ok, <<"invoiceitem">>} ->
      decode_event_object(Object, {ref, stripe, invoiceitem});
    {ok, <<"issuing.authorization">>} ->
      decode_event_object(Object, {ref, stripe, issuing_authorization});
    {ok, <<"issuing.card">>} ->
      decode_event_object(Object, {ref, stripe, issuing_card});
    {ok, <<"issuing.cardholder">>} ->
      decode_event_object(Object, {ref, stripe, issuing_cardholder});
    {ok, <<"issuing.dispute">>} ->
      decode_event_object(Object, {ref, stripe, issuing_dispute});
    {ok, <<"issuing.transaction">>} ->
      decode_event_object(Object, {ref, stripe, issuing_transaction});
    {ok, <<"mandate">>} ->
      decode_event_object(Object, {ref, stripe, mandate});
    {ok, <<"order">>} ->
      decode_event_object(Object, {ref, stripe, order});
    {ok, <<"order_return">>} ->
      decode_event_object(Object, {ref, stripe, order_return});
    {ok, <<"payment_intent">>} ->
      decode_event_object(Object, {ref, stripe, payment_intent});
    {ok, <<"payment_link">>} ->
      decode_event_object(Object, {ref, stripe, payment_link});
    {ok, <<"payment_method">>} ->
      decode_event_object(Object, {ref, stripe, payment_method});
    {ok, <<"payout">>} ->
      decode_event_object(Object, {ref, stripe, payout});
    {ok, <<"person">>} ->
      decode_event_object(Object, {ref, stripe, person});
    {ok, <<"plan">>} ->
      decode_event_object(Object, {ref, stripe, plan});
    {ok, <<"price">>} ->
      decode_event_object(Object, {ref, stripe, price});
    {ok, <<"product">>} ->
      decode_event_object(Object, {ref, stripe, product});
    {ok, <<"promotion_code">>} ->
      decode_event_object(Object, {ref, stripe, promotion_code});
    {ok, <<"quote">>} ->
      decode_event_object(Object, {ref, stripe, quote});
    {ok, <<"radar.early_fraud_warning">>} ->
      decode_event_object(Object, {ref, stripe, radar_early_fraud_warning});
    {ok, <<"recipient">>} ->
      decode_event_object(Object, {ref, stripe, recipient});
    {ok, <<"refund">>} ->
      decode_event_object(Object, {ref, stripe, refund});
    {ok, <<"reporting.report_run">>} ->
      decode_event_object(Object, {ref, stripe, reporting_report_run});
    {ok, <<"reporting.report_type">>} ->
      decode_event_object(Object, {ref, stripe, reporting_report_type});
    {ok, <<"review">>} ->
      decode_event_object(Object, {ref, stripe, review});
    {ok, <<"scheduled_query_run">>} ->
      decode_event_object(Object, {ref, stripe, scheduled_query_run});
    {ok, <<"sepa_debit">>} ->
      decode_event_object(Object, {ref, stripe, sepa_debit});
    {ok, <<"setup_intent">>} ->
      decode_event_object(Object, {ref, stripe, setup_intent});
    {ok, <<"sku">>} ->
      decode_event_object(Object, {ref, stripe, sku});
    {ok, <<"source_transaction">>} ->
      decode_event_object(Object, {ref, stripe, source_transaction});
    {ok, <<"subscription">>} ->
      decode_event_object(Object, {ref, stripe, subscription});
    {ok, <<"subscription_schedule">>} ->
      decode_event_object(Object, {ref, stripe, subscription_schedule});
    {ok, <<"tax_id">>} ->
      decode_event_object(Object, {ref, stripe, tax_id});
    {ok, <<"tax_rate">>} ->
      decode_event_object(Object, {ref, stripe, tax_rate});
    {ok, <<"topup">>} ->
      decode_event_object(Object, {ref, stripe, topup});
    {ok, <<"transfer">>} ->
      decode_event_object(Object, {ref, stripe, transfer});
    {ok, <<"verification_session">>} ->
      decode_event_object(Object, {ref, stripe, verification_session});
    {ok, Name} ->
      {error, {unknown_event_object, Name}};
    error ->
      {error, missing_event_object_name}
  end.

-spec decode_event_object(map(), jsv:definition()) -> stripe:result(map()).
decode_event_object(Value, Definition) ->
  Options =
    #{disable_verification => true,
      unknown_member_handling => keep,
      null_member_handling => remove},
  case jsv:validate(Value, Definition, Options) of
    {ok, Object} ->
      {ok, Object};
    {error, Errors} ->
      {error, {invalid_event_object, {jsv, Errors}}}
  end.
