%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 01 Mar 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(yahoo_messenger_frontend).
-export([request/2]).

-include("yahoo_messenger_frontend.hrl").

-define(Empty, "\n").

request(Command = {login, OAuth}, Flags) ->
	request(Command, Flags, yahoo_messenger_backend:login(OAuth));
request(Command = {logout, {OAuth, #yahoo_session{id = SessionID}}}, Flags) ->
	request(Command, Flags, yahoo_messenger_backend:logout(OAuth, SessionID));
request(Command = {keepalive, {OAuth, S = _Session}}, Flags) ->
	request(Command, Flags, yahoo_messenger_backend:keepalive(
		OAuth, S#yahoo_session.id));
request(
	Command = {send_message, {OAuth, S = _Session, ContactID, Message}}, Flags
) ->
	request(Command, Flags, yahoo_messenger_backend:send_message(OAuth,
		S#yahoo_session.id, S#yahoo_session.server, ContactID, Message));
request(Command = {notification, {OAuth, S = _Session, Seq}}, Flags) ->
	request(Command, Flags, yahoo_messenger_backend:notification(OAuth,
		S#yahoo_session.id,
		S#yahoo_session.notification_server
			#yahoo_notification_server.host_name,
		S#yahoo_session.primary_login_id, integer_to_list(Seq),
		S#yahoo_session.notification_server#yahoo_notification_server.token
	)).

request({login, OAuth}, Flags, {ok, Result}) ->
	result(Flags, {oauth, OAuth}, yahoo_messenger_session:read(Result));
request({logout, {OAuth, Session}}, Flags, {ok, ?Empty}) ->
	result(Flags, {OAuth, Session});
request({keepalive, {OAuth, Session}}, Flags, {ok, Result}) -> result(
	Flags, {oauth, OAuth}, yahoo_messenger_session:update(Session, Result));
request(
	{send_message, {OAuth, Session, _ContactID, _Message}}, Flags, {ok, ?Empty}
) ->
	result(Flags, {OAuth, Session});
request({notification, {OAuth, Session, _Seq}}, Flags, {ok, Result}) ->
	result(Flags, {OAuth, Session}, yahoo_messenger_notify:read(Result));

request(Command, Flags, {error, Reason}) ->
	yahoo_messenger_error:handle(Command, Flags, Reason).

result([], _Args) -> ok;
result(Flags, Args) -> {ok, args(Flags, Args)}.

result(Flags, {oauth, OAuth}, Result) ->
	{ok, {case lists:member(oauth, Flags) of
		true -> OAuth; false -> up_to_date end, Result}};

result([], _Args, Result) -> {ok, Result};
result(Flags, Args, Result) -> {ok, {args(Flags, Args), Result}}.

args([oauth], {OAuth, _Session}) -> {OAuth, up_to_date};
args([session], {_OAuth, Session}) -> {up_to_date, Session};
args([oauth, session], {OAuth, Session}) -> {OAuth, Session}.
