%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 01 Mar 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(yahoo_messenger_frontend).

-export([login/1, logout/2]).
-export([send_message/4]).
-export([refresh/2, keepalive/3]).

-include("yahoo_oauth_backend.hrl").
-include("yahoo_messenger_frontend.hrl").

-define(Empty, "\n").

login(OAuth) -> login(OAuth, yahoo_messenger_backend:login(OAuth)).
login(OAuth, {ok, Result}) -> {ok, {OAuth, read_session(Result)}};
login(OAuth, {error, Reason}) -> handle_error(login, OAuth, Reason).

logout(OAuth, Session) -> logout(OAuth, Session,
	yahoo_messenger_backend:logout(OAuth, Session#session.id)).
logout(OAuth, Session, {ok, ?Empty}) -> {ok, {OAuth, Session}};
logout(OAuth, Session, {error, Reason}) ->
	handle_error(logout, {OAuth, Session}, Reason).

send_message(OAuth, Session, ContactID, Message) -> send_message(
	OAuth, Session, ContactID, Message, yahoo_messenger_backend:send_message(
		OAuth, Session#session.id, Session#session.server, ContactID, Message)
).
send_message(OAuth, Session, _ContactID, _Message, {ok, ?Empty}) ->
	{ok, {OAuth, Session}};
send_message(OAuth, Session, ContactID, Message, {error, Reason}) ->
	handle_error(send_message, {OAuth, Session, ContactID, Message}, Reason).

refresh(OAuth, Callback) ->
	refresh(OAuth, Callback, yahoo_oauth_backend:refresh_token(OAuth)).
refresh(OAuth, Callback, {ok, Token}) -> Callback(update_oauth(OAuth, Token));
refresh(OAuth, Callback, {error, Reason}) ->
	handle_error(refresh, {OAuth, Callback}, Reason).

keepalive(OAuth, Session, Callback) ->
	keepalive(OAuth, Session, Callback,
		yahoo_messenger_backend:keepalive(OAuth, Session#session.id)).
keepalive(OAuth, Session, Callback, {ok, Result}) ->
	Callback(OAuth, update_session(Session, Result));
keepalive(OAuth, Session, Callback, {error, Reason}) ->
	handle_error(keepalive, {OAuth, Session, Callback}, Reason).


read_session(Session) -> #session{
	id = binary_to_list(read(sessionId, Session)),
	primary_login_id = binary_to_list(read(primaryLoginId, Session)),
	server = binary_to_list(read(server, Session)),
	notify = #notify{
		server_token = binary_to_list(read(token,
			read(notifyServerToken, Session))),
		server = binary_to_list(read(notifyServer, Session))
	},
	contacts = lists:map(fun([{contact, Contact}]) ->
		#contact{id = binary_to_list(read(id, Contact))}
	end, read(contacts, Session))
}.

read(Key, List) -> {Key, Value} = lists:keyfind(Key, 1, List), Value.

update_oauth(OAuth, Token) -> OAuth#oauth{
	token = Token#oauth_token.token,
	secret = Token#oauth_token.secret,
	session_handle = Token#oauth_token.session_handle
}.
update_session(Session, Result) ->
	[{notifyServerToken, NotifyServerToken}] = Result,
	{token, Token} = lists:keyfind(token, 1, NotifyServerToken),
	Session#session{notify = Session#session.notify#notify{
		server_token = binary_to_list(Token)}}.

handle_error(Command, State, Reason) ->
	yahoo_messenger_error:handle(Command, State, Reason).
