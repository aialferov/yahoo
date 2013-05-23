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

-include("oauth.hrl").
-include("yahoo_messenger_frontend.hrl").

-define(Empty, "\n").

login(OAuth = #oauth{}) -> login({current, OAuth});
login({Version, OAuth}) ->
	login({Version, OAuth}, yahoo_messenger_backend:login(OAuth)).
login({current, _OAuth}, {ok, Result}) -> {ok, read_session(Result)};
login({new, OAuth}, {ok, Result}) -> {ok, {OAuth, read_session(Result)}};
login({_Version, OAuth}, {error, Reason}) ->
	handle_error(login, OAuth, Reason).

logout(OAuth = #oauth{}, Session) -> logout({current, OAuth}, Session);
logout({Version, OAuth}, Session) -> logout({Version, OAuth}, Session,
	yahoo_messenger_backend:logout(OAuth, Session#session.id)).
logout({current, _OAuth}, Session, {ok, ?Empty}) -> {ok, Session};
logout({new, OAuth}, Session, {ok, ?Empty}) -> {ok, {OAuth, Session}};
logout({_Version, OAuth}, Session, {error, Reason}) ->
	handle_error(logout, {OAuth, Session}, Reason).

send_message(OAuth = #oauth{}, Session, ContactID, Message) ->
	send_message({current, OAuth}, Session, ContactID, Message);
send_message({Version, OAuth}, Session, ContactID, Message) -> send_message(
	{Version, OAuth}, Session, ContactID, Message,
	yahoo_messenger_backend:send_message(
		OAuth, Session#session.id, Session#session.server, ContactID, Message)
).
send_message({current, _OAuth}, Session, _ContactID, _Message, {ok, ?Empty}) ->
	{ok, Session};
send_message({new, OAuth}, Session, _ContactID, _Message, {ok, ?Empty}) ->
	{ok, {OAuth, Session}};
send_message(
	{_Version, OAuth}, Session,
	ContactID, Message, {error, Reason}
) ->
	handle_error(send_message, {OAuth, Session, ContactID, Message}, Reason).

refresh(OAuth, Callback) ->
	refresh(OAuth, Callback, oauth:refresh_access_token(yahoo, OAuth)).
refresh(OAuth, Callback, {ok, Token}) -> Callback(OAuth#oauth{token = Token});
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

update_session(Session, Result) ->
	[{notifyServerToken, NotifyServerToken}] = Result,
	{token, Token} = lists:keyfind(token, 1, NotifyServerToken),
	Session#session{notify = Session#session.notify#notify{
		server_token = binary_to_list(Token)}}.

handle_error(Command, State, Reason) ->
	yahoo_messenger_error:handle(Command, State, Reason).
