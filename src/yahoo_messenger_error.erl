%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 01 Mar 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(yahoo_messenger_error).
-export([handle/3]).

-include("yahoo_messenger_notify.hrl").

-define(AuthError, "Authorization Required").
-define(AuthTokenExpiredError, {{_, ?AuthError}, "token_expired" ++ _}).
-define(AuthSessionExpiredError, {{_, ?AuthError}, [{error, _}, {code, 28}]}).

-define(BadImCookieError, {{_, "bad IM cookie or URI credentials"}, []}).
-define(EmptyError, {{999, []}, "\n"}).

handle(Command, State, Reason) ->
	io:format("handle error: ~p ~p~n", [Command, Reason]),
	handle(Command, State, Reason, read_error(Reason)).
handle(_Command, _State, Reason, unknown) -> handle(error, Reason);
handle(Command, State, Reason, Known) ->
	handle(handle_error(Command, State, Known), Reason).

handle(error, Reason) -> exit({error, Reason});
handle(Result, _Reason) -> Result.

handle_error(login, OAuth, token_expired) ->
	refresh(OAuth, fun(NewOAuth) -> login({new, NewOAuth}) end);
handle_error(logout, {OAuth, Session}, token_expired) ->
	refresh(OAuth, fun(NewOAuth) -> logout({new, NewOAuth}, Session) end);
handle_error(send_message, State, token_expired) ->
	{OAuth, Session, ContactID, Message} = State,
	refresh(OAuth, fun(NewOAuth) -> send_message(
		NewOAuth, Session, ContactID, Message) end);
handle_error(keepalive, {OAuth, Session, Callback}, token_expired) ->
	refresh(OAuth, fun(NewOAuth) -> keepalive(
		NewOAuth, Session, Callback) end);

handle_error(notification, State, token_expired) ->
	{NotifyHandler, OAuth, Session, Seq} = State,
	refresh(OAuth, fun(NewOAuth) ->
		(NotifyHandler#notify_handler.handle_oauth)(NewOAuth),
		yahoo_messenger_notify:loop(NotifyHandler, NewOAuth, Session, Seq)
	end);
handle_error(notification, State, bad_im_cookie) ->
	{NotifyHandler, OAuth, Session, Seq} = State,
	keepalive(OAuth, Session, fun(NewOAuth, NewSession) ->
		(NotifyHandler#notify_handler.handle_oauth_session)
			(NewOAuth, NewSession),
		yahoo_messenger_notify:loop(NotifyHandler, NewOAuth, NewSession, Seq)
	end);
handle_error(notification, State, Reason) when
	Reason == empty; Reason == socket_closed_remotely
->
	{NotifyHandler, OAuth, Session, Seq} = State,
	yahoo_messenger_notify:loop(NotifyHandler, OAuth, Session, Seq);

handle_error(_Command, _State, _Reason) -> error.

read_error(socket_closed_remotely) -> socket_closed_remotely;
read_error(?AuthTokenExpiredError) -> token_expired;
read_error(?AuthSessionExpiredError) -> session_expired;
read_error(?BadImCookieError) -> bad_im_cookie;
read_error(?EmptyError) -> empty;
read_error({Reason, [_|T]}) -> read_error({Reason, T});
read_error({_, []}) -> unknown.


login(OAuth) -> yahoo_messenger_frontend:login(OAuth).
logout(OAuth, Session) -> yahoo_messenger_frontend:login(OAuth, Session).
send_message(OAuth, Session, ContactID, Message) ->
	yahoo_messenger_frontend:login(OAuth, Session, ContactID, Message).

refresh(OAuth, Callback) -> yahoo_messenger_frontend:refresh(OAuth, Callback).
keepalive(OAuth, Session, Callback) ->
	yahoo_messenger_frontend:keepalive(OAuth, Session, Callback).
