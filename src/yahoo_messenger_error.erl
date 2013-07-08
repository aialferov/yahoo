%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 01 Mar 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(yahoo_messenger_error).
-export([handle/3]).

-include("yahoo_messenger_notification.hrl").

-define(AuthError, "Authorization Required").
-define(AuthTokenExpiredError, {{_, ?AuthError}, "token_expired" ++ _}).
-define(AuthSessionExpiredError, {{_, ?AuthError}, [{error, _}, {code, 28}]}).

-define(BadImCookieError, {{_, "bad IM cookie or URI credentials"}, []}).
-define(EmptyError, {{999, []}, "\n"}).

-define(FailedToConnect, {failed_connect, _}).
-define(SocketClosedRemotely, socket_closed_remotely).

-define(Flags, [oauth, session]).

handle(Command, Flags, Reason) ->
	request(error_to_request(read_error(Reason)), Command, Flags).

error_to_request(session_expired) -> {ok, login};
error_to_request(token_expired) -> {ok, refresh};
error_to_request(bad_im_cookie) -> {ok, keepalive};
error_to_request(Reason) -> {error, Reason}.

request({ok, Request}, Command, Flags) ->
	request(Request, Command, Flags, request(Request, Command));
request(Error, _Command, _Flags) -> Error.

request(login, Command) -> yahoo:login(oauth(Command));
request(refresh, Command) -> oauth:refresh_access_token(yahoo, oauth(Command));
request(keepalive, Command) ->
	yahoo:keepalive(oauth(Command), session(Command)).

request(Request, Command, Flags, {ok, Result}) ->
	request(update_request(Command, result(Request, Result), Flags));
request(_Request, _Command, _Flags, Error) -> Error.

result(Request, Result) when Request == login; Request == keepalive ->
	case Result of
		{up_to_date, Session} -> {session, Session};
		{OAuth, Session} -> {OAuth, Session}
	end;
result(refresh, OAuth) -> {oauth, OAuth}.

oauth(Command) -> {OAuth, _Session} = oauth_session(Command), OAuth.
session(Command) -> {_OAuth, Session} = oauth_session(Command), Session.

oauth_session({login, OAuth}) -> {OAuth, false};
oauth_session({logout, {OAuth, Session}}) -> {OAuth, Session};
oauth_session({keepalive, {OAuth, Session}}) -> {OAuth, Session};
oauth_session({send_message,
	{OAuth, Session, _ContactID, _Message}}) -> {OAuth, Session};
oauth_session({notification, {OAuth, Session, _Seq}}) -> {OAuth, Session}.

update_request({login, _OAuth}, {oauth, NewOAuth}, Flags) ->
	{{login, NewOAuth}, [oauth|Flags]};

update_request({Name, {OAuth, Session}}, NewArgs, Flags)
	when Name == logout; Name == keepalive
->
	{{NewOAuth, NewSession}, NewFlags} =
		update_args({OAuth, Session}, NewArgs, Flags),
	{{Name, {NewOAuth, NewSession}}, NewFlags};

update_request(
	{send_message, {OAuth, Session, ContactID, Message}}, NewArgs, Flags
) ->
	{{NewOAuth, NewSession}, NewFlags} =
		update_args({OAuth, Session}, NewArgs, Flags),
	{{send_message, {NewOAuth, NewSession, ContactID, Message}}, NewFlags};

update_request({notification, {OAuth, Session, Seq}}, NewArgs, Flags) ->
	{{NewOAuth, NewSession}, NewFlags} =
		update_args({OAuth, Session}, NewArgs, Flags),
	{{notification, {NewOAuth, NewSession, Seq}}, NewFlags}.

update_args({OAuth, Session}, NewArgs, Flags) -> case NewArgs of
	{oauth, NewOAuth} -> {{NewOAuth, Session}, [oauth|Flags]};
	{session, NewSession} -> {{OAuth, NewSession}, [session|Flags]};
	{NewOAuth, NewSession} -> {{NewOAuth, NewSession}, [oauth, session|Flags]}
end.

request({Command, Flags}) -> yahoo_messenger:request(
	Command, [Flag || Flag <- ?Flags, lists:member(Flag, Flags)]).

read_error(?FailedToConnect) -> failed_to_connect;
read_error(?SocketClosedRemotely) -> socket_closed_remotely;
read_error(?AuthTokenExpiredError) -> token_expired;
read_error(?AuthSessionExpiredError) -> session_expired;
read_error(?BadImCookieError) -> bad_im_cookie;
read_error(?EmptyError) -> empty;
read_error({Reason, [_|T]}) -> read_error({Reason, T});
read_error({_, []}) -> unknown.
