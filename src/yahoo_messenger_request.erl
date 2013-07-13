%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 04 Feb 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(yahoo_messenger_request).

-export([login/1, logout/2, keepalive/2]).
-export([send_message/5, notification/6]).

-define(ContentType, "application/json;charset=utf-8").

-define(MessengerApiUrl, "http://developer.messenger.yahooapis.com").

-define(SessionUrl, ?MessengerApiUrl ++ "/v1/session").
-define(KeepAliveUrl, ?SessionUrl ++ "/keepalive").
-define(MessageUrl(Server, ContactID),
	"http://" ++ Server ++ "/v1/message/yahoo/" ++ ContactID).
-define(PushNotificationUrl(Server, PrimaryLoginID),
	"http://" ++ Server ++ "/v1/pushchannel/" ++ PrimaryLoginID).

-define(LoginParams, [
	{"fieldsBuddyList", "%2Baddressbook"},
	{"notifyServerToken", "2"}
]).
-define(LogoutParams(SessionID), [{"sid", SessionID}]).
-define(KeepAliveParams(SessionID), [
	{"sid", SessionID},
	{"notifyServerToken", "2"}
]).
-define(MessageParams(SessionID), [{"sid", SessionID}]).
-define(PushNotificationParams(SessionID, Seq, ImToken), [
	{"sid", SessionID},
	{"seq", Seq},
	{"format", "json"},
	{"imtoken", ImToken}
]).

-define(LoginBody, "{}").
-define(MessageBody(Message),
	jsx:encode([{<<"message">>, unicode:characters_to_binary(Message)}])).

login(OAuth) ->
	Url = ?SessionUrl, Params = ?LoginParams,
	read_response(httpc:request(post, {utils_http:url(Url, Params),
		[oauth:auth_header(post, Url, Params, OAuth)],
		?ContentType, ?LoginBody
	}, [], [])).

logout(OAuth, SessionID) ->
	Url = ?SessionUrl, Params = ?LogoutParams(SessionID),
	read_response(httpc:request(delete, {utils_http:url(Url, Params),
		[oauth:auth_header(delete, Url, Params, OAuth)]}, [], [])).

keepalive(OAuth, SessionID) ->
	Url = ?KeepAliveUrl, Params = ?KeepAliveParams(SessionID),
	read_response(httpc:request(put, {utils_http:url(Url, Params),
		[oauth:auth_header(put, Url, Params, OAuth)], [], []}, [], [])).

send_message(OAuth,	SessionID, Server, ContactID, Message) ->
	Url = ?MessageUrl(Server, ContactID), Params = ?MessageParams(SessionID),
	read_response(httpc:request(post, {utils_http:url(Url, Params),
		[oauth:auth_header(post, Url, Params, OAuth)],
		?ContentType, ?MessageBody(Message)
	}, [], [])).

notification(OAuth, SessionID, Server, PrimaryLoginID, Seq, ImToken) ->
	Url = ?PushNotificationUrl(Server, PrimaryLoginID),
	Params = ?PushNotificationParams(SessionID, Seq, ImToken),
	read_response(httpc:request(utils_http:url(
		Url, Params, oauth:auth_query(get, Url, Params, OAuth)))).

read_response({ok, {{_, 200, _}, _, Body}}) -> {ok, read_json(Body)};
read_response({ok, {{_, Code, Reason}, _, Body}}) ->
	{error, {{Code, Reason}, read_json(Body)}};
read_response(Error) -> Error.

read_json(Json) ->
	case begin JsonBin = list_to_binary(Json), jsx:is_json(JsonBin) end of
		true -> jsx:decode(JsonBin, [{labels, atom}]); _ -> Json end.
