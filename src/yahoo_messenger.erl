%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 04 Feb 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(yahoo_messenger).

-export([login/1, logout/2, keepalive/2]).
-export([send_message/5, receive_notification/6]).

-define(ContentType, "application/json;charset=utf-8").

-define(MessengerApiUrl, "http://developer.messenger.yahooapis.com").

-define(SessionUrl, ?MessengerApiUrl ++ "/v1/session").
-define(KeepAliveUrl, ?MessengerApiUrl ++ "/v1/session/keepalive").
-define(MessageUrl(Server, ContactID),
	"http://" ++ Server ++ "/v1/message/yahoo/" ++ ContactID).
-define(PushNotificationUrl(Server, PrimaryLoginID),
	"http://" ++ Server ++ "/v1/pushchannel/" ++ PrimaryLoginID).

-define(LoginParams, [
	{"fieldsBuddyList", "%2Bgroups"},
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
-define(MessageBody(Message), "{\"message\" : \"" ++ Message ++ "\"}").

login(OAuth) ->
	read_response(httpc:request(post, {
		url(?SessionUrl, ?LoginParams),
		[yahoo_oauth:auth_header(post, ?SessionUrl, ?LoginParams, OAuth)],
		?ContentType, ?LoginBody
	}, [], [])).

logout(OAuth, SessionID) ->
	read_response(httpc:request(delete, {
		url(?SessionUrl, ?LogoutParams(SessionID)),
		[yahoo_oauth:auth_header(delete,
			?SessionUrl, ?LogoutParams(SessionID), OAuth)]
	}, [], [])).

keepalive(OAuth, SessionID) ->
	read_response(httpc:request(put, {
		url(?KeepAliveUrl, ?KeepAliveParams(SessionID)),
		[yahoo_oauth:auth_header(put,
			?KeepAliveUrl, ?KeepAliveParams(SessionID), OAuth)], [], []
	}, [], [])).

send_message(OAuth,	SessionID, Server, ContactID, Message) ->
	read_response(httpc:request(post, {
		url(?MessageUrl(Server, ContactID), ?MessageParams(SessionID)),
		[yahoo_oauth:auth_header(post,
			?MessageUrl(Server, ContactID), ?MessageParams(SessionID), OAuth)],
		?ContentType, ?MessageBody(Message)
	}, [], [])).

receive_notification(OAuth, SessionID, Server, PrimaryLoginID, Seq, ImToken) ->
	Url = ?PushNotificationUrl(Server, PrimaryLoginID),
	Params = ?PushNotificationParams(SessionID, Seq, ImToken),
	Request = url(Url, Params,
		yahoo_oauth:auth_query(get, Url, Params, OAuth)),
	read_response(httpc:request(Request)).

read_response({ok, {{_, 200, _}, _, Body}}) -> {ok, read_json(Body)};
read_response({ok, {{_, _, Reason}, _, Body}}) ->
	{error, {Reason, read_json(Body)}};
read_response(Error) -> Error.

read_json(Json) ->
	JsonBinary = list_to_binary(Json),
	case jsx:is_json(JsonBinary) of
		true -> jsx:decode(JsonBinary, [{labels, atom}]);
		_ -> Json
	end.

url(Url, []) -> Url;
url(Url, Params) -> Url ++ "?" ++ utils_http:query_string(Params).
url(Url, Params, []) -> url(Url, Params);
url(Url, Params, ParamString) -> url(Url, Params) ++ "&" ++ ParamString.
