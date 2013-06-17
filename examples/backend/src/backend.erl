-module(backend).
-compile(export_all).

-include("oauth.hrl").

-define(OAuthConsumer, #oauth_consumer{
	key = "dj0yJmk9WEl0SG5NMldYQW9QJmQ9WVdrOVNYWnVTbXN4TXpBbWNHb" ++
		"zlNakExTnpBMk56VTJNZy0tJnM9Y29uc3VtZXJzZWNyZXQmeD05Zg--",
	secret = "04f62469a98078e32083c92e4beb0361dbce3060",
	callback = "http://qcasper.dyndns.org"
}).

get_request_token() -> case oauth:get_request_token(?OAuthConsumer, yahoo) of
	{ok, Token} -> xdg_open(oauth:auth_url(yahoo, Token)), {ok, Token};
	Error -> Error
end.

get_access_token(OAuth, Verifier) -> oauth:get_access_token(OAuth, Verifier).
refresh_access_token(OAuth) -> oauth:refresh_access_token(OAuth).

login(OAuth) ->
	{ok, Result} = yahoo_messenger_backend:login(OAuth),
	io:format("~p~n~p~n", [Result, utils_lists:keyfind2(token,
		utils_lists:keyfind2(notifyServerToken, Result))]).

logout(OAuth, SessionID) ->
	yahoo_messenger_backend:logout(OAuth, SessionID).

keepalive(OAuth, SessionID) ->
	{ok, Result} = yahoo_messenger_backend:keepalive(OAuth, SessionID),
	io:format("~p~n~p~n", [Result, utils_lists:keyfind2(token,
		utils_lists:keyfind2(notifyServerToken, Result))]).

send_message(OAuth, SessionID, Server, ContactID, Message) ->
	yahoo_messenger_backend:send_message(
		OAuth, SessionID, Server, ContactID, Message).

notification_loop(OAuth, SessionID, Server, PrimaryLoginID, Seq, ImToken) ->
	yahoo_messenger_backend:receive_notification(
		OAuth, SessionID, Server, PrimaryLoginID, Seq, ImToken).

xdg_open(Params) -> os:cmd("xdg-open \"" ++ Params ++ "\"").
