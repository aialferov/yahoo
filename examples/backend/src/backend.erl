-module(backend).
-compile(export_all).

-include("oauth.hrl").

-define(OAuth(Token, Secret, SessionHandle, Consumer, Config), #oauth{
	token = Token, secret = Secret, session_handle = SessionHandle,
	consumer = Consumer, config = Config
}).

-define(OAuthRequest(Token, Secret, Verifier), #oauth_request{
	token = Token, secret = Secret, verifier = Verifier}).

-define(OAuthConsumer, #oauth_consumer{
	key = "dj0yJmk9WEl0SG5NMldYQW9QJmQ9WVdrOVNYWnVTbXN4TXpBbWNHb" ++
		"zlNakExTnpBMk56VTJNZy0tJnM9Y29uc3VtZXJzZWNyZXQmeD05Zg--",
	secret = "04f62469a98078e32083c92e4beb0361dbce3060"
}).
-define(OAuthConfig, #oauth_config{
	url = "https://api.login.yahoo.com/oauth/v2",
	realm = "yahooapis.com",
	callback = "oob",
	version = "1.0",
	lang = "en-us",
	signature_method = "HMAC-SHA1"
}).

-define(Realm, "yahooapis.com").

get_request_token() ->
	case yahoo_oauth:get_request_token(?OAuthConsumer, ?OAuthConfig) of
		{ok, RequestToken} ->
			os:cmd("xdg-open " ++ RequestToken
				#oauth_request_token.request_auth_url),
			{ok, RequestToken};
		Error -> Error
	end.

get_token(RequestToken, RequestTokenSecret, Verifier) ->
	yahoo_oauth:get_token(
		?OAuthRequest(RequestToken, RequestTokenSecret, Verifier),
		?OAuthConsumer, ?OAuthConfig
	).

refresh_token(Token, TokenSecret, SessionHandle) ->
	yahoo_oauth:refresh_token(?OAuth(Token, TokenSecret,
		SessionHandle, ?OAuthConsumer, ?OAuthConfig)).

authorization_header() ->
	yahoo_oauth:authorization_header(
		post, ?OAuthConfig#oauth_config.url,
		[{"key1", "value1"}, {"key2", "value2"}],
		?OAuth("token", "secret", "session_handle",
			?OAuthConsumer, ?OAuthConfig)
	).

authorization_query() ->
	yahoo_oauth:authorization_query(
		post, ?OAuthConfig#oauth_config.url,
		[{"key1", "value1"}, {"key2", "value2"}],
		?OAuth("token", "secret", "session_handle", ?OAuthConsumer,
			?OAuthConfig#oauth_config{signature_method = "plaintext"})
	).

login(Token, TokenSecret) ->
	{ok, Result} = yahoo_messenger:login(?OAuth(Token, TokenSecret, [],
		?OAuthConsumer, ?OAuthConfig)),
	{_, NsTokenData} = lists:keyfind(notifyServerToken, 1, Result),
	{_, NsToken} = lists:keyfind(token, 1, NsTokenData),
	io:format("~p~n~p~n", [Result, NsToken]).

logout(Token, TokenSecret, SessionID) ->
	yahoo_messenger:logout(?OAuth(Token, TokenSecret, [],
		?OAuthConsumer, ?OAuthConfig), SessionID).

keepalive(Token, TokenSecret, SessionID) ->
	{ok, Result} = yahoo_messenger:keepalive(?OAuth(Token, TokenSecret, [],
		?OAuthConsumer, ?OAuthConfig), SessionID),
	{_, NsTokenData} = lists:keyfind(notifyServerToken, 1, Result),
	{_, NsToken} = lists:keyfind(token, 1, NsTokenData),
	io:format("~p~n~p~n", [Result, NsToken]).

send_message(Token, TokenSecret, SessionID, Server, ContactID, Message) ->
	yahoo_messenger:send_message(
		?OAuth(Token, TokenSecret, [], ?OAuthConsumer, ?OAuthConfig),
		SessionID, Server, ContactID, Message
	).

notification_loop(Token, TokenSecret, SessionID,
	Server, PrimaryLoginID, Seq, ImToken
) ->
	yahoo_messenger:receive_notification(
		?OAuth(Token, TokenSecret, [], ?OAuthConsumer, ?OAuthConfig),
		SessionID, Server, PrimaryLoginID, Seq, ImToken
	).
