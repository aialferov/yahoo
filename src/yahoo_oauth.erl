%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 04 Feb 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(yahoo_oauth).

-export([get_request_token/2, get_token/3, refresh_token/1]).
-export([auth_header/4, auth_query/4]).

-include("oauth.hrl").

-define(OAuthRequestTokenRecord, {#oauth_request_token{}, [
    "oauth_token",
    "oauth_token_secret",
    "oauth_expires_in",
    "xoauth_request_auth_url"
]}).
-define(OAuthTokenRecord, {#oauth_token{}, [
    "oauth_token",
    "oauth_token_secret",
    "oauth_expires_in",
    "oauth_session_handle",
    "oauth_authorization_expires_in",
    "xoauth_yahoo_guid"
]}).

-define(NonceFormat, [4]).

get_request_token(
	#oauth_consumer{key = ConsumerKey, secret = ConsumerSecret},
	#oauth_config{
		url = Url, callback = Callback, version = Version,
		lang = Lang, signature_method = SignatureMethod
	}
) ->
	read_oauth(request_token(Url ++ "/get_request_token", [
		{"oauth_callback", Callback},
		{"oauth_consumer_key", ConsumerKey},
		{"oauth_nonce", generate_nonce()},
		{"oauth_signature_method", SignatureMethod},
		{"oauth_timestamp", timestamp()},
		{"oauth_version", Version},
		{"xoauth_lang_pref", Lang}
	], ConsumerSecret), ?OAuthRequestTokenRecord).

get_token(
	#oauth_request{token = Token, secret = TokenSecret, verifier = Verifier},
	#oauth_consumer{key = ConsumerKey, secret = ConsumerSecret},
	#oauth_config{url = Url, version = Version,
		signature_method = SignatureMethod}
) ->
	read_oauth(request_token(Url ++ "/get_token", [
		{"oauth_consumer_key", ConsumerKey},
		{"oauth_nonce", generate_nonce()},
		{"oauth_signature_method", SignatureMethod},
		{"oauth_timestamp", timestamp()},
		{"oauth_token", Token},
		{"oauth_verifier", Verifier},
		{"oauth_version", Version}
	], ConsumerSecret, TokenSecret), ?OAuthTokenRecord).

refresh_token(#oauth{
	token = Token, secret = TokenSecret, session_handle = SessionHandle,
	consumer = #oauth_consumer{key = ConsumerKey, secret = ConsumerSecret},
	config = #oauth_config{url = Url, version = Version,
		signature_method = SignatureMethod}
}) ->
	read_oauth(request_token(Url ++ "/get_token", [
		{"oauth_consumer_key", ConsumerKey},
		{"oauth_nonce", generate_nonce()},
		{"oauth_session_handle", SessionHandle},
		{"oauth_signature_method", SignatureMethod},
		{"oauth_timestamp", timestamp()},
		{"oauth_token", http_uri:encode(Token)},
		{"oauth_version", Version}
	], ConsumerSecret, TokenSecret), ?OAuthTokenRecord).

request_token(Url, Params, ConsumerSecret) ->
	request_token(Url, Params, ConsumerSecret, []).
request_token(Url, Params, ConsumerSecret, TokenSecret) ->
	{"oauth_signature_method", SignatureMethod} =
		lists:keyfind("oauth_signature_method", 1, Params),
	Signature = signature(signature_method(SignatureMethod),
		post, Url, Params, ConsumerSecret, TokenSecret),
	httpc:request(post, {Url, [], "application/x-www-form-urlencoded",
		utils_http:query_string([{"oauth_signature", Signature}|Params])},
	[], []).

read_oauth({ok, {{_, 200, _}, _, Body}}, {Record, Keys}) ->
	Query = utils_http:read_query(Body),
	{ok, lists:foldl(fun(Key, R) -> case lists:keyfind(Key, 1, Query) of
		{Key, Value} -> update_record(R, Key, Value); false -> R
	end end, Record, Keys)};
read_oauth({ok, {{_, _, _}, _, Body}}, _) -> {error, Body};
read_oauth(Error, _) -> Error.

update_record(R = #oauth_request_token{}, Key, Value) -> case Key of
	"oauth_token" -> R#oauth_request_token{token = Value};
	"oauth_token_secret" -> R#oauth_request_token{secret = Value};
	"oauth_expires_in" -> R#oauth_request_token{expires_in = Value};
	"xoauth_request_auth_url" ->
		R#oauth_request_token{request_auth_url = Value}
end;
update_record(R = #oauth_token{}, Key, Value) -> case Key of
	"oauth_token" -> R#oauth_token{token = Value};
	"oauth_token_secret" -> R#oauth_token{secret = Value};
	"oauth_expires_in" -> R#oauth_token{expires_in = Value};
	"oauth_session_handle" -> R#oauth_token{session_handle = Value};
	"oauth_authorization_expires_in" ->
		R#oauth_token{authorization_expires_in = Value};
	"xoauth_yahoo_guid" -> R#oauth_token{yahoo_guid = Value}
end.

auth_header(Method, Url, Params, OAuth = #oauth{
	secret = TokenSecret,
	consumer = #oauth_consumer{secret = ConsumerSecret},
	config = #oauth_config{realm = Realm, signature_method = SignatureMethod}
}) ->
	AuthParams = auth_params(OAuth),
	Signature = signature(signature_method(SignatureMethod),
		Method, Url, Params ++ AuthParams, ConsumerSecret, TokenSecret),
	{"Authorization", "OAuth " ++ utils_http:header_string([
		{"realm", http_uri:encode(Realm)},
		{"oauth_signature", Signature}|AuthParams
	])}.

auth_query(Method, Url, Params, OAuth = #oauth{
	secret = TokenSecret,
	consumer = #oauth_consumer{secret = ConsumerSecret},
	config = #oauth_config{signature_method = SignatureMethod}
}) ->
	AuthParams = auth_params(OAuth),
	Signature = signature(signature_method(SignatureMethod),
		Method, Url, Params ++ AuthParams, ConsumerSecret, TokenSecret),
	utils_http:query_string([{"oauth_signature", Signature}|AuthParams]).

auth_params(#oauth{
	token = Token, consumer = #oauth_consumer{key = ConsumerKey},
	config = #oauth_config{version = Version,
		signature_method = SignatureMethod}
}) -> [
	{"oauth_consumer_key", ConsumerKey},
	{"oauth_nonce", generate_nonce()},
	{"oauth_signature_method", SignatureMethod},
	{"oauth_timestamp", timestamp()},
	{"oauth_token", http_uri:encode(Token)},
	{"oauth_version", Version}
].

signature(plaintext, _Method, _Url, _Params, ConsumerSecret, TokenSecret) ->
	ConsumerSecret ++ "%26" ++ TokenSecret;
signature(hmac_sha1, Method, Url, Params, ConsumerSecret, TokenSecret) ->
	BaseString = case Method of
			get -> "GET";
			put -> "PUT";
			post -> "POST";
			delete -> "DELETE"
		end ++ "&" ++ http_uri:encode(Url) ++ "&" ++ http_uri:encode(
			utils_http:query_string(lists:keysort(1, Params)))
	,
	http_uri:encode(binary_to_list(base64:encode(crypto:sha_mac(
		ConsumerSecret ++ "&" ++ TokenSecret, BaseString)))).

signature_method("HMAC-SHA1") -> hmac_sha1;
signature_method("PLAINTEXT") -> plaintext;
signature_method("plaintext") -> plaintext.

generate_nonce() -> utils_crypto:generate_nonce(?NonceFormat).

timestamp() ->
	{MSecs, Secs, _} = erlang:now(),
	integer_to_list(MSecs * 1000000 + Secs).
