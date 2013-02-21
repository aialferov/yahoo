%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 04 Feb 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-record(oauth, {token, secret, session_handle, consumer, config}).
-record(oauth_request, {token, secret, verifier}).

-record(oauth_request_token, {token, secret, expires_in, request_auth_url}).
-record(oauth_token, {token, secret, expires_in,
	session_handle, authorization_expires_in, yahoo_guid}).

-record(oauth_consumer, {key, secret}).
-record(oauth_config, {url, realm, callback, version, lang, signature_method}).
