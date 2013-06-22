%%%-------------------------------------------------------------------
%%% Created: 21 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

{application, yahoo, [
	{id, "yahoo"},
	{vsn, "0.0.1"},
	{description, "Yahoo! API messaging library"},
	{modules, [
		yahoo_messenger_backend,
		yahoo_messenger_error,
		yahoo_messenger_frontend,
		yahoo_messenger_notify
	]},
	{registered, []},
	{applications, [kernel, stdlib, ssl, httpc, utils, oauth]}
]}.
