%%%-------------------------------------------------------------------
%%% Created: 21 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

{application, yahoo, [
	{id, "yahoo"},
	{vsn, "0.0.1"},
	{description, "Yahoo! API messaging library"},
	{modules, [
		yahoo,
		yahoo_messenger,
		yahoo_messenger_error,
		yahoo_messenger_request,
		yahoo_messenger_session,
		yahoo_messenger_notification
	]},
	{registered, []},
	{applications, [kernel, stdlib, ssl, inets, utils, oauth]}
]}.
