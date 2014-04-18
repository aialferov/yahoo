%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 26 Jun 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(yahoo_messenger_session).
-export([read/1, update/2]).

-include("yahoo_messenger.hrl").

read(Session) -> #yahoo_session{
	id = binary_to_list(utils_lists:value(sessionId, Session)),
	primary_login_id = binary_to_list(
		utils_lists:value(primaryLoginId, Session)),
	server = binary_to_list(utils_lists:value(server, Session)),
	notification_server = #yahoo_notification_server{
		token = binary_to_list(utils_lists:value(
			token, utils_lists:value(notifyServerToken, Session))),
		host_name = binary_to_list(utils_lists:value(notifyServer, Session))
	},
	contacts = lists:map(fun([{contact, Contact}]) ->
		#yahoo_contact{id = binary_to_list(utils_lists:value(id, Contact))}
	end, utils_lists:value(contacts, Session))
}.

update(S = _Session, [{notifyServerToken, NotifyServerToken}]) ->
	S#yahoo_session{notification_server =
		S#yahoo_session.notification_server#yahoo_notification_server{token =
			binary_to_list(utils_lists:value(token, NotifyServerToken))}
	}.
