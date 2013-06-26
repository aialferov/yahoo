%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 26 Jun 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(yahoo_messenger_session).
-export([read/1, update/2]).

-include("yahoo_messenger_frontend.hrl").

read(Session) -> #yahoo_session{
	id = binary_to_list(utils_lists:keyfind2(sessionId, Session)),
	primary_login_id = binary_to_list(
		utils_lists:keyfind2(primaryLoginId, Session)),
	server = binary_to_list(utils_lists:keyfind2(server, Session)),
	notification_server = #yahoo_notification_server{
		token = binary_to_list(utils_lists:keyfind2(
			token, utils_lists:keyfind2(notifyServerToken, Session))),
		host_name = binary_to_list(utils_lists:keyfind2(notifyServer, Session))
	},
	contacts = lists:map(fun([{contact, Contact}]) ->
		#yahoo_contact{id = binary_to_list(utils_lists:keyfind2(id, Contact))}
	end, utils_lists:keyfind2(contacts, Session))
}.

update(S = _Session, [{notifyServerToken, NotifyServerToken}]) ->
	S#yahoo_session{notification_server =
		S#yahoo_session.notification_server#yahoo_notification_server{token =
			binary_to_list(utils_lists:keyfind2(token, NotifyServerToken))}
	}.
