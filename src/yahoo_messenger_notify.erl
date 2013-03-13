%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 01 Mar 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(yahoo_messenger_notify).
-export([loop/3, loop/4]).

-include("yahoo_messenger_notify.hrl").
-include("yahoo_messenger_frontend.hrl").

loop(NotifyHandler, OAuth, Session) ->
	loop(NotifyHandler, OAuth, Session, 0).

loop(NotifyHandler, OAuth, Session, Seq) ->
	loop(NotifyHandler, OAuth, Session, Seq,
		yahoo_messenger_backend:receive_notification(
			OAuth, Session#session.id,
			Session#session.notify#notify.server,
			Session#session.primary_login_id, integer_to_list(Seq),
			Session#session.notify#notify.server_token
		)
	).
loop(NotifyHandler, OAuth, Session, Seq, {ok, []}) ->
	loop(NotifyHandler, OAuth, Session, Seq);
loop(NotifyHandler, OAuth, Session, _Seq, {ok, Result}) ->
	Notification = read_notification(Result),
	(NotifyHandler#notify_handler.handle_responses)
		(Notification#notification.responses),
	case lists:keyfind(disconnect, 1, Notification#notification.responses) of
		#disconnect{} -> ok;
		false -> loop(NotifyHandler, OAuth, Session,
			Notification#notification.sequence + 1)
	end;
loop(NotifyHandler, OAuth, Session, Seq, {error, Reason}) ->
	yahoo_messenger_error:handle(notification,
		{NotifyHandler, OAuth, Session, Seq}, Reason).

read_notification(Notification) ->
	lists:foldl(fun([{Type, Details}], Acc) ->
		update_notification(Acc, read_notification(Type, Details))
	end, #notification{}, read(responses, Notification)).

read_notification(buddyInfo, BuddyInfo) ->
	{read(sequence, BuddyInfo), #buddy_info{
		contact_ids = case lists:keyfind(contact, 1, BuddyInfo) of
			{_, Contacts} -> lists:map(fun(Contact) ->
				binary_to_list(read(sender, Contact)) end, Contacts);
			false -> []
		end
	}};

read_notification(buddyStatus, BuddyStatus) ->
	{read(sequence, BuddyStatus), #buddy_status{}};

read_notification(logOff, LogOff) ->
	{read(sequence, LogOff), #log_off{
		contact_id = binary_to_list(read(buddy, LogOff))}};

read_notification(message, Message) ->
	{read(sequence, Message), read_message(Message)};

read_notification(offlineMessage, OfflineMessage) ->
	ReadMessage = fun(Message) -> read_message(read(message, Message)) end,
	{read(sequence, OfflineMessage), #offline_message{
		messages = lists:map(ReadMessage, read(messages, OfflineMessage))}};

read_notification(disconnect, Disconnect) ->
	{read(sequence, Disconnect), #disconnect{
		reason = disconnect_reason(read(reason, Disconnect))}}.

read_message(Message) -> #message{
	contact_id = binary_to_list(read(sender, Message)),
	timestamp = read(timeStamp, Message),
	text = binary_to_list(read(msg, Message))
}.

read(Key, List) -> {Key, Value} = lists:keyfind(Key, 1, List), Value.

disconnect_reason(1) -> regen;
disconnect_reason(2) -> idle;
disconnect_reason(3) -> queue_full;
disconnect_reason(4) -> self_logoff.


update_notification(Notification, {Sequence, Response}) ->
	Notification#notification{
		sequence = if Sequence > Notification#notification.sequence ->
			Sequence; true -> Notification#notification.sequence end,
		responses = [Response|Notification#notification.responses]
	}.
