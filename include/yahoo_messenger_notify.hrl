-record(notification, {sequence = 0, responses = []}).
-record(buddy_info, {contact_ids = []}).
-record(buddy_status, {}).
-record(log_off, {contact_id}).
-record(message, {contact_id, timestamp, text}).
-record(offline_message, {messages}).
-record(disconnect, {reason}).

-record(notify_handler, {
	handle_oauth, handle_session, handle_oauth_session, handle_responses}).
