-record(yahoo_session,
	{id, primary_login_id, server, notification_server, contacts}).
-record(yahoo_notification_server, {host_name, token}).
-record(yahoo_contact, {id, presence = offline}).
