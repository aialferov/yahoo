-record(yahoo_notification, {sequence = 0, responses = []}).
-record(yahoo_buddy, {sender, presence_message}).
-record(yahoo_buddy_info, {buddies = []}).
-record(yahoo_buddy_status, {buddy}).
-record(yahoo_log_off, {buddy}).
-record(yahoo_message, {sender, msg, time_stamp}).
-record(yahoo_offline_message, {messages}).
-record(yahoo_disconnect, {reason}).
