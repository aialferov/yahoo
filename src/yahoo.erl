%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 26 Mar 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(yahoo).

-export([start/0, stop/0]).

-export([login/1, logout/2]).
-export([keepalive/2]).

-export([send_message/4]).
-export([notification/3]).

start() -> application:start(?MODULE).
stop() -> application:stop(?MODULE).

login(OAuth) -> request({login, OAuth}).
logout(OAuth, Session) -> request({logout, {OAuth, Session}}).
keepalive(OAuth, Session) -> request({keepalive, {OAuth, Session}}).

send_message(OAuth, Session, ContactID, Message) ->
	request({send_message, {OAuth, Session, ContactID, Message}}).
notification(OAuth, Session, Seq) ->
	request({notification, {OAuth, Session, Seq}}).

request(Command) -> yahoo_messenger:request(Command, []).
