-module(mnesia_manager).
-export([init/1, handle_call/3, handle_cast/2]).
-export([start_server/0, login/2, register/3, add_credit/2, get_credit/1, add_good/3, update_good/2, get_user_goods/1,
	get_good/1, get_user/1, in_auction/1, all_auctions/1, get_auction_from_good/1]).
-behavior(gen_server).

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% START SERVER
%%%===================================================================

start_server() ->
	mnesia_server:init(),
	gen_server:start_link({local,mnesia_manager},?MODULE, [], []).

%%%===================================================================
%%% USER OPERATIONS
%%%===================================================================

login(Username, Password) ->
	gen_server:call(mnesia_manager, {login, {Username, Password}}).

register(Username, Password, Credit) ->
	gen_server:call(mnesia_manager, {register, {Username, Password, Credit}}).

add_credit(Username, NewCredit) ->
	gen_server:call(mnesia_manager, {add_credit, {Username, NewCredit}}).

get_credit(Username) ->
	gen_server:call(mnesia_manager, {get_credit, Username}).

get_user(Username) ->
	gen_server:call(mnesia_manager, {get_user, Username}).

%%%===================================================================
%%% GOOD OPERATIONS
%%%===================================================================

add_good(Name, Description, User) ->
	gen_server:call(mnesia_manager, {add_good, {Name, Description, User}}).

update_good(GoodId, User) -> 
	gen_server:call(mnesia_manager, {update_good, {GoodId, User}}).

get_user_goods(User) ->
	gen_server:call(mnesia_manager, {get_user_goods, User}).

get_good(GoodId) ->
	gen_server:call(mnesia_manager, {get_good, GoodId}).

%%%===================================================================
%%% AUCTION OPERATIONS
%%%===================================================================

in_auction(GoodId) -> 
	gen_server:call(mnesia_manager, {in_auction, GoodId}).

all_auctions(User) -> 
	gen_server:call(mnesia_manager, {all_auctions, User}).

get_auction_from_good(GoodId) -> 
	gen_server:call(mnesia_manager, {get_auction_from_good, GoodId}).


%%%===================================================================
%%% CALLBACK FUNCTIONS
%%%===================================================================

init(_InitialState) ->
  	{
  		ok,
  		[]
  	}.


handle_call({login, {Username, Password}}, _From, _Status) ->
	Result = mnesia_server:login(Username,Password),
	{reply, Result, _Status };

handle_call({register, {Username, Password, Credit}}, _From, _Status) ->
	Result = mnesia_server:register(Username,Password,Credit),
	{reply, Result, _Status };

handle_call({add_credit, {Username, NewCredit}}, _From, _Status) ->
	Result = mnesia_server:add_credit(Username, NewCredit),
	{reply, Result, _Status };

handle_call({get_credit, Username}, _From, _Status) ->
	Result = mnesia_server:get_credit(Username),
	{reply, Result, _Status };

handle_call({get_user, Username}, _From, _Status) ->
	Result = mnesia_server:get_user(Username),
	{reply, Result, _Status };

handle_call({add_good, {Name, Description, User}}, _From, _Status) ->
	Result = mnesia_server:add_good(Name, Description, User),
	{reply, Result, _Status };

handle_call({update_good, {GoodId, User}}, _From, _Status) ->
	Result = mnesia_server:update_good(GoodId, User),
	{reply, Result, _Status };

handle_call({get_user_goods, User}, _From, _Status) ->
	Result = mnesia_server:get_user_goods(User),
	{reply, Result, _Status };

handle_call({get_good, GoodId}, _From, _Status) ->
	Result = mnesia_server:get_good(GoodId),
	{reply, Result, _Status };

handle_call({in_auction, GoodId}, _From, _Status) ->
	Result = mnesia_server:in_auction(GoodId),
	{reply, Result, _Status };

handle_call({all_auctions, User}, _From, _Status) ->
	Result = mnesia_server:all_auctions(User),
	{reply, Result, _Status };

handle_call({get_auction_from_good, GoodId}, _From, _Status) ->
	Result = mnesia_server:get_auction_from_good(GoodId),
	{reply, Result, _Status }.

handle_cast(reset, _Status) ->
	{
		noreply,
		[]
	}.

%%ADD terminate both here and in the other file