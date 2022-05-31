-module(auction_server).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).
-export([get_info/1, get_status/1, decrease_timer/1, finish_auction/1, make_offer/2, stop_auction/1, start_auction/4]).

%%API%%

start_auction(GoodId,Duration,InitialPrice,Seller) ->
	AuctionID = mnesia_server:add_auction(GoodId, Duration, InitialPrice, InitialPrice, empty, Seller, in_auction),
	gen_server:start({local,get_server_name(AuctionID)},?MODULE, {AuctionID,GoodId,Duration,InitialPrice,Seller,spawn(fun()-> timer(Duration, get_server_name(AuctionID)) end)}, []),
	get_server_name(AuctionID).

get_info(AuctionID) ->
	gen_server:call(get_server_name(AuctionID), get_info).

get_status(AuctionID)->
	gen_server:call(get_server_name(AuctionID), get_status).

decrease_timer(ServerID) -> 
	gen_server:cast(ServerID, decrease_timer).

make_offer(AuctionID, Msg) -> 
	gen_server:call(get_server_name(AuctionID), {make_offer, Msg}).

finish_auction(ServerID) ->
	gen_server:cast(ServerID, finish).

stop_auction(AuctionID) ->
	gen_server:cast(get_server_name(AuctionID), stop).


%%Gen_server callback


init({AuctionID, GoodId, Duration, InitialPrice, Seller, Timer}) ->
	{
  		ok,
  		{AuctionID, GoodId, Duration, InitialPrice, InitialPrice, empty, Seller, in_auction, Timer}
  	}.


handle_call(get_info, _From, {AuctionID, GoodId, Duration, InitialPrice, CurrentPrice, CurrentWinner, Seller, Status, Timer}) ->
	{	
		reply,
		{AuctionID, GoodId, Duration, InitialPrice, CurrentPrice, CurrentWinner, Seller, Status},
		{AuctionID, GoodId, Duration, InitialPrice, CurrentPrice, CurrentWinner, Seller, Status, Timer}
	};


handle_call(get_status, _From, {AuctionID, GoodId, Duration, InitialPrice, CurrentPrice, CurrentWinner, Seller, Status, Timer}) ->
	{
		reply,
		{AuctionID, Status},
		{AuctionID, GoodId, Duration, InitialPrice, CurrentPrice, CurrentWinner, Seller, Status, Timer}
	};


handle_call({make_offer,_Msg}, _From, {AuctionID, GoodId, Duration, InitialPrice, CurrentPrice, CurrentWinner, Seller, finished, Timer}) ->
	{
		reply,
		false,
		{AuctionID, GoodId, Duration, InitialPrice, CurrentPrice, CurrentWinner, Seller, finished, Timer}
	};
handle_call({make_offer,{User,Price}}, _From, {AuctionID, GoodId, Duration, InitialPrice, CurrentPrice, CurrentWinner, Seller, in_auction, Timer}) ->
	UserInfo = mnesia_server:get_user(User),
	case element(5,UserInfo) >= Price of
		true->
			case Price > CurrentPrice of
		     true -> 
		     		case CurrentWinner =:= empty of
		     			true->
		     				mnesia_server:add_credit(User,-Price);
		     			false -> 
		     				mnesia_server:add_credit(User,-Price),
		     				mnesia_server:add_credit(CurrentWinner,CurrentPrice)
		     		end,
					case Duration < 120000 of
						true ->  
					     	{
		     					reply,
								true,
		     					{AuctionID, GoodId, 120000, InitialPrice, Price, User, Seller, in_auction, Timer}
		     				};
		     			false ->
		     				{
		     					reply,
								true,
		     					{AuctionID, GoodId, Duration, InitialPrice, Price, User, Seller, in_auction, Timer}
		     				}
		     		end;

		     false -> {
		     			reply,
		     			false,
		     			{AuctionID, GoodId, Duration, InitialPrice, CurrentPrice, CurrentWinner, Seller, in_auction, Timer}
		     		}
		   end;
		 false -> 
		 	{
		  		reply,
		   		false,
	     		{AuctionID, GoodId, Duration, InitialPrice, CurrentPrice, CurrentWinner, Seller, in_auction, Timer}
	  		}
	end.
%%Usare lo status per discriminare!

handle_cast(finish, {AuctionID, GoodId, _Duration, InitialPrice, CurrentPrice, CurrentWinner, Seller, _Status, Timer}) ->
	mnesia_server:finish_auction(AuctionID,CurrentPrice, CurrentWinner),
	auctions_manager:remove_auction(list_to_atom("auction" ++ integer_to_list(AuctionID))),
	{
		noreply,
		{AuctionID, GoodId, 0,InitialPrice, CurrentPrice, CurrentWinner, Seller, finished, Timer}
	};
handle_cast(decrease_timer, {AuctionID, GoodId, 0, InitialPrice, CurrentPrice, CurrentWinner, Seller, Status, Timer}) ->
	{
		noreply,
		{AuctionID, GoodId, 0,InitialPrice, CurrentPrice, CurrentWinner, Seller, Status, Timer}
	};
handle_cast(decrease_timer, {AuctionID, GoodId, Duration, InitialPrice, CurrentPrice, CurrentWinner, Seller, Status, Timer}) ->
	{
		noreply,
		{AuctionID, GoodId, Duration -1000,InitialPrice, CurrentPrice, CurrentWinner, Seller, Status, Timer}
	};
handle_cast(stop, {AuctionID, GoodId, _Duration, InitialPrice, CurrentPrice, CurrentWinner, Seller, _Status, Timer}) ->
	Timer ! stop,
 	{
 		noreply,
 		{AuctionID, GoodId, 0,InitialPrice, CurrentPrice, CurrentWinner, Seller, finished, Timer}
 	}.


%% UTILS

timer(0, AuctionID) -> finish_auction(AuctionID);
timer(Period, AuctionID) ->
 	sleep(1000),
 	decrease_timer(AuctionID),
 	receive
 		stop -> ok
 	after 0 ->
 		timer(Period-1000, AuctionID)
 	end.

sleep(Delay) ->
 	receive
 		after Delay -> ok
 	end.


get_server_name(AuctionID) ->
	list_to_atom("auction" ++ integer_to_list(AuctionID)).


%%
