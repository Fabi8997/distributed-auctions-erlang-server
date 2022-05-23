-module(auctions_manager).
-export([init/1, handle_call/3, handle_cast/2]).
-export([start_server/0, get_auctions/0, reset/0, start_new_auction/1, remove_auction/1]).
-behavior(gen_server).

%%API

start_server() ->
	gen_server:start({local,auctions_manager},?MODULE, [], []).

get_auctions() ->
	gen_server:call(auctions_manager, get_auctions).

start_new_auction(AuctionInitialState) ->
	gen_server:call(auctions_manager, {new_auction, AuctionInitialState}).

remove_auction(AuctionId) -> 
	gen_server:cast(auctions_manager, {remove_auction, AuctionId}).

reset() ->
	gen_server:cast(auctions_manager, reset).

%%CALLBACK

init(_InitialState) ->
  	{
  		ok,
  		[]
  	}.


handle_call({new_auction, {GoodId,Duration,InitialPrice,Seller}}, _From, Auctions) ->
	NewAuctionAdd = auction_server:start_auction(GoodId,Duration,InitialPrice,Seller),
	{
		reply,
		NewAuctionAdd,
		[NewAuctionAdd | Auctions]
	};

handle_call(get_auctions, _From, Auctions) ->
	{
		reply,
		Auctions,
		Auctions
	}.

handle_cast({remove_auction, AuctionId}, Auctions) ->
	{
		noreply,
		lists:delete(AuctionId,Auctions)
	};
handle_cast(reset, _Status) ->
	{
		noreply,
		[]
	}.

%%ADD terminate both here and in the other file