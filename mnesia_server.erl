%%%-------------------------------------------------------------------
%%% @author Stefano
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. apr 2022 16:55
%%%-------------------------------------------------------------------
-module(mnesia_server).

%% API
-export([init/0, login/2, register/3, all_user/0,
  add_user/3, update_auction/3, update_good/2,
  add_credit/2, add_auction/7, add_good/3, is_auction_present/1, is_good_present/1,
  is_user_present/1, finish_auction/3, get_user_goods/1, start_all_counters/0, start_counter/1,
  get_all_counters/0, empty_all_tables/0, get_credit/1, get_good/1, get_user/1, in_auction/1,
  all_auctions/1, get_auction_from_good/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("headers/records.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  case mnesia:wait_for_tables([user, auction, good, offer, table_id], 5000) == ok of
    true ->
      ok;
    false ->
      mnesia:create_table(user,
        [{attributes, record_info(fields, user)},
          {disc_copies, [node()]}
        ]),
	  mnesia:create_table(auction,
        [{attributes, record_info(fields, auction)},
          {disc_copies, [node()]}
        ]),
      mnesia:create_table(good,
        [{attributes, record_info(fields, good)},
          {disc_copies, [node()]}
        ]),		
      mnesia:create_table(offer,
        [{attributes, record_info(fields, offer)},
          {type, bag},
          {disc_copies, [node()]}
        ]),
      mnesia:create_table(table_id,
        [{attributes, record_info(fields, table_id)},
          {disc_copies, [node()]}
        ])
  end.

  start_counter(TableName) ->
    Fun = fun() ->
      mnesia:write(table_id,
        #table_id{table_name=TableName, 
        last_id=0
      } )
          end,
    mnesia:transaction(Fun).

    start_all_counters() -> 
      Fun = fun() ->
      mnesia:write(#table_id{table_name=user, 
        last_id=0
      }),
      mnesia:write(#table_id{table_name=good, 
        last_id=0
      }),
      mnesia:write(#table_id{table_name=auction, 
        last_id=0
      }),
      mnesia:write(#table_id{table_name=offer, 
        last_id=0
      })
          end,
    mnesia:activity(transaction,Fun).

  
  empty_all_tables() ->
      mnesia:clear_table(user),
      mnesia:clear_table(auction),
      mnesia:clear_table(good),
      mnesia:clear_table(table_id),
      mnesia:clear_table(offer).

get_all_counters() ->
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(table_id)]),
    qlc:e(Q)
      end,
  mnesia:transaction(F).


%%%===================================================================
%%% USER OPERATIONS
%%%===================================================================

add_user(Username, Password, Credit) ->
  Index = mnesia:dirty_update_counter(table_id, user, 1),
  Fun = fun() ->
    mnesia:write(#user{userId = Index,
      username = Username,
      password = Password,
	  credit = Credit
    })
        end,
  mnesia:activity(transaction, Fun).
  
register(Username, Password, Credit) ->
  F = fun() ->
    case is_user_present(Username) of
      false -> % User not present
        add_user(Username, Password, Credit),
        true;
      true ->
        false
    end
      end,
  mnesia:activity(transaction, F).

login(Username, Password) ->
  F = fun() ->
    case is_user_present(Username) of
      true -> % User present
        %% GET PASSWORD
        Q = qlc:q([E || E <- mnesia:table(user),E#user.username == Username]),
        [{user, _, Username, Pass, _}] = qlc:e(Q),

        %% CHECK IF THE PASSWORD IS CORRECT
        case Password =:= Pass of
          true ->
            true;
          false -> 
            false
        end;
      false ->
        false
    end
      end,
  mnesia:activity(transaction, F).

add_credit(Username, Credit) ->
  F = fun() ->
    case is_user_present(Username) of
      true -> % User present
        %% If the user is present I add the credit
        update_credit(Username, Credit),
        true;
      false ->
        false
    end
      end,
  mnesia:activity(transaction, F).


update_credit(Username, NewCredit) ->
  F = fun() ->
        Q = qlc:q([E || E <- mnesia:table(user),E#user.username == Username]),
        [User] = qlc:e(Q),
        [{_,_,Username,_,Credit}] = [User],
    mnesia:write(User#user{credit = Credit + NewCredit})
      end,
  mnesia:activity(transaction, F).

is_user_present(Username) ->
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(user),E#user.username == Username]),
    case qlc:e(Q) =:= [] of
      true ->
        false;
      false ->
        true
    end
      end,
   {atomic,Res} = mnesia:transaction(F),
   Res.


all_user() ->
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(user)]),
    qlc:e(Q)
      end,
  mnesia:transaction(F).

get_credit(Username) -> 
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(user),E#user.username == Username]),
        [{user, _, _, _, Credit}] = qlc:e(Q),
        Credit
  end,
  {atomic, Res} = mnesia:transaction(F),
  Res.

get_user(Username) -> 
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(user),E#user.username == Username]),
        [User] = qlc:e(Q),
        User
  end,
  {atomic, Res} = mnesia:transaction(F),
  Res.
  
%%%===================================================================
%%% AUCTION OPERATIONS
%%%===================================================================
  
add_auction(GoodId, Duration, InitialPrice, CurrentPrice, CurrentWinner, Seller, Status) ->
  Index = mnesia:dirty_update_counter(table_id, auction, 1),
  Fun = fun() ->
    mnesia:write(#auction{auctionId = Index,
      goodId = GoodId,
      duration = Duration,
      initialPrice = InitialPrice,
      currentPrice = CurrentPrice,
      currentWinner = CurrentWinner,
      seller = Seller,
      status = Status
    })
        end,
  mnesia:activity(transaction, Fun),
  Index.

update_auction(AuctionId, CurrentPrice, CurrentWinner) ->
  F = fun() ->
    [Auction] = mnesia:read(auction, AuctionId),
    mnesia:write(Auction#auction{currentWinner = CurrentWinner, currentPrice = CurrentPrice})
      end,
  mnesia:activity(transaction, F).

finish_auction(AuctionId, FinalPrice, Winner) ->
  F = fun() ->
    [Auction] = mnesia:read(auction, AuctionId),
    case Winner =:= empty of 
      true -> 
        mnesia:write(Auction#auction{status = finished});
      false -> 
        update_good(element(3,Auction),Winner),
        add_credit(element(8,Auction),FinalPrice),
        mnesia:write(Auction#auction{status = finished, currentPrice = FinalPrice, currentWinner = Winner }) 
    end
  end,
  mnesia:activity(transaction, F).

is_auction_present(AuctionId) ->
  F = fun() ->
    case mnesia:read({auction, AuctionId}) =:= [] of
      true ->
        false;
      false ->
        true
    end
      end,
  mnesia:activity(transaction, F).

in_auction(GoodId) -> 
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(auction),E#auction.goodId == GoodId, E#auction.status == in_auction]),
    case qlc:e(Q) =:= [] of
      true ->
        false;
      false ->
        true
    end
      end,
   {atomic,Res} = mnesia:transaction(F),
   Res.
  
get_auction_from_good(GoodId) ->
  F = fun() ->
    Q = qlc:q([E#auction.auctionId || E <- mnesia:table(auction),E#auction.goodId == GoodId, E#auction.status == in_auction]),
    [AuctionId] = qlc:e(Q),
    AuctionId
  end,
  {atomic,Res} = mnesia:transaction(F),
  Res.

all_auctions(User) ->
  F = fun() ->
    Q = qlc:q([E || E <- mnesia:table(auction),E#auction.seller =/= User, E#auction.status == in_auction]),
    qlc:e(Q)
      end,
  {atomic, Res} = mnesia:transaction(F),
  Res.
  
%%%===================================================================
%%% GOOD OPERATIONS
%%%===================================================================
  
add_good(Name, Description, User) ->
  Index = mnesia:dirty_update_counter(table_id, good, 1),
  Fun = fun() ->
    mnesia:write(#good{goodId = Index,
	  name = Name,
    description = Description,
	  user = User
    })
        end,
  mnesia:activity(transaction, Fun).

update_good(GoodId, User) ->
  F = fun() ->
    [Good] = mnesia:read(good, GoodId),
    mnesia:write(Good#good{user = User})
      end,
  mnesia:activity(transaction, F).

is_good_present(GoodId) ->
  F = fun() ->
    case mnesia:read({good, GoodId}) =:= [] of
      true ->
        false;
      false ->
        true
    end
      end,
  mnesia:activity(transaction, F).

get_user_goods(User) ->
  F = fun() ->
    mnesia:match_object(good, {good,'_','_','_',User}, read)  
    end,
  mnesia:activity(transaction, F).

get_good(GoodId) ->
  F = fun() ->
    case is_good_present(GoodId) of
      true ->
        [Good] = mnesia:read({good, GoodId}),
        Good;
      false ->
        false
    end
  end,
  mnesia:activity(transaction, F).

  
