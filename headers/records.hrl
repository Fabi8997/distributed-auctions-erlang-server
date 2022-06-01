-record(good, {goodId,
  name,
  description,
  user
}).

-record(auction, {auctionId,
  goodId,
  duration,
  initialPrice,
  currentPrice,
  currentWinner,
  seller,
  status
}).

-record(offer, {sender,
  receiver,
  value,
  timestamp,
  idAuction
}).

-record(user, {userId,
  username,
  password,
  credit
}).

-record(table_id, {
  table_name,
   last_id
}).