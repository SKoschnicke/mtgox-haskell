module Data.OrderBook (
      OrderBook(..)
    , updateOrderBook
    )
    where

import Text.PrettyPrint.Boxes

import Data.Mtgox

-- | Inserts (key, value) pairs into an ordered list.
insertWith :: (k -> k -> Ordering)      -- ^ comparision function.
                -> ((k, v) -> [(k, v)]) -- ^ update function, needs to preserve order, i.e x < y => f x < f y.
                -> ((k, v) -> [(k, v)]) -- ^ insert function.
                -> (k, v)               -- ^ element to insert.
                -> [(k, v)]             -- ^ ordered list.
                -> [(k, v)]
insertWith _ _ insert x [] = insert x
insertWith comp update insert (x,a) ys@((y, b) : [])= case comp x y of
                                    LT -> insert (x,a) ++ ys
                                    GT -> (y, b) : insert (x, a) 
                                    EQ -> update (y, b)
insertWith comp update insert (x,a) xs = 
    let n = length xs `div` 2
        (l,r) = splitAt n xs
        (p, c) = xs !! n
    in
        case comp x p of
        LT -> insertWith comp update insert (x,a) l ++ r
        GT -> l ++ insertWith comp update insert (x,a) r
        EQ -> l ++ update (p,c) ++ tail r

data OrderBook = OrderBook { bids :: [(Integer, Integer)], asks :: [(Integer, Integer)]}

instance Show OrderBook where
    show ob = 
        let     
            h caption position xs = vcat left $ map text $ caption : map (show . position) (take 10 xs)
            bid_box = h "bids:" fst (bids ob)
            bid_vol_box = h "vol:" snd (bids ob)
            ask_box = h "asks:" fst (asks ob)
            ask_vol_box = h "vol:" snd (asks ob)
        in
            render $ hsep 4 top [bid_vol_box, bid_box, ask_box, ask_vol_box]

updateOrderBook :: DepthMsg -> OrderBook -> OrderBook
updateOrderBook d ob | type_str d == Bid = 
    ob {bids = insertWith comp update insert (dPrice_int d, total_volume_int d) (bids ob)}
    where
        comp p1 p2 = invertOrdering $ compare p1 p2
        update (p, _) | volume_int d == 0 = [(p, total_volume_int d)]
        update (p, v) = let new_volume = v + volume_int d in
                          if new_volume == 0 then [] else [(p, new_volume)]
        insert (p, v) = if v <= 0 then [] else [(p, v)]
updateOrderBook d ob | type_str d == Ask = 
    ob {asks = insertWith comp update insert (dPrice_int d, total_volume_int d) (asks ob)}
    where
        comp = compare
        update (p, _) | volume_int d == 0 = [(p, total_volume_int d)]
        update (p, v) = let new_volume = v + volume_int d in
                          if new_volume == 0 then [] else [(p, new_volume)] 
        insert (p, v) = if v <= 0 then [] else [(p, v)]
updateOrderBook _ _ | otherwise = error "unknown trade type." 

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT
