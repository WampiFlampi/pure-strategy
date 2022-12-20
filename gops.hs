-- State monad comes from the book Programming in Haskell by Graham Hutton
type State = Game
newtype ST a = S(State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) s = st s 

instance Functor ST where
  fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
  pure x = S (\s -> (x,s))

  stf <*> stx = S (\s ->  
    let (f, s')  = app stf s
        (x, s'') = app stx s' in (f x, s''))

instance Monad ST where
  return x = S (\s -> (x, s)) 
  st >>= f = S (\s ->  
                   let (x, s') = app st s
                   in  app (f x) s')


type Cards = [Char] 
type Player = (Cards, Cards, Cards)
type Game = (Player, Player, Cards)
type Result = ((Cards, Cards, Int), (Cards, Cards, Int))
type Strat = Player -> Char

hand :: Cards 
hand = ['A', '2', '3', '4', '5', '6', '7', '8', '9','D','J','Q','K']

--Players have 3 strings, the cards in the order they are played, available cards to be played
-- ...and cards won throughout the game
--game includes two Players, and a extra list to keep track of tied cards
game :: Game
game = (([], hand, []), ([], hand, []), [])

--examples of some simple strategies 
f2l :: Strat
f2l (_ , (x : xs), _) = x
f2l (_, _, _) = 'q'

l2f :: Strat
l2f (_, xs, _) | xs == []  = 'q'
               | otherwise = head $ reverse xs

low2high :: Strat
low2high (_, [], _)  = 'q'
low2high (ds, hs, _) | ds == []        = f2l ([],hs,[])
                     | last ds >= '7'  = f2l ([], hs, [])
                     | otherwise       = l2f ([], hs, [])

cord :: Char -> Char -> Ordering
cord c1 c2 | c1 == c2  = EQ
           | c1 > c2   = GT
           | otherwise = LT

--plays a round of the game
evalr :: Game -> Strat -> Strat -> Game
evalr g str str' = case (cord card1 card2) of
                EQ -> ((ds1 ++ [card1], hs1', gs1), (ds2 ++ [card2], hs2', gs2), c ++ [card1, card2])
                GT -> ((ds1 ++ [card1], hs1', gs1 ++ [card1, card2] ++ c), (ds2 ++ [card2], hs2', gs2), [])
                LT -> ((ds1 ++ [card1], hs1', gs1), (ds2 ++ [card2], hs2', gs2 ++ [card1, card2] ++ c), [])
             where
              (p1, p2, c) = g
              card1 = str p1
              card2 = str' p2 
              (ds1, hs1, gs1) = p1
              (ds2, hs2, gs2) = p2
              hs1' = filter (/= card1) hs1
              hs2' = filter (/= card2) hs2

--passes the game through using the State monad
gamet f g = S (\s -> (s, evalr s f g))

--plays the full game
gops :: Strat -> Strat -> ST Game
gops f g = do
       (p1, p2, c) <- gamet f g
       let (_,hs,_) = p1
       if hs == []
       then do
        return (p1, p2, c)
       else gops f g 

--Returns the result of game between strategies
gres :: Strat -> Strat -> Result
gres f g = ((disc1, win1, length win1), (disc2, win2, length win2))
        where 
          gg = fst $ app (gops f g) game
          ((disc1, _, win1), (disc2, _, win2), _) = gg
          
