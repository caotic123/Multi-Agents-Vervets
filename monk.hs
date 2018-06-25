import System.Random
import Data.Char
import Data.Matrix
import Data.List
import Data.List.Index

data Pos = Pos{x :: Int, y :: Int} deriving Show
data Predator = Predator Int Pos deriving Show
data Symb_lex = Symb_lex {snake :: [String], tiger :: [String], eagle :: [String]} deriving Show
data Monkey = Monkey {id_ :: Int, lex_ :: Symb_lex, pos :: Pos} deriving Show
data Agents = Agent_M Monkey | Agent_P Predator deriving Show
data Floor = Floor Int (Maybe Agents) deriving Show
data Floor_map = Floor_map (Matrix Floor) [Pos] deriving Show

size_of_area = 23
amoung_of_agent_monkey = 3
predators = 20
area_length = 6

rand__d :: (Int, Int) -> IO Int
rand__d i = do
   rx <- getStdGen
   let (x, ry) = randomR i rx
   setStdGen ry
   return x

fx :: Int -> (Int, Int) -> IO [Int]
fx 0 p = return []
fx n p = do
   t <- rand__d p
   n <- fx (n - 1) p
   return (t : n)

generate_ran_lex :: [Int] -> [Char]
generate_ran_lex  [] = []
generate_ran_lex  k = (chr (head k) : generate_ran_lex (tail k))

monkey_lex = do
  rand_lis <- fx 10 (97, 122)
  return (generate_ran_lex  rand_lis)

lex__s :: Int -> IO [String]
lex__s a = do {d <- monkey_lex; if a <= 0 then return []; else do {f <- lex__s (a - 1); return (d : f);}; }

monkey_generate_lex :: [String] -> Symb_lex
monkey_generate_lex lex = Symb_lex lex lex lex

monkey__l :: Int -> Symb_lex -> [Monkey]
monkey__l 0 monkey_lex = []
monkey__l x___ monkey_lex = (Monkey x___ monkey_lex (Pos 0 0)) : monkey__l (x___ - 1) monkey_lex

pred__l :: Int -> [Int] -> [Predator]
pred__l 0 p = []
pred__l x p = (Predator (head p) (Pos 0 0)) : pred__l (x - 1) (tail p)

floor__ :: Matrix Floor
floor__ = matrix size_of_area size_of_area (\i -> Floor 0 Nothing)

map__floor__ :: Floor_map
map__floor__  = (Floor_map floor__ [])

monkey___ :: Matrix Floor -> Monkey -> (Int, Int) -> Matrix Floor
monkey___ k m_ (x, y) = setElem (Floor 1 (Just (Agent_M (Monkey (id_ m_) (lex_ m_) (Pos x y))))) (x, y) k
pred__ :: Matrix Floor -> Predator-> (Int, Int) -> Matrix Floor
pred__ k m_ pp = setElem (Floor 2 (Just (Agent_P m_))) pp k

floor_attr :: Matrix Floor -> (Int, Int) -> Int
floor_attr f_ p = get_att (f_ ! p)
get_att (Floor s d) = s
get_maybe_agents :: Floor -> Maybe Agents
get_maybe_agents(Floor s d) = d
get__ :: Agents -> Monkey
get__(Agent_M x) = x
get___ :: Agents -> Predator
get___(Agent_P x) = x
get_maybe_monkeys :: Maybe Agents -> Monkey
get_maybe_monkeys (Just x) = get__ x
get_maybe_predator (Just x) = get___ x

getFloor(Floor_map f pos_) = f
getPos_Agents(Floor_map f m_pos) = m_pos
pos_to_tuple :: Pos -> (Int, Int)
pos_to_tuple (Pos x y) = (x, y)
getMonkeyByPos :: Matrix Floor -> Pos -> Monkey
getMonkeyByPos k (Pos x y) =  get_maybe_monkeys $ get_maybe_agents (k ! (x, y))

put_in_map_floor :: Floor_map -> [Monkey] -> IO (Floor_map)
put_in_map_floor (Floor_map a p_) k = do rx <- rand__d (1, size_of_area)
                                         ry <- rand__d (1, size_of_area)
                                         if (length k) <= 0
                                           then return (Floor_map a p_)
                                         else
                                           if (floor_attr a (rx, ry)) /= 0
                                             then put_in_map_floor (Floor_map a p_) k
                                           else do 
                                             d <- put_in_map_floor (Floor_map a p_) (tail k)
                                             return (Floor_map (monkey___ (getFloor d) (head k) (rx, ry)) ((Pos rx ry) : (getPos_Agents d)))

putP_in_map_floor :: Floor_map -> [Predator] -> IO (Floor_map)
putP_in_map_floor (Floor_map a p_) k = do rx <- rand__d (1, size_of_area)
                                          ry <- rand__d (1, size_of_area)
                                          if (length k) <= 0
                                            then return (Floor_map a p_)
                                          else
                                            if (floor_attr a (rx, ry)) /= 0
                                              then putP_in_map_floor (Floor_map a p_) k
                                             else do 
                                               d <- putP_in_map_floor (Floor_map a p_) (tail k)
                                               return (Floor_map (pred__ (getFloor d) (head k) (rx, ry)) ((Pos rx ry) : (getPos_Agents d)))


monkey_xs :: Floor_map -> [Monkey]
monkey_xs xs
              | (length (getPos_Agents xs) ) == 0 = []
              | (get_att ((getFloor xs) ! (pos_to_tuple ((head (getPos_Agents xs)))))) == 1 = (get_maybe_monkeys (get_maybe_agents ((getFloor xs) ! (pos_to_tuple ((head (getPos_Agents xs))))))) :  (monkey_xs (Floor_map (getFloor xs) (tail (getPos_Agents xs))))
              | otherwise = (monkey_xs (Floor_map (getFloor xs) (tail (getPos_Agents xs))))

max_or_min_area :: Int -> Int
max_or_min_area x
                   | x > size_of_area = size_of_area
                   | x <= 0 = 1
                   | otherwise = x

get_agents_area :: Matrix Floor -> Pos -> Int -> [Floor]
get_agents_area f (Pos x y) area_len = toList (submatrix (max_or_min_area (x - (div area_len 2))) (max_or_min_area (x + ((*) area_len 2))) (max_or_min_area (y - (div area_len 2))) (max_or_min_area (y + ((*) area_len 2))) f)

getMonkeys_floor :: [Floor] -> [Monkey]
getMonkeys_floor [] = []
getMonkeys_floor xs = if ((get_att (head xs)) == 1) then (get_maybe_monkeys (get_maybe_agents (head xs))) : (getMonkeys_floor (tail xs)) else (getMonkeys_floor (tail xs))

getPredator_floor :: [Floor] -> [Predator]
getPredator_floor [] = []
getPredator_floor xs = if ((get_att (head xs)) == 2) then (get_maybe_predator (get_maybe_agents (head xs))) : (getPredator_floor (tail xs)) else (getPredator_floor (tail xs))

getPredator map_env x n = getPredator_floor (get_agents_area (getFloor map_env) x n)
getMonkey map_env x n = getMonkeys_floor (get_agents_area (getFloor map_env) x n)

lex_t :: String -> String -> Int
lex_t lex_ "" = 0
lex_t lex_ lex__ = (lex_s (head lex__) lex_) + (lex_t lex_ (tail lex__))
                   where lex_s str "" = 0
                         lex_s str xs = (lex_s str (tail xs)) + (if (head xs) == str then 1 else 0)
 
lex_eq :: [String] -> [String] -> [Int]
lex_eq [] [] = []
lex_eq lex_ n = (lex_t (head lex_) (head  n)) : (lex_eq (tail lex_) (tail n))

get_lex k = (elemIndex (foldl1' max k) k)

add_completixy_lex :: String -> String -> String
add_completixy_lex lex_ lex__ = (head lex__) : (tail lex_)

lis_index :: Int -> [a] -> a
lis_index 1 n = (head n)
lis_index y n = lis_index (y -1) (tail n) 

lexical__v :: [String] -> [String] -> Int -> Int -> [String]
lexical__v m k i i_ = setAt i (add_completixy_lex (lis_index i m) (lis_index i_ k )) m

check_area :: Matrix Floor -> Monkey -> Pos -> [Predator] -> Matrix Floor
check_area f_ x k [] = f_
check_area f_ x k p =  f_

interact__ :: Floor_map -> Floor_map
interact__ k
                         | (length (getPos_Agents k)) <= 0 = k
                         | otherwise = Floor_map (check_area (getFloor k) (getMonkeyByPos (getFloor k) (head (getPos_Agents k))) (head (getPos_Agents k)) (getPredator k (head (getPos_Agents k)) area_length)) (tail (getPos_Agents k))
main :: IO ()
main = do
   lis_posx <- fx amoung_of_agent_monkey (1, size_of_area)
   lis_posy <- fx amoung_of_agent_monkey (1, size_of_area)
   lis_p <- fx predators (1, 3)
   x <- monkey_lex
   lex__ <- lex__s amoung_of_agent_monkey
   let monkeys = monkey__l amoung_of_agent_monkey (monkey_generate_lex lex__)
   let predator_ = pred__l predators lis_p
   let map_floor = map__floor__
   map_env <- put_in_map_floor map_floor monkeys
   map_env <- putP_in_map_floor map_env predator_
   let t = [1, 2, 32, 5]
   print $ lis_index 3 t