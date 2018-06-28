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

size_of_area = 40
amoung_of_agent_monkey = 3
predators = 30
area_length = 4
len_of_table_symb = 10
len_of_word_lex = 6

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

lex__s :: Int -> IO [String]
lex__s a = do {rand_lis <- fx len_of_word_lex (97, 122); if a <= 0 then return []; else do {f <- lex__s (a - 1); return ((generate_ran_lex rand_lis) : f);}; }

monkey_generate_lex :: [String] -> [String] -> [String] -> Symb_lex
monkey_generate_lex lex lex_ lex__ = Symb_lex lex lex_ lex__

lis_lex :: Int -> IO [[String]]
lis_lex 0 = return []
lis_lex n = do
        lex__ <- lex__s len_of_table_symb
        f <- lis_lex (n - 1)
        return (lex__ : f)

monkey__l :: Int -> [[String]] -> [[String]] -> [[String]] -> [Monkey]
monkey__l 0 xs xt xe = []
monkey__l d xs xt xe = (Monkey d (monkey_generate_lex (head xs) (head xt) (head xe)) (Pos 0 0)) : (monkey__l (d - 1) (tail xs) (tail xt) (tail xe))

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
pred__ k m_ (x, y) = setElem (Floor 2 (Just (Agent_P (Predator (getPredatorType m_) (Pos x y))))) (x, y) k

floor_attr :: Matrix Floor -> (Int, Int) -> Int
floor_attr f_ p = get_att (f_ ! p)
get_att (Floor s d) = s
get_Pos (Monkey s k z) = z
get_PPos (Predator s k) = k
get_maybe_agents :: Floor -> Maybe Agents
get_maybe_agents(Floor s d) = d
getPredatorType(Predator x d) = x
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
put_in_map_floor (Floor_map a p_) [] = return (Floor_map a p_)
put_in_map_floor (Floor_map a p_) k = do rx <- rand__d (1, size_of_area)
                                         ry <- rand__d (1, size_of_area)
                                         d <- put_in_map_floor (Floor_map a p_) (tail k)
                                         if (floor_attr (getFloor d) (rx, ry)) /= 0
                                           then put_in_map_floor (Floor_map a p_) k
                                         else do 
                                           return (Floor_map (monkey___ (getFloor d) (head k) (rx, ry)) ((Pos rx ry) : (getPos_Agents d)))

putP_in_map_floor :: Floor_map -> [Predator] -> IO (Floor_map)
putP_in_map_floor (Floor_map a p_) [] = return (Floor_map a p_)
putP_in_map_floor (Floor_map a p_) k = do rx <- rand__d (1, size_of_area)
                                          ry <- rand__d (1, size_of_area)
                                          d <- putP_in_map_floor (Floor_map a p_) (tail k)
                                          if (floor_attr (getFloor d) (rx, ry)) /= 0
                                            then putP_in_map_floor (Floor_map a p_) k
                                           else do 
                                             return (Floor_map (pred__ (getFloor d) (head k) (rx, ry)) ((Pos rx ry) : (getPos_Agents d)))


monkey_xs :: Floor_map -> [Monkey]
monkey_xs xs
              | (length (getPos_Agents xs) ) == 0 = []
              | (get_att ((getFloor xs) ! (pos_to_tuple ((head (getPos_Agents xs)))))) == 1 = (get_maybe_monkeys (get_maybe_agents ((getFloor xs) ! (pos_to_tuple ((head (getPos_Agents xs))))))) :  (monkey_xs (Floor_map (getFloor xs) (tail (getPos_Agents xs))))
              | otherwise = (monkey_xs (Floor_map (getFloor xs) (tail (getPos_Agents xs))))

predator_xs :: Floor_map -> [Predator]
predator_xs xs
              | (length (getPos_Agents xs) ) == 0 = []
              | (get_att ((getFloor xs) ! (pos_to_tuple ((head (getPos_Agents xs)))))) == 2 = (get_maybe_predator (get_maybe_agents ((getFloor xs) ! (pos_to_tuple ((head (getPos_Agents xs))))))) :  (predator_xs (Floor_map (getFloor xs) (tail (getPos_Agents xs))))
              | otherwise = (predator_xs (Floor_map (getFloor xs) (tail (getPos_Agents xs))))
         
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

get_lex k = case (elemIndex (foldl1' max k) k) of Just x -> x

add_completixy_lex :: String -> String -> String
add_completixy_lex lex_ lex__ = (last lex__) : (init lex_)

lis_index :: Int -> [a] -> a
lis_index 0 n = (head n) -- REMEMBER THIS
lis_index y n = lis_index (y -1) (tail n) 

lexical__v :: [String] -> [String] -> Int -> Int -> [String]
lexical__v m k i i_ = setAt i (add_completixy_lex (lis_index i m) (lis_index i_ k)) m

add_lex :: Symb_lex -> Symb_lex -> Int -> Symb_lex
add_lex sl sl_ p = case p of
                             1 ->  (Symb_lex (lexical__v (snake sl) (snake sl_) (get_lex (lex_eq (snake sl) (snake sl_)))  (get_lex (lex_eq (snake sl_) (snake sl)))) (tiger sl) (eagle sl))
                             2 ->  (Symb_lex (snake sl) (lexical__v (tiger sl) (tiger sl_) (get_lex (lex_eq (tiger sl) (tiger sl_))) (get_lex (lex_eq (tiger sl_) (tiger sl)))) (eagle sl))
                             3 ->  (Symb_lex (snake sl) (tiger sl) (lexical__v (eagle sl) (eagle sl_) (get_lex (lex_eq (eagle sl) (eagle sl_))) (get_lex (lex_eq (eagle sl_) (eagle sl)))))

alert__monkey :: Monkey -> Monkey -> Predator -> Monkey
alert__monkey x k p = (Monkey (id_ x) (add_lex (lex_ x) (lex_ k) (getPredatorType p)) (pos x))

alert_in_area :: Matrix Floor -> Monkey -> [Monkey] -> Predator -> Matrix Floor
alert_in_area f x [] z = f
alert_in_area f x y z = monkey___ (alert_in_area f x (tail y) z) (alert__monkey (head y) x z) (pos_to_tuple $ get_Pos (head y))

check_area :: Floor_map -> Monkey -> [Predator] -> Floor_map
check_area f_ x [] = (Floor_map (getFloor f_) (getPos_Agents f_))

check_area f_ x p = (Floor_map (alert_in_area (getFloor (check_area f_ x (tail p))) x (getMonkey f_ (get_Pos x) area_length) (head p)) (getPos_Agents f_))

monkey_interact :: Floor_map -> Monkey -> Floor_map
monkey_interact k m = check_area k m (getPredator k (get_Pos m) area_length)

interact__ :: Floor_map -> [Monkey] -> Floor_map
interact__ k m
                         | (length m) <= 0 = k
                         | otherwise = monkey_interact (interact__ k (tail m)) (head m)

f :: Int -> Symb_lex -> Symb_lex-> Symb_lex -- Just for tests
f 0 x y = x
f n x y = add_lex (f (n - 1 ) x y) y 1

remove_monkey :: Floor_map -> Monkey -> Floor_map
remove_monkey f m = (Floor_map (setElem (Floor 0 Nothing) (pos_to_tuple (get_Pos m)) (getFloor f)) (getPos_Agents f))

remove_predator :: Floor_map -> Predator -> Floor_map
remove_predator f m = (Floor_map (setElem (Floor 0 Nothing) (pos_to_tuple (get_PPos m)) (getFloor f)) (getPos_Agents f))

remove_predator_of_map :: Floor_map -> [Predator] -> Floor_map
remove_predator_of_map f [] = f
remove_predator_of_map f k = remove_predator (remove_predator_of_map f (tail k)) (head k)

remove_monkey_of_map :: Floor_map -> [Monkey] -> Floor_map
remove_monkey_of_map f [] = f
remove_monkey_of_map f k = remove_monkey (remove_monkey_of_map f (tail k)) (head k)

update_monkey :: Floor_map -> [Monkey] -> IO Floor_map
update_monkey k m = do
                    let map__ = remove_monkey_of_map k m 
                    p <- put_in_map_floor map__ m
                    return p
                    
update_predator :: Floor_map -> [Predator] -> IO Floor_map
update_predator k m = do
                    let map__ = remove_predator_of_map k m 
                    p <- putP_in_map_floor map__ m
                    return p

create_simp :: IO Floor_map
create_simp = do
                           let map_floor = map__floor__
                           lis_posx <- fx amoung_of_agent_monkey (1, size_of_area)
                           lis_posy <- fx amoung_of_agent_monkey (1, size_of_area)
                           lis_p <- fx predators (1, 3)
                           lex__snake <- lis_lex amoung_of_agent_monkey
                           lex__tiger <- lis_lex amoung_of_agent_monkey
                           lex__eagle <- lis_lex amoung_of_agent_monkey
                           map_env <- put_in_map_floor map_floor (monkey__l amoung_of_agent_monkey lex__snake lex__tiger lex__eagle)
                           map_env <- putP_in_map_floor map_env (pred__l predators lis_p)
                           return map_env


simp ::  Floor_map -> IO Floor_map
simp map_env = do
 
                           let d = interact__ map_env (monkey_xs map_env)
                           let new_map = (Floor_map (getFloor d) [])
                           new_map <- update_monkey new_map (monkey_xs d)
                           new_map <- update_predator new_map (predator_xs map_env)
                           return new_map

ai :: Int -> IO [Floor_map]
ai 0 = do
          map_env <- create_simp
          return [map_env, map_env]
ai n = do
         ai___ <- ai (n - 1)
         simp__ <- simp (head ai___)
         return [simp__, (head (tail ai___))]

print_sym :: [String] -> [String] -> IO ()
print_sym [] [] = putStrLn ""
print_sym n n_ = do
                   putStr ("[" ++ ((head n) ++ " = " ++ (head n_)) ++ "]")
                   putStr " "
                   print_sym (tail n) (tail n_)

print_symb_lex :: [Monkey] -> [Monkey] -> IO ()
print_symb_lex [] [] = do
                    putStrLn ""

print_symb_lex m k = do
                   putStr "Monkey "
                   print (id_ (head m))
                   putStr "Snake Lex"
                   print_sym (snake (lex_ (head m))) (snake (lex_ (head k))) 
                   putStr "Tiger Lex Monkey "
                   print_sym (tiger (lex_ (head m))) (tiger (lex_ (head k)))
                   putStr "Eagle Lex Monkey "
                   print_sym (eagle (lex_ (head m))) (eagle (lex_ (head k)))
                   putStrLn ""
                   print_symb_lex (tail m) (tail k) 

main :: IO ()
main = do
    env <- ai 10000
    print "------------------------------------  -----------------------------"
    print_symb_lex (monkey_xs (head env)) (monkey_xs (head (tail env)))