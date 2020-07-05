module Internal.BelieveNet where

import           Data.List
import           System.Random

type Bern = (String, Double)

type Factor = ([String], [[Bool]], [Double])

data Node = V_Node Int String [Int] | F_Node Int Factor [Int] | O_Node Int Bern [Int] deriving (Show)

data Dist = D [Double] | And | Or

type Graph = [Node]

eval_variables :: [String] -> [String] -> [Double] -> Graph -> IO()
eval_variables variables input point_mass graph = do
                let n_graph = set_input input point_mass graph
                let berns = map (\name -> eval_variable name n_graph) variables
                putStrLn $ "input " ++ (show input) ++ " " ++ (show point_mass)
                putStrLn $ "pediction " ++ (show berns)

eval_variable :: String-> Graph -> Bern
eval_variable name graph = get_message nr name node graph
                    where
                        nr = head $ get_ids [name] graph
                        node = graph !! nr

get_message :: Int  -> String -> Node -> Graph -> Bern
get_message _ _ (O_Node _ bern _ )  graph = bern
get_message sender name (V_Node nr n2 ts) graph = foldr1 mult_bern berns
                                                                    where
                                                                        n_ts = filter (/=sender) ts
                                                                        berns = map(\x -> get_message nr n2 (graph !! x) graph ) n_ts
get_message sender name (F_Node nr factor ts) graph = eval_factor name berns factor
                                                            where
                                                                n_ts = filter (/= sender) ts
                                                                berns = map(\x -> get_message nr name (graph !! x) graph ) n_ts


set_input :: [String] -> [Double] -> Graph -> Graph
set_input [] [] graph = graph
set_input (n:names) (v:values) graph = set_input names values n_graph
       where
           n_graph = map(add_input n v ) graph

add_input :: String -> Double  -> Node -> Node
add_input name v node@(V_Node nr na ts ) = if name == na
                                                                            then O_Node nr (name,v) ts
                                                                            else node
add_input name v node@(O_Node nr (na,_) ts ) = if name == na
                                                                            then O_Node nr (name,v) ts
                                                                            else node
add_input _ _ x = x

generate_d_graph :: [([String],Dist)] -> Graph
generate_d_graph entries = foldl (\x (a,b) -> add_d_nodes x a b) [] entries

add_d_nodes :: Graph -> [String] -> Dist-> Graph
add_d_nodes graph names values = up_graph ++ [F_Node act_idx factor ((act_idx+1):ids), V_Node (act_idx+1) name [act_idx] ]
        where
            act_idx = length graph
            factor = generate_d_factor names values
            up_graph = update_graph (init names) graph
            ids = get_ids (init names) graph
            name = last names

generate_graph :: [([String],[Double])] -> Graph
generate_graph entries = foldl (\x (a,b) -> add_nodes x a b) [] entries

add_nodes :: Graph -> [String] -> [Double]-> Graph
add_nodes graph names values = up_graph ++ [F_Node act_idx factor ((act_idx+1):ids), V_Node (act_idx+1) name [act_idx] ]
        where
            act_idx = length graph
            factor = generate_factor names values
            up_graph = update_graph (init names) graph
            ids = get_ids (init names) graph
            name = last names


get_ids :: [String]  -> Graph -> [Int]
get_ids names graph = concatMap(get_id names) graph

get_id :: [String] -> Node -> [Int]
get_id names (V_Node nr name _ ) = if(name `elem` names) then [nr] else []
get_id _ _                       = []


update_graph :: [String] -> Graph -> Graph
update_graph names ns =  map( add_entry idx names ) ns
                                where
                                    idx = length ns


add_entry :: Int -> [String] -> Node -> Node
add_entry idx names nodes@(V_Node nr name xs) = if (name `elem` names)
                                                                                     then V_Node nr  name (idx:xs)
                                                                                     else nodes
add_entry _ _  x = x

generate_d_factor :: [String] -> Dist -> Factor
generate_d_factor variables (D dist) = generate_factor variables dist
generate_d_factor variables And      = and_factor variables
generate_d_factor variables Or       = or_factor variables


generate_factor :: [String] -> [Double] -> Factor
generate_factor variables dist = (variables, generate_truth_table (length variables), dist)

and_factor :: [String] -> Factor
and_factor variables = (variables, tb, dist)
            where
                 tb' = generate_truth_table (length variables - 1)
                 last_col = map and  $ transpose tb'
                 tb = tb' ++ [last_col]
                 dist = map (\x -> 1.0) last_col

or_factor :: [String] -> Factor
or_factor variables = (variables, tb, dist)
            where
                 tb' = generate_truth_table (length variables - 1)
                 last_col = map or  $ transpose tb'
                 tb = tb' ++ [last_col]
                 dist = map (\x -> 1.0) last_col

mult_bern:: Bern -> Bern -> Bern
mult_bern (name, a) (_,b) = (name, a*b/(a*b + (1-a)*(1-b)))

eval_factor :: String -> [Bern] -> Factor -> Bern
eval_factor searched berns (ns, tb, values) = (searched, a/(a+b))
            where
                berns_v = map(\(name, value)  -> map(\x -> if x then value else (1-value)) (tb !! (find_idx name ns))) berns
                n_values = foldr (zipWith (*)) values berns_v
                xs = zip ( tb !! (find_idx searched ns)) n_values
                (a,b) = foldr (\(m,n) (k,l) -> if m then (k+n,l) else (k,l+n))  (0.0,0.0) xs

find_idx :: (Eq a ) =>  a -> [a] -> Int
find_idx x xs = find_idx' 0 x xs
        where
            find_idx' n x (y:ys) = if x == y then n else (find_idx' (n+1) x ys)

generate_truth_table :: Int -> [[Bool]]
generate_truth_table nr = map(generate_entries n) nums
    where
        n = 2^nr
        nums = reverse $ map (2^) [0..(nr-1)]
        generate_entries n num = take n (cycle ( (replicate num True) ++ (replicate num False )) )

draw_bern :: Bern -> IO Bool
draw_bern (_,p) = do
    d <- randomRIO(0::Double, 1)
    if (d < p ) then return True else return False




--Task 1
test_multiply_bernoulli :: IO()
test_multiply_bernoulli = do
    print $ mult_bern ("test",0.3) ("test",0.9)
    print $ mult_bern ("test",0.5) ("test",0.2)
    print $ mult_bern ("test",0.5) ("test",0.3)
    print $ mult_bern ("test",1) ("test",0.2)
    print $ mult_bern ("test",1) ("test",0.3)

test_bernouilli_properties:: IO()
test_bernouilli_properties = do
    first_values <- sequence $ replicate 10000000 (draw_bern ("test",0.5))
    second_values <- sequence $ replicate 10000000 (draw_bern ("test",0.5))
    let trues =   length $ filter (\(a,b) -> a == True && b == True) ( zip first_values second_values)
    let falses = length $ filter (\(a,b) -> a == False && b == False) ( zip first_values second_values)
    print $ fromIntegral trues/ ( fromIntegral (trues + falses) )
    print $ mult_bern ("test",0.3) ("test",0.3)


--Task 3
test_example :: IO()
test_example = do
       let graph = generate_d_graph[(["c#"],  D [0.5, 0.5]),
                                                      (["sql"],  D  [0.5,0.5]),
                                                      (["c#","c1"], D [0.9,0.1,0.2,0.8]),
                                                      ( ["sql","c2"], D  [0.9,0.1,0.2,0.8]),
                                                      (["c#","sql","hs"], And),
                                                      (["hs","c3"], D [0.9,0.1,0.2,0.8])]
       let inputs = [[x,y,z]| x <-[0::Double,1], y <-[0,1], z <-[0,1] ]
       sequence_ $ map (\input -> eval_variables ["c#","sql","hs"] ["c1","c2","c3"] input graph) inputs

test_example2 :: IO()
test_example2 = do
       let graph = generate_d_graph [(["sI"], D [0.3, 0.7]),
                                                      (["sI","vT"], D [0.9,0.1,0.01,0.99]),
                                                      (["sI","ccd"], D [0.99,0.01,0.1,0.9]),
                                                      ( ["sI","tb"], D [0.7,0.3,0.1,0.9]),
                                                      (["sI","td"],  D [0.99,0.01,0.001,0.999]),
                                                      (["sI","pl"], D [0.9,0.1,0.01,0.99])]
       let inputs = [[x,y,z,k,l]| x <-[0::Double,1], y <-[0,1], z <-[0,1], k <- [0,1], l <- [0,1] ]
       sequence_ $ map (\input -> eval_variables ["sI"] ["vT","ccd","tb","td","pl"] input graph) inputs
