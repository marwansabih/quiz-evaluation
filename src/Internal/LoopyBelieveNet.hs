module Internal.LoopyBelieveNet where

import           Data.List
import           System.Random

type Bern = (String, Double)

type Factor = ([String], [[Bool]], [Double])

data Node = V_Node Int String [Int] (Int, Double) | F_Node Int Factor [Int]  | O_Node Int Bern [Int] deriving (Show)

data Dist = D [Double] | And | Or deriving Show

type Graph = [Node]

evalVariablesInputs :: [String] -> [String] -> [[Double]] -> Graph -> IO()
evalVariablesInputs variables input observed graph = mapM_ f observed
    where 
        f x = eval_variables variables input x graph


eval_variables :: [String] -> [String] -> [Double] -> Graph -> IO()
eval_variables variables input point_mass graph = do
                let n_graph = set_input input point_mass graph
                let berns = map (\name -> eval_variable name n_graph) variables
                putStrLn $ "input " ++ (show input) ++ " " ++ (show point_mass)
                putStrLn $ "pediction " ++ (show berns)

sample_variables :: [String] -> [String] -> [Double] -> Graph -> [Bern]
sample_variables variables input point_mass graph = berns
            where
                n_graph = set_input input point_mass graph
                berns = map (\name -> eval_variable name n_graph) variables

eval_variable :: String-> Graph -> Bern
eval_variable name graph = snd $ get_message nr name node graph
                    where
                        nr = head $ get_ids [name] graph
                        node = graph !! nr

get_message :: Int  -> String -> Node -> Graph -> (Graph,Bern)
get_message _ _ (O_Node _ bern _ )  graph = (graph,bern)
get_message sender name (V_Node nr n2 ts (-1,v)) graph = (graph, foldr1 mult_bern berns)
                                                                    where
                                                                        n_ts = filter (/=sender) ts
                                                                        node = (V_Node nr n2 ts (sender,v))
                                                                        n_graph = replace_node node graph
                                                                        berns = map(  \x -> let
                                                                                                          (gr, br) = get_message nr n2 (graph !! x) n_graph
                                                                                                          (V_Node nr' n2' ts' (idx, v') ) =  gr !! nr
                                                                                                        in
                                                                                                         if idx == -2
                                                                                                         then
                                                                                                             if  v == v--abs( v - v') <= 1
                                                                                                                 then  (n2,v')
                                                                                                             else
                                                                                                                        let  n_graph' = replace_node (V_Node nr n2 ts (-1,v')) graph
                                                                                                                        in  snd  $ get_message nr n2 (graph !! x) n_graph'
                                                                                                          else br
                                                                                            ) n_ts
get_message sender name (V_Node nr n2 ts (idx,v)) graph = (n_graph, (n2,v))
                                                            where
                                                                 n_graph = replace_node (V_Node nr n2 ts (-2,v)) graph
get_message sender name (F_Node nr factor ts) graph = (graph, eval_factor name berns factor)
                                                            where
                                                                n_ts = filter (/= sender) ts
                                                                berns = map( snd .(\x -> get_message nr name (graph !! x) graph )) n_ts



replace_node :: Node -> Graph -> Graph
replace_node node@(V_Node nr _ _ _ ) graph = xs ++ [node] ++  tail ys
                                   where
                                      (xs,ys) = splitAt nr graph
replace_node _ graph = graph

set_input :: [String] -> [Double] -> Graph -> Graph
set_input [] [] graph = graph
set_input (n:names) (v:values) graph = set_input names values n_graph
       where
           n_graph = map(add_input n v ) graph

add_input :: String -> Double  -> Node -> Node
add_input name v node@(V_Node nr na ts bern ) = if name == na
                                                                            then O_Node nr (name,v) ts
                                                                            else node
add_input name v node@(O_Node nr (na,_) ts ) = if name == na
                                                                            then O_Node nr (name,v) ts
                                                                            else node
add_input _ _ x = x

generate_d_graph :: [([String],Dist)] -> Graph
generate_d_graph entries = foldl (\x (a,b) -> add_d_nodes x a b) [] entries

add_d_nodes :: Graph -> [String] -> Dist-> Graph
add_d_nodes graph names values = up_graph ++ [F_Node act_idx factor ((act_idx+1):ids), V_Node (act_idx+1) name [act_idx] (-1, 0.5) ]
        where
            act_idx = length graph
            factor = generate_d_factor names values
            up_graph = update_graph (init names) graph
            ids = get_ids (init names) graph
            name = last names

generate_graph :: [([String],[Double])] -> Graph
generate_graph entries = foldl (\x (a,b) -> add_nodes x a b) [] entries

add_nodes :: Graph -> [String] -> [Double]-> Graph
add_nodes graph names values = up_graph ++ [F_Node act_idx factor ((act_idx+1):ids), V_Node (act_idx+1) name [act_idx] (-1, 0.5) ]
        where
            act_idx = length graph
            factor = generate_factor names values
            up_graph = update_graph (init names) graph
            ids = get_ids (init names) graph
            name = last names


get_ids :: [String]  -> Graph -> [Int]
get_ids names graph = concatMap(get_id names) graph

get_id :: [String] -> Node -> [Int]
get_id names (V_Node nr name _ _ ) = if(name `elem` names) then [nr] else []
get_id _ _                         = []


update_graph :: [String] -> Graph -> Graph
update_graph names ns =  map( add_entry idx names ) ns
                                where
                                    idx = length ns


add_entry :: Int -> [String] -> Node -> Node
add_entry idx names nodes@(V_Node nr name xs bern) = if (name `elem` names)
                                                                                     then V_Node nr  name (idx:xs) bern
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
    first_values <- sequence $ replicate 100 (draw_bern ("test",0.5))
    second_values <- sequence $ replicate 100 (draw_bern ("test",0.5))
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

test_replace_node :: IO()
test_replace_node = do
    let graph = generate_d_graph[(["c#"],  D [0.5, 0.5]),
                                                   (["sql"],  D  [0.5,0.5]),
                                                   (["c#","c1"], D [0.9,0.1,0.2,0.8]),
                                                   ( ["sql","c2"], D  [0.9,0.1,0.2,0.8]),
                                                   (["c#","sql","hs"], And),
                                                   (["hs","c3"], D [0.9,0.1,0.2,0.8])]
    print graph
    let node = V_Node 2 "test" [] (-1, 10.0)
    print $ replace_node node graph


test_example2 :: IO()
test_example2 = do
       let graph = generate_d_graph [(["sI"], D [0.3, 0.7]),
                                                      (["sI","vT"], D [0.9,0.1,0.01,0.99]),
                                                      (["sI","ccd"], D [0.99,0.01,0.1,0.9]),
                                                      ( ["sI","tb"], D [0.7,0.3,0.1,0.9]),
                                                      (["sI","td"],  D [0.99,0.01,0.001,0.999]),
                                                      (["sI","pl"], D [0.9,0.1,0.01,0.99])]
       let inputs = [[k,l,m,n,o]| k <-[0::Double,1], l <-[0,1], m <-[0,1], n <-[0,1], o <- [0,1] ]
       sequence_ $ map (\input -> eval_variables ["sI"] ["vT","ccd","tb","td","pl"] input graph) inputs

test_the_loop :: IO()
test_the_loop = do
    let graph = generate_d_graph[(["c#"],  D [0.5, 0.5]),
                                                   (["sql"],  D  [0.5,0.5]),
                                                   (["wa"],  D  [0.5,0.5]),
                                                   (["c#","c1"], D [0.9,0.1,0.2,0.8]),
                                                   ( ["sql","c2"], D  [0.9,0.1,0.2,0.8]),
                                                   (["c#","sql","hs3"], And),
                                                   (["hs3","c3"], D [0.9,0.1,0.2,0.8]),
                                                   (["c#","sql","hs4"], And),
                                                   (["hs4","c4"], D [0.9,0.1,0.2,0.8]),
                                                   (["c#","sql","hs5"], And),
                                                   (["hs5","c5"], D [0.9,0.1,0.2,0.8]),
                                                   (["c#","wa","hs6"], And),
                                                   (["hs6","c6"], D [0.9,0.1,0.2,0.8])
                                                   ]
    let inputs = [[x,y,z,a,b,c]| x <-[0::Double,1], y <-[0,1], z <-[0,1] , a <- [0,1], b <-[0,1], c<-[0,1]]
    sequence_ $ map (\input -> eval_variables ["c#","sql"] ["c1","c2","c3","c4","c5","c6"] input graph) inputs
