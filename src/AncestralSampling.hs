module AncestralSampling
    (ancestral_sample,
    ancestral_samples)
    where

import           Internal.LoopyBelieveNet

partial_graph_from :: String -> Graph -> Graph
partial_graph_from name graph = map (filter_connections idx) found
    where
        idx = name_idx name graph
        (found,_) = splitAt (idx+1) graph


name_idx :: String -> Graph -> Int
name_idx name ((V_Node nr name' _ _ ):graph) | name == name' = nr
name_idx name (x:graph)                      = name_idx name graph

filter_connections :: Int -> Node -> Node
filter_connections idx (V_Node nr name cs bn) = V_Node nr name n_cs bn
                            where n_cs = filter (<= idx) cs
filter_connections idx (F_Node nr factor cs) = F_Node nr factor n_cs
                            where n_cs = filter (<= idx) cs
filter_connections idx (O_Node nr bn cs) = O_Node nr bn n_cs
                            where n_cs = filter (<= idx) cs

ancestral_samples :: [String] -> Int -> Graph ->  IO [[Double]]
ancestral_samples names n graph = sequence $ replicate n $ ancestral_sample names graph

ancestral_sample :: [String] -> Graph ->  IO [Double]
ancestral_sample names graph = ancestral_sample' names graph []

ancestral_sample' :: [String] -> Graph -> [Double] ->  IO [Double]
ancestral_sample' [] graph  found = return found
ancestral_sample' (v:ordered_variables) graph  found = do
                              let n_graph = partial_graph_from v graph
                              let bern  = eval_variable v n_graph
                              f_value <- (\x -> if x then 1 else 0) <$> (draw_bern bern)
                              let o_graph = set_input [v] [f_value] graph
                              ancestral_sample' ordered_variables o_graph (found ++ [f_value])

test_partial_graph :: IO()
test_partial_graph  = do
       let graph = generate_d_graph[(["c#"],  D [0.5, 0.5]),
                                                      (["sql"],  D  [0.5,0.5]),
                                                      (["c#","c1"], D [0.9,0.1,0.2,0.8]),
                                                      ( ["sql","c2"], D  [0.9,0.1,0.2,0.8]),
                                                      (["c#","sql","hs"], And),
                                                      (["hs","c3"], D [0.9,0.1,0.2,0.8])]
       print $ partial_graph_from "c#" graph
       print $ partial_graph_from "hs" graph
       --let inputs = [[x,y,z]| x <-[0::Double,1], y <-[0,1], z <-[0,1] ]
       --sequence_ $ map (\input -> eval_variables ["c#","sql","hs"] ["c1","c2","c3"] input graph) inputs
test_ancestral_sample :: IO()
test_ancestral_sample  = do
       let graph = generate_d_graph[(["c#"],  D [0.5, 0.5]),
                                                      (["sql"],  D  [0.5,0.5]),
                                                      (["c#","c1"], D [0.9,0.1,0.2,0.8]),
                                                      ( ["sql","c2"], D  [0.9,0.1,0.2,0.8]),
                                                      (["c#","sql","hs"], And),
                                                      (["hs","c3"], D [0.9,0.1,0.2,0.8])]
       found <- ancestral_samples ["c#","sql","c1", "c2","c3"] 10 graph
       print found
