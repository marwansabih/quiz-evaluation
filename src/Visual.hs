module Visual where
import           Data
import           Data.List
import           Graphics.Image as I

visualize_table_continious ::  [[Double]] -> Int -> Image VU RGB Double
visualize_table_continious table pixelsize = makeImageR VU (
                                                                                                      height*pixelsize, width*pixelsize) (\(i, j)
                                                                                                      -> let color = (table !! (div  i pixelsize)) !! (div j pixelsize)
                                                                                                      in PixelRGB color color color
                                                                                                     )
              where
                height = length table
                width = length $ head table

visualize_table :: [[[Int]]] -> [[Double]] -> Int -> Image VU RGB Double
visualize_table table colors pixelsize = makeImageR VU (
                                                                                              height*pixelsize, width*pixelsize) (\(i, j)
                                                                                              -> let ids = (table !! (div  i pixelsize)) !! (div j pixelsize)
                                                                                                   in  if length ids == 1
                                                                                                        then
                                                                                                          let idx = head ids
                                                                                                          in PixelRGB ((colors !! idx ) !! 0) ((colors !! idx) !! 1)   ((colors !! idx) !! 2)
                                                                                                        else
                                                                                                          let
                                                                                                             idx1 = head ids
                                                                                                             idx2 = last ids
                                                                                                          in if  mod i pixelsize < div pixelsize  2
                                                                                                                then PixelRGB ((colors !! idx1) !! 0) ((colors !! idx1) !! 1)   ((colors !! idx1) !! 2)
                                                                                                                else PixelRGB ((colors !! idx2) !! 0) ((colors !! idx2) !! 1)   ((colors !! idx2) !! 2)
                                                                                             )
                              where
                                height = length table
                                width = length $ head table

ordered_by_test_result_and_category :: IO ( Image VU RGB Double )
ordered_by_test_result_and_category  = do
         skills <- Prelude.map (Prelude.map (+1)) <$> needed_skills
         pairs <- (Prelude.map ( zip skills ) . (sortBy by_correct)) <$> correct
         let table =  (Prelude.map (Prelude.map (\x -> if snd x then [0] else fst x)))  pairs
         return $ visualize_table table [[1,1,1],[0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0]] 30

complete_ordered :: IO ( Image VU RGB Double )
complete_ordered = do
         skills <- Prelude.map (Prelude.map (+1)) <$> needed_skills
         pairs <- (Prelude.map ( zip skills ) . (sortBy by_correct)) <$> correct
         let n_pairs = Data.List.transpose  $ sortBy by_correct2 (Data.List.transpose pairs)
         let table =  (Prelude.map (Prelude.map (\x -> if snd x then [0] else fst x)))  n_pairs
         return $ visualize_table table [[1,1,1],[0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0]] 30

by_correct :: [Bool] -> [Bool] -> Ordering
by_correct x y = if c_xs < c_ys then GT else LT
            where
                   c_xs = length $ filter id x
                   c_ys = length $ filter id y


by_correct2 :: [([Int],Bool)] ->  [([Int],Bool)]  -> Ordering
by_correct2 x y = if c_xs < c_ys then GT else LT
            where
                   c_xs = length $ filter snd x
                   c_ys = length $ filter snd y

by_correct3:: ([[Int]],[Bool])-> ([[Int]],[Bool]) -> Ordering
by_correct3 (a,x) (b,y) = compare c_ys c_xs
            where
                   c_xs = length $ filter id x
                   c_ys = length $ filter id y


existing_skills = do
      pairs <-(Data.List.map (zip [1..7])) <$> possesed_skills
      let table = Data.List.map (Data.List.map (\x -> if snd x then  [0] else [fst x])) pairs
      return $ visualize_table table [[1,1,1],[0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0]] 30

ordered_by_test_result_and_category_with_skills :: IO ( Image VU RGB Double )
ordered_by_test_result_and_category_with_skills  = do
         skills <- Prelude.map (Prelude.map (+1)) <$> needed_skills
         pairs2 <-(Data.List.map (zip [1..7])) <$> possesed_skills
         let table2 = Data.List.map (Data.List.map (\x -> if snd x then  [0] else [fst x])) pairs2
         n_correct <- (zip table2) <$> correct
         let pairs =  sortBy by_correct3 n_correct
         let table =  Prelude.map (\(a,b) -> a ++ (Prelude.map (\x -> if snd x then [0] else fst x)) ( zip skills b))  pairs
         return $ visualize_table table [[1,1,1],[0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0]] 30
