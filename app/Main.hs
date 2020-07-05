{-# LANGUAGE FlexibleInstances #-}
import           AncestralSampling
import           Data
import           Data.List
import           Graphics.Image                as I
                                                ( displayImage
                                                , writeImage
                                                )
import           LoopyBelieveNet
import           System.Random
import           Visual

import           Graphics.Rendering.Chart.Plot.Histogram
                                                ( defaultNormedPlotHist )
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           Control.Monad
import           Data.Random.Distribution.Beta
import           Data.Random.Distribution
import Graphics.Rendering.Chart.Plot.Lines

generateAllCombination :: Int -> [[Double]]
generateAllCombination nr = generateAllCombination' nr [[]]

generateAllCombination' :: Int -> [[Double]] -> [[Double]]
generateAllCombination' 0 found = found
generateAllCombination' x xs    = generateAllCombination' (x - 1) n_xs
  where n_xs = map (0 :) xs ++ map (1 :) xs



--Task 1
-- just some mulitplication of Bernouille distribution
task1 :: IO ()
task1 = do
  print $ mult_bern ("test", 0.3) ("test", 0.9)
  print $ mult_bern ("test", 0.5) ("test", 0.2)
  print $ mult_bern ("test", 0.5) ("test", 0.3)
  print $ mult_bern ("test", 1) ("test", 0.2)
  print $ mult_bern ("test", 1) ("test", 0.3)

--Task 2
task2 :: IO ()
task2 = do
  first_values  <- replicateM 10000 (draw_bern ("test", 0.5))
  second_values <- replicateM 10000 (draw_bern ("test", 0.5))
  let trues = length $ filter (uncurry (&&)) (zip first_values second_values)
  let falses = length
        $ filter (\(a, b) -> not a && not b) (zip first_values second_values)
  print $ fromIntegral trues / fromIntegral (trues + falses)
  print $ mult_bern ("test", 0.3) ("test", 0.3)


--Task 3
-- BelieveNetwork for estimating skill based on given answeres on multiple choice
-- internal structre (c1 = correct 1 etc.)
--   F B(0.5)      F B(0.5)
--    |                    |
--   c#                sql
--    |                   |
--  F N(c1|c# ) F N(c2|sql)   F (And(hs|c+,sql)) (should be connected to c# and sql)
--    |                    |                            |
--   c1                c2                           hs
--                                                      |
--                                                  F (c3| hs)
--                                                      |
--                                                     c3
task3 :: IO ()
task3 = do
  let graph = generate_d_graph
        [ (["c#"]             , D [0.5, 0.5])
        , (["sql"]            , D [0.5, 0.5])
        , (["c#", "c1"]       , D [0.9, 0.1, 0.2, 0.8])
        , (["sql", "c2"]      , D [0.9, 0.1, 0.2, 0.8])
        , (["c#", "sql", "hs"], And)
        , (["hs", "c3"]       , D [0.9, 0.1, 0.2, 0.8])
        ]
  let inputs = generateAllCombination 3
  evalVariablesInputs ["c#", "sql", "hs"] ["c1", "c2", "c3"] inputs graph

--just for fun
forfun :: IO ()
forfun = do
  let graph = generate_d_graph
        [ (["sI"]       , D [0.3, 0.7])
        , (["sI", "vT"] , D [0.9, 0.1, 0.01, 0.99])
        , (["sI", "ccd"], D [0.99, 0.01, 0.1, 0.9])
        , (["sI", "tb"] , D [0.7, 0.3, 0.1, 0.9])
        , (["sI", "td"] , D [0.99, 0.01, 0.001, 0.999])
        , (["sI", "pl"] , D [0.9, 0.1, 0.01, 0.99])
        ]
  let inputs = generateAllCombination 5
  evalVariablesInputs ["sI"] ["vT", "ccd", "tb", "td", "pl"] inputs graph

-- task 4
task4 :: IO ()
task4 = do
  let graph = generate_d_graph
        [ (["c#"]              , D [0.5, 0.5])
        , (["sql"]             , D [0.5, 0.5])
        , (["c#", "c1"]        , D [0.9, 0.1, 0.2, 0.8])
        , (["sql", "c2"]       , D [0.9, 0.1, 0.2, 0.8])
        , (["c#", "sql", "hs3"], And)
        , (["hs3", "c3"]       , D [0.9, 0.1, 0.2, 0.8])
        , (["c#", "sql", "hs4"], And)
        , (["hs4", "c4"]       , D [0.9, 0.1, 0.2, 0.8])
        ]
  let inputs = generateAllCombination 4
  evalVariablesInputs ["c#", "sql"] ["c1", "c2", "c3", "c4"] inputs graph



task5 :: IO ()
task5 = do
      -- this image of a table shows in color of specific category, which question a person has failed,
      -- here the persons are ordered by least mistakes to most from top to bottom
      -- and the questions are ordered by category
      -- furthermore on the left a skill table is appended showing the skills a particular person has.
  image1 <- ordered_by_test_result_and_category_with_skills
  writeImage "../visualisation/ordered_by_category_and_test_result.png" image1
  -- this image of a table shows in color of specific category, which question a person has failed,
  -- here the persons are ordered by least mistakes to most from top to botto
  -- and the questions are ordered from least errors to most from left to right
  image2 <- complete_ordered
  writeImage
    "../visualisation/ordered_by_question_difficulty_and_test_result.png"
    image2


visualTable table = visualize_table table colors 30
   where 
     colors =   [1,1,1]: init (generateAllCombination 3)


toNodeDescribtion :: Show a => [String] -> [Int] -> (Dist, a) -> [([String], Dist)]
toNodeDescribtion skill xs (d, nr) = if length xs == 1
  then [([s_n, "c" ++ show nr], d)]
  else [([s_n, s_n2, hs], And), ([hs, "c" ++ show nr], d)]
 where
  s_n  = skill !! head xs
  s_n2 = skill !! last xs
  hs   = "hs" ++ show nr

originalInferred :: IO [[Double]]
originalInferred = do
  let skill       = ["core", "oo", "lc", "wa", "da", "sql", "c#"]
  let skill_nodes = map (\x -> ([x], D [0.5, 0.5])) skill
  --let to_skill_name [x,y]  nr = "hs" ++ (show x) ++ "_" ++ (show y)
  let d = D [0.9, 0.1, 0.2, 0.8]
  questions_nodes <-
    concatMap (\(nr, xs) -> toNodeDescribtion skill xs (d,nr)) . (zip [0 ..]) <$> needed_skills
  let graph         = generate_d_graph $ skill_nodes ++ questions_nodes
  let answere_nodes = map (\x -> "c" ++ (show x)) [0 .. 47]
  inputs <- map (map (\x -> if x then 1 :: Double else 0)) <$> correct
  return $ map (map snd) $ map
        (\input -> sample_variables skill answere_nodes input graph)
        inputs

task6 = do
  table <- originalInferred
  table2 <- (map (map (\x -> if x then 1 :: Double else 0))) <$> possesed_skills
  let image = visualize_table_continious (zipWith (++) table table2) 30
  writeImage "../visualisation/prediction_vs_ground_truth.png" image


-- testing if the inference is correct
task7 = do
  let skill       = ["core", "oo", "lc", "wa", "da", "sql", "c#"]
  let skill_nodes = map (\x -> ([x], D [0.5, 0.5])) skill
  let d = D [0.9, 0.1, 0.2, 0.8]
  questions_nodes <-
    concatMap (\(nr, xs) -> toNodeDescribtion skill xs (d,nr)) . (zip [0 ..]) <$> needed_skills
  let graph         = generate_d_graph $ skill_nodes ++ questions_nodes
  let answere_nodes = map (\x -> "c" ++ (show x)) [0 .. 47]
  table <- ancestral_samples (skill ++ answere_nodes) 50 graph
  let pairs = map (zip [(1 :: Int) ..] . (map (\x -> x)) . take 7) table
  let c_table = map
        (map (\(a, b) -> if b == (1 :: Double) then [0 :: Int] else [a]))
        pairs
  skills <- Prelude.map (Prelude.map (+ 1)) <$> needed_skills
  let n_pairs = map ((zip skills) . drop 7) table
  let a_table = Prelude.map
        (Prelude.map (\x -> if snd x == 1 then [0] else fst x))
        n_pairs
  let image = visualTable $ zipWith (++) c_table a_table
  writeImage "../visualisation/sampled_skills_answeres.png" image
  let inference_table =
        map (map snd)
          $ map (\input -> sample_variables skill answere_nodes input graph)
          $ map (drop 7) table
  let sampled_skill = map (take 7) table
  let inference_image = visualize_table_continious
        (zipWith (++) sampled_skill inference_table)
        30
  writeImage "../visualisation/sampled_vs_inferred_skills.png" inference_image



-- get a feeling for how the model is influenced by different choices of the distributions
task8 = do
  let skill = ["core", "oo", "lc", "wa", "da", "sql", "c#"]
  priors <- sequence $ replicate 7 (randomRIO (0 :: Double, 1))
  let skill_nodes = map (\(x, y) -> ([x], D [0.5, 0.5])) (zip skill priors)
  --let to_skill_name [x,y]  nr = "hs" ++ (show x) ++ "_" ++ (show y)
  dists <-
    (map (\[a, b] -> D [a, 1 - a, b, 1 - b]))
      <$> (sequence $ replicate 48 $ sequence
            [randomRIO (0.99 :: Double, 1), randomRIO (0 :: Double, 0.01)]
          )
  questions_nodes <-
    (concatMap (\(nr, xs) -> toNodeDescribtion skill xs nr) . (zip (zip dists [0 ..])))
      <$> needed_skills
  let graph         = generate_d_graph $ skill_nodes ++ questions_nodes
  let answere_nodes = map (\x -> "c" ++ (show x)) [0 .. 47]
  table <- ancestral_samples (skill ++ answere_nodes) 50 graph
  let pairs = map (zip [(1 :: Int) ..] . (map (\x -> x)) . take 7) table
  let c_table = map
        (map (\(a, b) -> if b == (1 :: Double) then [0 :: Int] else [a]))
        pairs
  skills <- Prelude.map (Prelude.map (+ 1)) <$> needed_skills
  let n_pairs = map ((zip skills) . drop 7) table
  let a_table =
        (Prelude.map (Prelude.map (\x -> if snd x == 1 then [0] else fst x)))
          n_pairs
  let image = visualTable (zipWith (++) c_table a_table)

  displayImage image
  let inference_table =
        map (map snd)
          $ map (\input -> sample_variables skill answere_nodes input graph)
          $ map (drop 7) table
  let sampled_skill = map (take 7) table
  let inference_image = visualize_table_continious
        (zipWith (++) sampled_skill inference_table)
        30
  displayImage inference_image


hist :: [Double] -> Int -> Int -> PlotHist Double Double
hist sample goal nr = defaultNormedPlotHist
  { _plot_hist_title     = "beta " ++ show (goal + 1) ++ " " ++ show
                             ((nr - goal) + 1)
  , _plot_hist_values    = sample
  , _plot_hist_bins      = 500
  , _plot_hist_norm_func = \n y -> realToFrac y / n
  }


sampledPropability :: Int -> Int -> IO [Double]
sampledPropability nr goal = sampledPropability' nr goal []

sampledPropability' :: Int -> Int -> [Double] -> IO [Double]
sampledPropability' 0  _    found = return found
sampledPropability' nr goal xs    = do
  sample_p <- randomRIO (0 :: Double, 1)
  result   <- replicateM 10 $ draw_bern ("Test", sample_p)
  let hit_count = length $ filter id result
  if hit_count == goal
    then sampledPropability' (nr - 1) goal (sample_p : xs)
    else sampledPropability' nr goal xs


betaPdf :: [Double] -> Int -> Int-> [(Double, Double)]
betaPdf xs goal nr =
  [ (x, pdf beta x) | x <- xs ]
  where
    beta = Beta (fromIntegral (goal + 1)) (fromIntegral (nr-goal+1))


chart sample goal nr = toFile def "../visualisation/hist_vs_beta.png" $ do
  layout_title .= "Estimated beta"
  --plot_lines_style .= defaultPlotLineStyle
  plot $ fmap histToPlot $ liftCState $ return $ hist sample goal nr
  plot ( line "" [betaPdf [0,0.001..1] goal nr])

plotBetas = toFile def "../visualisation/betas.png" $ do
  plot ( line "Beta(1,1)" [betaPdf [0,0.001..1] 0 0])
  plot ( line "Beta(2,2)" [betaPdf [0,0.001..1] 1 2])
  plot ( line "Beta(2,5)" [betaPdf [0,0.001..1] 1 5])
  plot ( line "Beta(4,10)" [betaPdf [0,0.001..1] 3 12])
  plot ( line "Beta(8,20)" [betaPdf [0,0.001..1] 7 26])

plotBetasRisingSampleSize = toFile def "../visualisation/betas_rising_sample_size.png" $ do
  plot ( line "Beta(4,8)" [betaPdf [0,0.001..1] 3 10])
  plot ( line "Beta(31,71)" [betaPdf [0,0.001..1] 30 100])
  plot ( line "Beta(301,701)" [betaPdf [0,0.001..1]300 1000])
  plot ( line "Beta(3001,7001)" [betaPdf [0,0.001..1] 3000 10000])


-- task for showing the origin of the beta distribution by experiment
task9 :: IO ()
task9 = do
  sample <- sampledPropability 1000000 3
  chart sample 3 10
  plotBetas
  plotBetasRisingSampleSize


estimatedDistribution :: IO [Dist]
estimatedDistribution = do
  cor <- correct
  ps  <- possesed_skills
  cat <- needed_skills
  let ps_cor = map (zip cat) cor
  let has_skill_vs_cor = transpose $ map
        (\(hs, xs) -> map (\(a, b) -> (and (map (\x -> hs !! x) a), b)) xs)
        (zip ps ps_cor)
  let guessed_answeres =
        map ((map snd) . (filter (\(a, b) -> not a))) has_skill_vs_cor
  let
    true_vs_false = map
      (\xs ->
        let nr_true = length (filter id xs) in (nr_true, length xs - nr_true)
      )
      guessed_answeres
  let skilled_answeres = map ((map snd) . filter fst) has_skill_vs_cor
  let true_vs_false2 = map (\xs -> let nr_true = length (filter id xs) in (nr_true, length xs - nr_true)) skilled_answeres
  let guess_p = map
        (\(a, b) ->
          (1.5 + fromIntegral a) / (1.5 + fromIntegral a + 3.5 + fromIntegral b)
        )
        true_vs_false
  let skill_p = map
        (\(a, b) ->
          (4 + fromIntegral a) / (4 + fromIntegral a + 1 + fromIntegral b)
        )
        true_vs_false2
  return $ map (\(x1, x2) -> D [x1, 1 - x1, x2, 1 - x2]) (zip skill_p guess_p)

titles = ["original","learned"]

barPlot values file = toFile def file $ do
    layout_title .= "Negative log propability"
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
    plot $ plotBars <$> bars titles (addIndexes (map snd values))



nLogProbability :: [[Double]] -> [[Bool]] -> [Double]
nLogProbability xs expected = map (( \x ->  x / fromIntegral nr_person) . sum) probs 
  where
    probs' = zipWith (zipWith(\x y -> if y then - log x else  - log (1-x) )) xs expected 
    probs = transpose probs'
    nr_person = length probs'


plotRoc original learned perfect random = toFile def "../visualisation/roc_curves.png" $ do
  layout_title .= "ROC curves"
  let or_ar = show $ toAreas original
  let le_ar = show $ toAreas learned
  let pe_ar = show $ toAreas perfect
  let ra_ar = show $ toAreas random
  plot (line ("original AUC " ++ or_ar) [original])
  plot (line ("learned AUC " ++ le_ar) [learned])
  plot (line ("perfect AUC " ++ pe_ar) [perfect])
  plot (line ("ramdom AUC " ++ ra_ar) [random])

truePositivVsFalsePositiv :: [(Double,Bool)] -> Int -> (Double,Double)
truePositivVsFalsePositiv xs n = (nr_fp/(nr_fp + nr_tn),nr_tp/(nr_tp + nr_fn))
  where 
    (p_positiv,p_false) = splitAt n $ map snd xs
    nr_tp = fromIntegral $ length $ filter id p_positiv
    nr_fn = fromIntegral $ length $ filter id p_false
    nr_fp = fromIntegral$ length $ filter not p_positiv
    nr_tn = fromIntegral $ length $ filter not p_false 

toAreas :: [(Double,Double)] -> Double
toAreas xs = sum delta_areas
  where 
    pairs = zip xs (tail xs)
    delta_areas = map (\((x1,y1),(x2,y2)) -> (x2-x1)*y1) pairs
    
rocValues :: [[Double]] -> [[Bool]] -> [(Double,Double)]
rocValues table expected = values
  where
    org_skill_table = concat $ zipWith zip table expected
    org_sorted_skill = sortBy (\x y -> compare (fst y) (fst x)) org_skill_table
    values = map (truePositivVsFalsePositiv org_sorted_skill) [0..(length org_sorted_skill)]

-- a bit more than the required task
testOutBetaAndTask10 :: IO ()
testOutBetaAndTask10 = do
  let skill       = ["core", "oo", "lc", "wa", "da", "sql", "c#"]
  let skill_nodes = map (\x -> ([x], D [0.5, 0.5])) skill
  dists <- estimatedDistribution
  questions_nodes <- concatMap (\(nr, xs) -> toNodeDescribtion skill xs nr) . zip (zip dists [0 ..]) <$> needed_skills
  let graph         = generate_d_graph $ skill_nodes ++ questions_nodes
  let answere_nodes = map (\x -> "c" ++ (show x)) [0 .. 47]
  inputs <- (map (map (\x -> if x then 1 :: Double else 0))) <$> correct
  let applyInput input = sample_variables skill answere_nodes input graph
  let table = map (map snd) $ map applyInput inputs
  table2 <- (map (map (\x -> if x then 1 :: Double else 0))) <$> possesed_skills
  let image = visualize_table_continious (zipWith (++) table table2) 30
  writeImage "../visualisation/beta_prediction_vs_ground_truth.png" image
  table_org <- originalInferred
  expected <- possesed_skills
  let probs_org = nLogProbability table_org expected
  let probs = nLogProbability table expected
  let values =zip skill $ zipWith (\a b -> [a,b]) probs_org probs
  --calculating overall log propablity
  let avg_probs_org =   sum probs_org / 7
  let avg_probs = sum probs / 7
  let overall = [("Overall",[avg_probs_org, avg_probs])] 
  barPlot (overall ++ values) "../visualisation/original_vs_improved.png"
  --roc_curves
  let original_values = rocValues table_org expected
  let learned_values = rocValues table expected
  let perfect_values = rocValues ( map ( map (\x -> if x then 1 else 0) ) expected) expected
  r_v <- replicateM (length expected) $ replicateM (length (head expected)) $ randomRIO (0::Double,1)
  let random_values = rocValues r_v expected
  plotRoc original_values learned_values perfect_values random_values


main :: IO ()
main = do
  task8
