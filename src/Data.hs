module Data where
import qualified Data.ByteString               as B
import           Data.ByteString.Char8         as Char8
                                                ( unpack )
import           System.IO
import           Xeno.DOM

xml = B.readFile "../data/InputData.txt"

parsed = do
  text <- xml
  let Right node = parse text
  return node

skills = do
  par <- parsed
  let xs      = children par
  let skills' = concat $ map contents $ children ((children (xs !! 0)) !! 0)
  return $ map (Char8.unpack . (\(Text a) -> a)) skills'

needed_skills = do
  par <- parsed
  let xs = children par
  let skills' =
        map (map contents) $ map children $ children ((children (xs !! 0)) !! 1)
  return $ map
    ((map ((\x -> read x :: Int) . Char8.unpack . (\(Text a) -> a))) . concat)
    skills'

answeres = do
  par <- parsed
  let xs = children par
  let xs' =
        map (concat . (map (contents))) $ map (children) (children (xs !! 1))
  return $ map
    ((map ((\x -> read x :: Int) . Char8.unpack . (\(Text a) -> a))))
    xs'

correct = do
  par <- parsed
  let xs  = children par
  let xs' = map (concat . (map (contents))) $ map children (children (xs !! 2))
  return $ map
    ((map ((\x -> read x :: Bool) . Char8.unpack . (\(Text a) -> a))))
    xs'


right_answeres = do
  par <- parsed
  let xs  = children par
  let xs' = concat $ map contents (children (xs !! 3))
  return $ map ((\x -> read x :: Int) . Char8.unpack . (\(Text a) -> a)) xs'

possesed_skills = do
  par <- parsed
  let xs  = children par
  let xs' = map (concat . (map (contents))) $ map children (children (xs !! 4))
  return $ map
    ((map ((\x -> read x :: Bool) . Char8.unpack . (\(Text a) -> a))))
    xs'
