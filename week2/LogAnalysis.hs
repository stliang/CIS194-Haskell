{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- | parse a line from log and returns a LogMessage
-- read may blow up, leave this to future lessons
parseMessage :: String -> LogMessage
parseMessage s =
  let ss = words s
      lm =
        case ss of
          (a:b:c:_) ->
            case a of
              ('I':_) -> LogMessage Info (read b :: Int) s
              ('W':_) -> LogMessage Warning (read b :: Int) s
              ('E':_) -> LogMessage (Error (read b :: Int)) (read c :: Int) s
              _ -> Unknown s
          _ -> Unknown s
  in lm

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

compareLogTime :: (Int -> Int -> Bool) -> LogMessage -> LogMessage -> Bool
compareLogTime f (LogMessage _ t1 _) (LogMessage _ t2 _) = f t1 t2
compareLogTime _ _ _ = False

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert m Leaf = Node Leaf m Leaf
insert m (Node lt v rt)
  | compareLogTime (==) m v = Node lt v rt
  | compareLogTime (<) m v = Node lt v (insert m rt)
  | compareLogTime (>) m v = Node (insert m lt) v rt
insert _ _ = Leaf

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:[]) = insert x Leaf
build (x:ys) = insert x (build ys)

inOrder :: MessageTree -> [LogMessage]
inOrder tree = flatten inorder tree

sortLog :: String -> IO [LogMessage]
sortLog s = do
  ms <- parse <$> readFile s
  return (inOrder $ build ms)

flatten traversal = traversal (:) []

traverse2 step f z tree = go tree z
  where
    go Leaf z = z
    go (Node l v r) z = step (f v) (go l) (go r) z

preorder = traverse2 $ \n l r -> r . l . n

inorder = traverse2 $ \n l r -> r . n . l

postorder = traverse2 $ \n l r -> n . r . l

errorGrtSev :: Int -> [LogMessage] -> [LogMessage]
errorGrtSev _ [] = []
errorGrtSev i (x:[]) =
  case x of
    (LogMessage (Error n) _ _)
      | n > i -> [x]
    _ -> []
errorGrtSev i (x:ys) = errorGrtSev i [x] ++ errorGrtSev i ys

logToString :: [LogMessage] -> [String]
logToString [] = []
logToString ((LogMessage _ _ s):[]) = [s]
logToString (x:ys) = logToString [x] ++ logToString ys

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lm = logToString $ inOrder $ build $ errorGrtSev 50 lm
