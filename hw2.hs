{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
  import Log
  import Debug.Trace


  parseMessage :: String -> LogMessage
  parseMessage x = case words x of
    ("I":b:c) -> LogMessage Info (read b)(unwords c)
    ("E":b:c:d) -> LogMessage (Error (read b))(read c)(unwords d)
    ("W":b:c) -> LogMessage Warning (read b)(unwords c)
    _ -> trace ("Hello World -"++x) (Unknown x)


  parse :: String -> [LogMessage]
  parse x = case lines x of
    (y:ys) -> [parseMessage y] ++ parse (unlines ys)
    _ -> []

  insert :: LogMessage -> MessageTree -> MessageTree
  insert (Unknown _) x = x
  insert x (Leaf) = Node Leaf x Leaf
  insert _ mt@(Node _ (Unknown _) _) = mt
  insert message@(LogMessage _ ltime _) (Node leftTree top@(LogMessage _ rtime _) rightTree) =
    if ltime < rtime then
      Node (insert message leftTree) top rightTree
    else
      Node leftTree top (insert message rightTree)

  build :: [LogMessage] -> MessageTree
  build [] = Leaf
  build (x:xs) = insert x (build xs)

  inOrder :: MessageTree -> [LogMessage]
  inOrder Leaf = []
  inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

  whatWentWrong :: [LogMessage] -> [String]
  whatWentWrong [] = []
  whatWentWrong (LogMessage (Error level) _ str:xs) 
    | level > 50 =  [str] ++ whatWentWrong xs
  whatWentWrong (_:xs) = whatWentWrong xs
