{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Parsing logs
parse :: String -> [LogMessage]
parse s = map (\x -> (parseMessage x)) (lines s)

parseMessage :: String -> LogMessage
parseMessage s = parseMessageHelper (words s)

parseMessageHelper :: [String] -> LogMessage
parseMessageHelper ("I":xs) = LogMessage Info (read (head xs)) (unwords (tail xs))
parseMessageHelper ("W":xs) = LogMessage Warning (read (head xs)) (unwords (tail xs))
parseMessageHelper ("E":xs) = LogMessage (Error (read (head xs))) (read (head (tail xs))) (unwords (tail (tail xs)))
parseMessageHelper x = Unknown (unwords x)

-- Sorting logs
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert l Leaf = Node Leaf l Leaf
insert l@(LogMessage _ t _) (Node mtl lmt@(LogMessage _ tn _) mtr)
	| t > tn 	= Node mtl lmt (insert l mtr)
	| otherwise 	= Node (insert l mtl) lmt mtr

build :: [LogMessage] -> MessageTree
build lms = buildRecursive lms Leaf

buildRecursive :: [LogMessage] -> MessageTree -> MessageTree
buildRecursive (lm:lms) mt = buildRecursive lms (insert lm mt)
buildRecursive [] mt = mt

inOrder :: MessageTree -> [LogMessage]
inOrder (Node mtl@(Node _ _ _) lm mtr@(Node _ _ _)) = (inOrder mtl) ++ [lm] ++ (inOrder mtr)
inOrder (Node Leaf lm mtr@(Node _ _ _)) = [lm] ++ (inOrder mtr)
inOrder (Node Leaf lm Leaf) = [lm]
inOrder (Node mtl@(Node _ _ _) lm Leaf) = (inOrder mtl) ++ [lm]
inOrder Leaf = []

-- Extracting "relevant" messages
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = map (\(LogMessage _ _ s) -> s) (filter isImportant (inOrder (build lms)))

isImportant :: LogMessage -> Bool
isImportant (LogMessage (Error i) _ _)
	| i > 50	= True
	| otherwise	= False
isImportant _		= False
