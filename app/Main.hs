{-# LANGUAGE NamedFieldPuns #-}

module Main where

data Queue a = Queue {front :: [a], rear :: [a], schedule :: [a]}
  deriving (Show)

empty :: Queue a
empty = Queue {front = [], rear = [], schedule = []}

isEmpty :: Queue a -> Bool
isEmpty (Queue {front = [], rear = [], schedule = []}) = True
isEmpty _ = False

rotate :: [a] -> [a] -> [a] -> [a]
rotate f r a = case (f, r) of
  ([], y : _) -> y : a
  (x : f', y : r') -> x : rotate f' r' (y : a)

queue :: Queue a -> Queue a
queue (Queue {front, rear, schedule = (_ : s)}) = Queue {front = front, rear = rear, schedule = s}
queue (Queue {front, rear, schedule = []}) = let f' = rotate front rear [] in Queue {front = f', rear = [], schedule = f'}

snoc :: Queue a -> a -> Queue a
snoc (Queue {front, rear, schedule}) x = queue (Queue {front = front, rear = x : rear, schedule = schedule})

safeHead :: Queue a -> Maybe a
safeHead (Queue {front = [], rear, schedule}) = Nothing
safeHead (Queue {front = (x : _), rear, schedule}) = Just x

safeTail :: Queue a -> Maybe (Queue a)
safeTail (Queue {front = [], rear, schedule}) = Nothing
safeTail (Queue {front = (_ : f), rear, schedule}) = Just $ queue Queue {front = f, rear = rear, schedule = schedule}

main :: IO ()
main = print "worst-case O(1) Immutable queue lol"