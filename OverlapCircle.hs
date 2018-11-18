{-# LANGUAGE ScopedTypeVariables #-}
import Data.List (tails, span, break)
import Data.Foldable
import Data.Bits

data LeafList nil cons = Nil nil | Cons cons (LeafList nil cons)
  deriving (Eq, Ord, Show)

fromList nil = foldr Cons (Nil nil)

foldTails :: b -> (a -> [a] -> b -> b) -> [a] -> b
foldTails nil _ [] = nil
foldTails nil cons (x:xs) = cons x xs (foldTails nil cons xs)


{-
sentinel vs [sentinel + 1 : sentinel + step]
then: step := 2 * step
-}

detectCycleStages :: forall a . Eq a => [a] -> Bool
detectCycleStages = helper 1 where
  helper :: Int -> [a] -> Bool
  helper _ [] = False
  helper n (x:xs) =
    let (front, back) = splitAt n xs in
    case break (==x) front of
      -- not found
      (_, []) -> helper (2*n) back
      -- cycle
      (_, _) -> True

-- popCount
dcBin :: forall a . Eq a => [a] -> Bool
dcBin xs = foldr cons nil (zip [(1 :: Int)..] xs) (const False) where
  nil _ = False
  cons (i, x) rest check
    | popCount i <= 1
    = rest (x ==)
    | otherwise
    = check x || rest check


detectCycleStagesLoop :: forall a . Eq a => [a] -> Bool
detectCycleStagesLoop xs = helper 1 0 (const False) xs  where
  helper _    _ _ [] = False
  helper step 0 _ (x:xs) = helper (2*step) step (==x) xs

  helper step left check (x : xs)
    = check x || helper step (left - 1) check xs

detectCycleStagesLoop1 :: forall a . Eq a => [a] -> Bool
detectCycleStagesLoop1 xs = foldr cons nil xs 1 0 (const False) where
  cons x rest step 0 _ = rest (2*step) step (==x)
  cons x rest step left check
    = check x || rest step (left - 1) check
  nil _ _ _ = False



-- detectCycleStagesOps xs =
--  any checkBlock blocks where
--  -- More than one block.
--  checkBlock [] = Finite
--  checkBlock (x:xs)
--    | not . null . dropWhile (==x) $ xs
--    = Cycle
--  checkBlock _ = CantTell
--  -- Below is just fold.
-- detectCycleStagesOps xs = foldr cons nil (zipWith (_) ops xs) start where
--   nil _ = False
--   cons op rest acc = rest (op acc)
--   start = const False
--
--   ops :: [a -> ((a -> Bool) -> Bool) -> (a -> Bool) -> Bool]
--   ops = do
--     step <- iterate (2*) 1
--     replace : replicate step test
--
--   replace :: a -> ((a -> Bool) -> Bool) -> ignored -> Bool
--   replace x rest _oldCheck = rest (x==)
--   test :: a -> ((a -> Bool) -> Bool) -> (a -> Bool) -> Bool
--   test x rest check
--     | check x = True
--     | otherwise = rest check

-- splitAt
-- span

detectCycle :: forall a . Eq a => [a] -> LeafList [a] a
-- Needs to be 2 here:
-- Non-trivial gcd of cycle length and difference in speeds
-- gives trouble.
detectCycle xs = helper xs (everyNth 2 $ drop 1 xs) where
  helper :: [a] -> [a] -> LeafList [a] a
  helper (slow:slows) (fast:_)
    | slow == fast
    = Nil $ slow : takeWhile (/= slow) slows
  helper (slow:slows) (_:fasts)
    = slow `Cons` helper slows fasts
  helper slows _ = Nil slows

-- Use zip3 and group_by (/=)


-- detectCycle xs = foldTails nil cons xs (everyNth 2 $ drop 1 xs)
--   nil = _
--   cons slow slows
--   fast = everyNth 3 $ drop 1 xs
-- 
--   update ([], _) _ = Nil []
--   update ((slow : slows), fast) rest
--     | fast == slow = Nil $ slow : takeWhile (/=slow) slows
--     | otherwise = slow `Cons` rest

everyNth :: Int -> [a] -> [a]
everyNth n [] = []
everyNth n (x:xs) = x : (everyNth n . drop (n-1)) xs

main = return ()

--- (8,11),(9,8),(10,11),(11,8),(6,11),(7,8),(8,11)
--   0      1      2       3     4      5      6
--  (1, 2) (2, 1)
--
-- 
