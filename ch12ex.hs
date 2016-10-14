{-# OPTIONS_GHC -Wall #-}
module Chapter12Exercise where

notThe :: String -> Maybe String
notThe word | word == "the" = Nothing
            | otherwise     = Just word

replaceThe :: String -> String
replaceThe = correct . replace . words
  where replace ws = map (replace' . notThe) ws
        replace' x | x == Nothing = Just "a"
                   | otherwise    = x
        correct ws = unwords . map correct' $ ws
        correct' (Just x) = x


isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z _ Nothing  = z
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe z = mayybee z id

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = map m . filter f
  where f Nothing = False
        f _       = True
        m (Just x) = x

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs | (length . catMaybes $ xs) == length xs = Just $ catMaybes xs
             | otherwise                            = Nothing

lefts' :: [Either a b] -> [a] 
lefts' = foldr f []
  where f (Left x) y  = (x:y)
        f (Right _) y = y

rights' :: [Either a b] -> [b] 
rights' = foldr f []
  where f (Right x) y  = (x:y)
        f (Left _) y = y

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right x) = Just $ f x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ g (Right x) = g x

eitherMaybe'' :: (b -> c) ->  Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (Just . f)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : iterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f z = myUnfoldr' $ f z
  where myUnfoldr' Nothing       = []
        myUnfoldr' (Just (x, y)) = x : myUnfoldr' (f y)

betterIterate :: (a -> a) -> a -> [a]
betterIterate f z = myUnfoldr ff z
  where ff x = Just (x, f x)


data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                    deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f z = unfold' $ f z
  where unfold' Nothing          = Leaf
        unfold' (Just (r, s, t )) = Node (unfold' $ f r) s (unfold' $ f t)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f n
  where f x | x == 0    = Nothing
            | otherwise = Just (x-1, n-x, x-1)
