module Foo.Bar.Main where

data IdentityM a = IdentityM a deriving (Show)

instance Monad IdentityM where
    (IdentityM x) >>= f = f x
    return = IdentityM



-- this is equivalent to >=>
infixl 1 >>>=
(>>>=) :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
(>>>=) f g x = f x >>= g

-- fail: can't define >>=# (analog of >>=) using only >>>=
-- thats why >>= is present in definitino of Monad, and not >>>=
-- infixl 1 >>=#
-- >>=# :: (Monad m) => m b -> (b -> m c) -> m c
-- m >>=# f = ???

(>>=#) :: (Monad m) => m b -> (b -> m c) -> m c
(>>=#) mx f = const mx >>>= f $ undefined


infixl 1 >>>
(>>>) :: (Monad m) => (a -> m b) -> m c -> a -> m c
(>>>) f g x = f x >> g


(>>>) :: (Monad m) => (a -> m b) -> m c -> a -> m c
(>>>) f g x = f x >> g


extract :: IdentityM a -> a
extract (IdentityM x) = x

main :: IO ()
main = do
    let x = 10 :: Int
    putStrLn $ show . extract $ (h x :: IdentityM Int)

    putStrLn >>> putStrLn "world" $ "hello"
  where
    f :: (Num t) => t -> IdentityM t
    f x = return $ x^(2 :: Int)

    g :: (Num t) => t -> IdentityM t
    g x = return $ x - 1

    h :: (Integral t, Num t) => t -> IdentityM t
    h = f >>>= g

    -- h = \x -> f x >>= g
