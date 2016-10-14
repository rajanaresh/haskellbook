-- main :: IO ()
-- main = putStrLn "Hey, what's your name" >>
--        getLine >>=
--        \name -> putStrLn $ "Sup, "++name

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")


