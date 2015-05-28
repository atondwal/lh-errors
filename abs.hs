assert True = ()

foo n = assert $ (abs n) < 0 

main = return $ foo 5
--main = getLine >>= (return . foo . read) >>= print
