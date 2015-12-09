import IEEE754

main = do
    bits <- getLine
    putStrLn . show . floatToDecimal $ bits