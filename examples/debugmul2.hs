import Bytes
import Control.Monad

main2 :: IO ()
main2 = do
    let x = 0xFD94E3B7FE36FB18 :: Integer
    let xbits = (bytesUint64L x)
    let y = 49 :: Integer
    let ybits =(bytesUint64L y)

    let mul = x * y
    let mulbits = bytesUint128L mul

    putStrLn $ "x: " <> show x <> "  " <> show xbits
    putStrLn $ "xhi: " <> show (signed $ block 32 1 xbits)
    putStrLn $ "xlo: " <> show (signed $ block 32 0 xbits)
    putStrLn $ "y: " <> show y <> " " <> show ybits
    putStrLn $ "yhi: " <> show (signed $ block 32 1 ybits)
    putStrLn $ "ylo: " <> show (signed $ block 32 0 ybits)

    putStrLn $ "mul: " <> show mul <> " " <> show mulbits
    forM_ [0..1] $ \i -> do
        putStrLn $ "  block64: " <> show i <> show (block 64 i mulbits)
    forM_ [0..3] $ \i -> do
        putStrLn $ "  block32: " <> show i <> show (block 32 i mulbits)



    let mymul = 9223372039682133912 :: Integer
    let mymulbits = bytesUint128L mymul
    putStrLn $ "***my mul: " <> show mymul <> " " <> show mymulbits
    putStrLn $ "***mymul - mul " <> show (mymul - mul)

    putStrLn $ "***mymul bits: "
    forM_ [0..1] $ \i -> do
        putStrLn $ "  block64: " <> show i <> show (block 64 i mymulbits)
    forM_ [0..3] $ \i -> do
        putStrLn $ "  block32: " <> show i <> show (block 32 i mymulbits) <>
            show (signed $ block 32 i mymulbits)


    putStrLn $ "***bits that differ: "
    print (mymulbits #^ mulbits)
    putStrLn $ "***mymul ^ mul bits: "
    forM_ [0..1] $ \i -> do
        putStrLn $ "  block64: " <> show i <> show ((block 64 i mymulbits) #^ (block 64 i mulbits))
    forM_ [0..3] $ \i -> do
        putStrLn $ "  block32: " <> show i <> show ((block 32 i mymulbits) #^ (block 32 i mulbits))



main = main2

