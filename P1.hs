module Main (main) where

    import Criterion.Main
    import System.Random
    import AritmeticaModular

    shanks_list :: (Integral a, Random a) => (a, a, a) -> [a]
    shanks_list (a,c,p) = baby_s_giant_s a c p

    -- Our benchmark harness.
    main = defaultMain [
        bgroup "miller_rabin" [ bench "46381"  $ whnf miller_rabin (46381 :: Int)
                   , bench "768479"  $ whnf miller_rabin (768479 :: Int)
                   , bench "9476407"  $ whnf miller_rabin (9476407 :: Int)
                   , bench "36780481" $ whnf miller_rabin (36780481 :: Int)
                   , bench "562390847" $ whnf miller_rabin (562390847 :: Int)
                   , bench "1894083629" $ whnf miller_rabin (1894083629 :: Int)
                   , bench "65398261921" $ whnf miller_rabin (65398261921 :: Int)
                   , bench "364879542899" $ whnf miller_rabin (364879542899 :: Int)
                   , bench "8590365927553" $ whnf miller_rabin (8590365927553 :: Int)
                   , bench "28564333765949" $ whnf miller_rabin (28564333765949 :: Int)
                   , bench "123456789101119" $ whnf miller_rabin (123456789101119 :: Int)
                   ],
        bgroup "shanks" [ bench "46381"  $ whnf shanks_list ((123456 :: Int), (1749924 :: Int), (46381 :: Int))
                   , bench "768479"  $ whnf shanks_list ((123456 :: Int), (1749924 :: Int), (768479 :: Int))
                   , bench "9476407"  $ whnf shanks_list ((123456 :: Int), (1749924 :: Int), (9476407 :: Int))
                   , bench "36780481" $ whnf shanks_list ((123456 :: Int), (1749924 :: Int), (36780481 :: Int))
                   , bench "562390847" $ whnf shanks_list ((123456 :: Int), (1749924 :: Int), (562390847 :: Int))
                   , bench "1894083629" $ whnf shanks_list ((123456 :: Int), (1749924 :: Int), (1894083629 :: Int))
                   , bench "65398261921" $ whnf shanks_list ((123456 :: Int), (1749924 :: Int), (65398261921 :: Int))
                   , bench "364879542899" $ whnf shanks_list ((123456 :: Int), (1749924 :: Int), (364879542899 :: Int))
                   , bench "8590365927553" $ whnf shanks_list ((123456 :: Int), (1749924 :: Int), (8590365927553 :: Int))
                   , bench "28564333765949" $ whnf shanks_list ((123456 :: Int), (1749924 :: Int), (28564333765949 :: Int))
                   , bench "123456789101119" $ whnf shanks_list ((123456 :: Int), (1749924 :: Int), (123456789101119 :: Int))
                    ]
        ]
