module Main (main) where

    import Criterion.Main
    import System.Random
    import AritmeticaModular

    shanks_list :: (Integral a, Random a) => (a, a, a) -> a
    shanks_list (a,c,p) = baby_s_giant_s a c p

    cuadrados_list :: (Integral a, Random a) => (a,a) -> (a,a)
    cuadrados_list (a,p) = cuadrados a p

    raices_list :: (Integral a, Random a) => (a,a,a) -> (a,a,a,a)
    raices_list (r,p,q) = raices_cuadradas r p q

    rho_f :: (Integral a, Random a) => a -> (a,a)
    rho_f n = rho n func

    -- Our benchmark harness.
    main = defaultMain [
        bgroup "miller_rabin" [ bench "46381"  $ whnf miller_rabin (46381 :: Integer)
                   , bench "768479"  $ whnf miller_rabin (768479 :: Integer)
                   , bench "9476407"  $ whnf miller_rabin (9476407 :: Integer)
                   , bench "36780481" $ whnf miller_rabin (36780481 :: Integer)
                   , bench "562390847" $ whnf miller_rabin (562390847 :: Integer)
                   , bench "1894083629" $ whnf miller_rabin (1894083629 :: Integer)
                   , bench "65398261921" $ whnf miller_rabin (65398261921 :: Integer)
                   , bench "364879542899" $ whnf miller_rabin (364879542899 :: Integer)
                   , bench "8590365927553" $ whnf miller_rabin (8590365927553 :: Integer)
                   , bench "28564333765949" $ whnf miller_rabin (28564333765949 :: Integer)
                   , bench "123456789101119" $ whnf miller_rabin (123456789101119 :: Integer)
                   ],
        bgroup "shanks" [ bench "46381"  $ whnf shanks_list ((123456 :: Integer), (1749924 :: Integer), (46381 :: Integer))
                   , bench "768479"  $ whnf shanks_list ((123456 :: Integer), (1749924 :: Integer), (768479 :: Integer))
                   , bench "9476407"  $ whnf shanks_list ((123456 :: Integer), (1749924 :: Integer), (9476407 :: Integer))
                   , bench "36780481" $ whnf shanks_list ((123456 :: Integer), (1749924 :: Integer), (36780481 :: Integer))
                   , bench "562390847" $ whnf shanks_list ((123456 :: Integer), (1749924 :: Integer), (562390847 :: Integer))
                   , bench "1894083629" $ whnf shanks_list ((123456 :: Integer), (1749924 :: Integer), (1894083629 :: Integer))
                   , bench "65398261921" $ whnf shanks_list ((123456 :: Integer), (1749924 :: Integer), (65398261921 :: Integer))
                   , bench "364879542899" $ whnf shanks_list ((123456 :: Integer), (1749924 :: Integer), (364879542899 :: Integer))
                   , bench "8590365927553" $ whnf shanks_list ((123456 :: Integer), (1749924 :: Integer), (8590365927553 :: Integer))
                   , bench "28564333765949" $ whnf shanks_list ((123456 :: Integer), (1749924 :: Integer), (28564333765949 :: Integer))
                   , bench "123456789101119" $ whnf shanks_list ((123456 :: Integer), (1749924 :: Integer), (123456789101119 :: Integer))
                    ],
        bgroup "cuadrados" [ bench "46381"  $ whnf cuadrados_list ((123456 :: Integer), (46381 :: Integer))
                   , bench "768479"  $ whnf cuadrados_list ((123457 :: Integer), (768479 :: Integer))
                   , bench "9476407"  $ whnf cuadrados_list ((123456 :: Integer), (9476407 :: Integer))
                   , bench "36780481" $ whnf cuadrados_list ((123456 :: Integer), (36780481 :: Integer))
                   , bench "562390847" $ whnf cuadrados_list ((123456 :: Integer), (562390847 :: Integer))
                   , bench "1894083629" $ whnf cuadrados_list ((123456 :: Integer), (1894083629 :: Integer))
                   , bench "65398261921" $ whnf cuadrados_list ((123457 :: Integer), (65398261921 :: Integer))
                   , bench "364879542899" $ whnf cuadrados_list ((123457 :: Integer), (364879542899 :: Integer))
                   , bench "8590365927553" $ whnf cuadrados_list ((123456 :: Integer), (8590365927553 :: Integer))
                   , bench "28564333765949" $ whnf cuadrados_list ((123460 :: Integer), (28564333765949 :: Integer))
                   , bench "123456789101119" $ whnf cuadrados_list ((123458 :: Integer), (123456789101119 :: Integer))
                    ],
        bgroup "raices" [ bench "46381"  $ whnf raices_list ((123456 :: Integer), (46381 :: Integer), (46381 :: Integer))
                   , bench "768479"  $ whnf raices_list ((123457 :: Integer), (46381 :: Integer), (768479 :: Integer))
                   , bench "9476407"  $ whnf raices_list ((123456 :: Integer), (46381 :: Integer), (9476407 :: Integer))
                   , bench "36780481" $ whnf raices_list ((123456 :: Integer), (46381 :: Integer), (36780481 :: Integer))
                   , bench "562390847" $ whnf raices_list ((123456 :: Integer), (46381 :: Integer), (562390847 :: Integer))
                   , bench "1894083629" $ whnf raices_list ((123456 :: Integer), (46381 :: Integer), (1894083629 :: Integer))
                   , bench "65398261921" $ whnf raices_list ((123457 :: Integer), (46381 :: Integer), (65398261921 :: Integer))
                   , bench "364879542899" $ whnf raices_list ((123457 :: Integer), (46381 :: Integer), (364879542899 :: Integer))
                   , bench "8590365927553" $ whnf raices_list ((123456 :: Integer), (46381 :: Integer), (8590365927553 :: Integer))
                   , bench "28564333765949" $ whnf raices_list ((123460 :: Integer), (46381 :: Integer), (28564333765949 :: Integer))
                   , bench "123456789101119" $ whnf raices_list ((123458 :: Integer), (46381 :: Integer), (123456789101119 :: Integer))
                    ],
        bgroup "metodo_fermat" [ bench "46381"  $ whnf metodo_fermat (46381 :: Integer)
                   , bench "768479"  $ whnf metodo_fermat (768479 :: Integer)
                   , bench "9476407"  $ whnf metodo_fermat (9476407 :: Integer)
                   , bench "36780481" $ whnf metodo_fermat (36780481 :: Integer)
                   , bench "562390847" $ whnf metodo_fermat (562390847 :: Integer)
                   , bench "1894083629" $ whnf metodo_fermat (1894083629 :: Integer)
                   , bench "65398261921" $ whnf metodo_fermat (65398261921 :: Integer)
                   , bench "364879542899" $ whnf metodo_fermat (364879542899 :: Integer)
                   , bench "8590365927553" $ whnf metodo_fermat (8590365927553 :: Integer)
                   , bench "28564333765949" $ whnf metodo_fermat (28564333765949 :: Integer)
                   , bench "123456789101119" $ whnf metodo_fermat (123456789101119 :: Integer)
                   ],
        bgroup "rho" [ bench "46381"  $ whnf rho_f (46381 :: Integer)
                   , bench "768479"  $ whnf rho_f (768479 :: Integer)
                   , bench "9476407"  $ whnf rho_f (9476407 :: Integer)
                   , bench "36780481" $ whnf rho_f (36780481 :: Integer)
                   , bench "562390847" $ whnf rho_f (562390847 :: Integer)
                   , bench "1894083629" $ whnf rho_f (1894083629 :: Integer)
                   , bench "65398261921" $ whnf rho_f (65398261921 :: Integer)
                   , bench "364879542899" $ whnf rho_f (364879542899 :: Integer)
                   , bench "8590365927553" $ whnf rho_f (8590365927553 :: Integer)
                   , bench "28564333765949" $ whnf rho_f (28564333765949 :: Integer)
                   , bench "123456789101119" $ whnf rho_f (123456789101119 :: Integer)
                   ]
        ]
