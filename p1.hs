module P1 (main) where

    import AritmeticaModular
    import Data.Time.Clock

    -- http://stackoverflow.com/questions/2110419/how-to-get-system-time-in-haskell-using-data-time-clock
    getSeconds = getCurrentTime >>= return . fromRational . toRational . utctDayTime

    main = do
        putStrLn "Ejecutando Algoritmo extendido de Euclides..."
        let t1 = getSeconds
        let a = extended_euclides 458973489 123245
    --     let t2 = getSeconds
    --     putStrLn ("\t 458973489 123245 = " ++ (show a) ++ " " ++ (show (t2-t1)) ++ "s")
