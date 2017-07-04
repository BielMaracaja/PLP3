main = do
  entrada1 <- getLine
  entrada2 <- getLine
  let saida = juntaListas (read entrada1) (read entrada2)
  print saida
 
juntaListas :: [Int] -> [Int] -> [Int]
juntaListas [] [] = []
juntaListas (a:as) [] = (a:as)
juntaListas (a:as)(b:bs) | verificaElemento(a:as) b = juntaListas (a:as) bs
   					  | otherwise = juntaListas((a:as)++[b]) bs
                    	 
verificaElemento :: [Int] -> Int -> Bool
verificaElemento [] n = False
verificaElemento (a:as) n  | a == n = True
   					 | otherwise = verificaElemento as n
