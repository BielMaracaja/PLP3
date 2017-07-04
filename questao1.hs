main = do
	lista <- getLine
	indice <- getLine
	let saida = retornaIesimo (read lista) (read indice)
	print saida


retornaIesimo :: [Int] -> Int -> Int
retornaIesimo lista indice = lista !! (indice-1)
 
 
