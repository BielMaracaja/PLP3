main = do
  entrada1 <- getLine
  print (retornaTuplas (read entrada1))

capacity :: String -> [String] -> Int
capacity word [] = 0
capacity word (head:body) =
    if head == word then 
        1 + capacity word body
    else 
        0 + capacity word body
 
excluiElemento :: String -> [String] -> [String]
excluiElemento word [] = []
excluiElemento word lista = [p | p <- lista, p /= word]
 
retornaTuplas :: [String] -> [(String, Int)]
retornaTuplas [] = []
retornaTuplas (head:body) = do
    let quantidade = (capacity head body) + 1
    let lista = excluiElemento head body
    [(head, quantidade)] ++ retornaTuplas lista

