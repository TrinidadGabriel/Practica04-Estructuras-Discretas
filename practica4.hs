data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

-------------------- EJERCICIO 1 --------------------
longitud :: Arbol a -> Int 
longitud ArbolVacio = 0
longitud (Raiz _ izq der) = 1 + longitud izq + longitud der  

-------------------- EJERCICIO 2 --------------------
profundidad :: Arbol a -> Int 
profundidad ArbolVacio = 0
profundidad (Raiz _ izq der) = 1 + max (profundidad izq) (profundidad der)

-------------------- EJERCICIO 3 --------------------
ancho :: Arbol a -> Int 
ancho ArbolVacio = 0
ancho (Raiz _ ArbolVacio ArbolVacio) = 1
ancho (Raiz _ izq der) = ancho izq + ancho der

-------------------- EJERCICIO 4 --------------------
data Recorrido = InOrder | PreOrder | PosOrder

recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolVacio _ = []
recorrido (Raiz x izq der) InOrder = recorrido izq InOrder ++ [x] ++ recorrido der InOrder
recorrido (Raiz x izq der) PreOrder = [x] ++ recorrido izq PreOrder ++ recorrido der PreOrder
recorrido (Raiz x izq der) PosOrder = recorrido izq PosOrder ++ recorrido der PosOrder ++ [x]


-------------------- EJERCICIO 5 --------------------
niveles :: Arbol a -> [[a]]
niveles ArbolVacio = []
niveles (Raiz raiz ArbolVacio ArbolVacio) = [[raiz]]
niveles (Raiz raiz arbolIzquierdo arbolDerecho) = 
    [raiz] : combinarNiveles (niveles arbolIzquierdo) (niveles arbolDerecho)

-- Función auxiliar para combinar niveles de dos listas de niveles
combinarNiveles :: [[a]] -> [[a]] -> [[a]]
combinarNiveles [] ys = ys
combinarNiveles xs [] = xs
combinarNiveles (x:xs) (y:ys) = (x ++ y) : combinarNiveles xs ys


-------------------- EJERCICIO 6 --------------------
minimo :: Ord a => Arbol a -> a
minimo (Raiz valor ArbolVacio _) = valor
minimo (Raiz _ izquierda _) = minimo izquierda

-------------------- EJERCICIO 7 --------------------
maximo :: Ord a => Arbol a -> a
maximo (Raiz valor _ ArbolVacio) = valor
maximo (Raiz _ _ derecha) = maximo derecha


-------------------- EJERCICIO 8 --------------------
eliminar :: Ord a => Arbol a -> a -> Arbol a
eliminar ArbolVacio _ = ArbolVacio
eliminar (Raiz valor izquierda derecha) x
    | x < valor = Raiz valor (eliminar izquierda x) derecha
    | x > valor = Raiz valor izquierda (eliminar derecha x)
    | x == valor = case (izquierda, derecha) of
        (ArbolVacio, _) -> derecha
        (_, ArbolVacio) -> izquierda
        _ -> let nuevoValor = maximo izquierda
             in Raiz nuevoValor (eliminar izquierda nuevoValor) derecha
