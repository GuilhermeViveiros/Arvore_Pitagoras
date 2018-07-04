-- (c) MP-I (1998/9-2006/7) and CP (2005/6-2018/19)

module Main(main) where


import Graphics.Gloss
import Cp
import List
import Data.Monoid
import Control.Applicative
import Nat


data FTree a b = Unit b | Comp a (FTree a b) (FTree a b) deriving (Eq , Show)

type PTree = FTree Square Square
type Square = Float


--inPtree recebe Unit (inteiro) ou Comp (inteiro) PTree Ptree
inPTree :: Either Square (Square , (PTree ,PTree)) -> PTree
inPTree (Left x) = Unit x
inPTree (Right (x,(a,b)) ) = Comp x  a b

--recebe uma Ptree e pode dar tanto Unit ou Comp
outPTree :: PTree -> Either Square (Square , (PTree ,PTree))
outPTree (Unit x) = i1 x
outPTree (Comp x  a b) = i2 (x,(a,b)) 


-- (2) Ana + cata + hylo -------------------------------------------------------

--recPTree g = id -|- (id >< (g >< g))

--Versao simplificada 
inFTree (Left x) = Unit x
inFTree (Right (x,(a,b)) ) = Comp x a b

--outPTree :: PTree -> Either Square (Square , (PTree ,PTree))
outFTree (Unit x) = i1 x
outFTree (Comp x  a b) = i2 (x,(a,b))

recFTree = baseFTree id id

cataFTree g = g . recFTree (cataFTree g) . outFTree

anaFTree g = inFTree . recFTree (anaFTree g) . g

hyloFTree h g = h . recFTree ( hyloFTree h g ) . g

baseFTree f o g = o -|- (f >< (g >< g))

--Funcoes auxiliares---

instance BiFunctor FTree where
    bmap f g =  cataFTree ( inFTree . baseFTree f g id)



submax x = let valor  = either id p1 . outFTree  
                         in  uncurry (**) . 
                                    split (const (sqrt 2 / 2)) ( fromIntegral . uncurry (-) .
                                                                        split (const (valor x)) id )


--modify ptree aplica a funcao submax a esquerda e submax a direita da arovre

--modifyPTree tr = bmap (submax tr) (submax tr) tr
modifyPTree = ap.( (ap.(ap><id).assocl) >< id ) .
                                        (split (const bmap) (dup.submax) >< id ) . dup

--usamos um anamorfismo,pois atraves de um inteiro estamos a controir uma arvore 
--dado um inteiro entao o generat prioritariamente cria uma arvore com essa dimensao ou seja argumento x:
--o primeiro nodo tem dimensao x e os filhos x-1 e assim sucessivamente até chegar a 0 -> Unit's
--Após isso é aplicado a função modifyPTree na arvore , este modify oque faz basicamente é ir a arvore ,
--ve qual o maior valor e define em um x qualquer , após isso entao transformar a arvore segundo um funcao submax
--subamax elevada raiz de 2 / 2 ao valor(seria o numero maximo da arvore - numeroa atual do nodo(criado prioritariamente pela generatePtree))
generatePTree =  modifyPTree . anaFTree gene . toInteger
        where gene = ( zero -|- split succ (split id id) ) . outNat
              
              

--engage aplica praticamente a rotacao e a translacao das arvores , seja d o comprimento , entao oque este
--faz é subir a primeira arvore d e vai para a posicao d/2 para a arvore da direita
--na arvore da esquerda a posicao y té igual ou seja sobe d mas vai para -d/2  
engage = let pad x y = fmap . uncurry (.) . split (uncurry translate . split x id) (const (rotate (y 45)))
                     in split p1 ((uncurry (pad (negate.(/2)) negate) >< uncurry (pad (/2) id)) . split (id><p1) (id><p2))
---- engage (h,(ltr1,ltr2)) = concat process1 process2
          --  where process1 = fmap ( rotate 45 . translate h (negate (h/2)) ) ltr1
           --       process2 = fmap ( rotate (-45)  . translate h (h/2) ) ltr2 



--drawPTree recebido a arvore oque faz é transformar numa lista de Pictures
--dai o uso de um catamorfismo / -> admitindo que ja recebemos as arvores aqui
--Usamos o cataFree no gene onde o gene tranforma a arvore em pictures se recebesse so um Unit x entao era uma imeagem de dimensao x por x
--caso recebesse um Compa a PTree PTree ou seja (a,(b,d))  
--oque fazemos é juntamos o a numa lista com a concatenacao de b e d apos b e d sofrerem uma transformacao feita pelo engage
drawPTree :: PTree -> [Picture]
drawPTree = cataFTree gene
    where   gene = either (singl . square) 
                                ( cons . ( square >< conc ) . engage)



window = (InWindow "CP" (800,800) (0,0))
square s = rectangleSolid s s

main = animatePTree 10

animatePTree :: Integer -> IO ()
animatePTree n = animate window white draw
    where
    pics = pictures $ drawPTree (bmap (80*) (80*) (generatePTree n) )
    draw t = pics 









