module TableTags where

{- Represents an HTML tag -}
data TableTag = Table | Tr | Td | Th

{- give a lower case name of the tag, e.g., Td is "td" -}
showTableTag :: TableTag -> String
showTableTag Tr = "tr"
showTableTag Td = "td"
showTableTag Th = "th"
showTableTag Table = "table"

equalTableTags :: TableTag -> TableTag -> Bool
equalTableTags Tr Tr = True
equalTableTags Td Td = True
equalTableTags Th Th = True
equalTableTags Table Table = True
equalTableTags _ _ = False

{- no need to modify these two instances; complete the
above functions instead -}
instance Show TableTag where
  show = showTableTag

instance Eq TableTag where
  (==) = equalTableTags

{-
Takes two tags: returns True iff the second tag is allowed to be nested dir ectly inside of the first
-}
canBeDirectElement :: TableTag -> TableTag -> Bool
canBeDirectElement Table Tr = True
canBeDirectElement Tr Td = True
canBeDirectElement Tr Th = True
canBeDirectElement Td Table = True
canBeDirectElement Th Table = True
canBeDirectElement _ _ = False

{- TableHtml

   The object

     Element tag [ sub element 0 , ... , sub element n-1 ]

   represents an HTML element with the given tag and the given sub elements.

There are two base cases for TableHtml
   n = 0
   Text s

   Text s represents unstructured textual data s 
   See Tests for example TableHtml objects -}



data TableHtml = Element TableTag [TableHtml] | Text String

showTable :: TableHtml -> String
showTable (Text s) = s
showTable (Element table []) = putInside (showTableTag table) ""
showTable (Element table [Text s]) = putInside (showTableTag table) (showTable (Text s))
showTable (Element table (x:xs)) = putInside (showTableTag table) ((showTable x) ++ showMultTable xs)

putInside :: String -> String -> String
putInside tag content = "<" ++ tag ++ ">" ++ content ++ "</" ++ tag ++ ">"

getSubTable :: [TableHtml] -> TableTag
getSubTable ((Element table xs):ys) = table 

showMultTable :: [TableHtml] -> String
showMultTable [] = ""
showMultTable [x] = showTable x
showMultTable (x:xs) = showTable x ++ showMultTable xs

--showTable (Element Tr x:xs) = showTable x ++ showTable xs
{- no need to modify this instance; complete the above function instead -}
instance Show TableHtml where
  show = showTable 

{- returns true iff the TableHtml is "okay". To be okay means that
* all rows have the same length
* follows the rules about what can be directly nested in what

Simplification: technically a Td and Th can contain a Table. But you only
need to deal with one level of Table. If a Td or Th contains a table, you don't need to check it
-}
{-}
tableOk1 :: TableHtml -> [String]
tableOk1 (Text a) = [a]
tableOk1 (Element x y) = [(showTableTag x)] ++ (oktHelper y)
oktHelper :: [TableHtml] -> [String]
oktHelper [n] = tableOk1 n
oktHelper [] = []
oktHelper (n:ns) = tableOk1 n ++ (oktHelper ns)
tranList :: TableHtml -> [String]
tranList (xs) = [x | x <- (tail (tableOk1 xs)), x == "td" || x == "tr" || x == "th" || x =="table" ]
-}
tableOk :: TableHtml -> Bool
tableOk (Element Table xs) = (equalLength (Element Table xs)) && (rightSeq (Element Table xs))
tableOk _ = False

--Helper Function
rightSeq :: TableHtml -> Bool
rightSeq (Text s) = True
rightSeq (Element table []) = True
rightSeq (Element table [Text s]) = (table == Td) || (table == Th)
rightSeq (Element table ((Element table1 xs):ys)) = (canBeDirectElement table table1) && (rightSeq (Element table1 xs))

listTr :: String -> [String]
listTr "" = []
listTr [x] = ["x"]
listTr "tr" = ["tr"]
listTr "td" = ["td"]
listTr "th" = ["th"]
listTr (x:ys) = if ((x == 't') && ((head ys) == 'r')) then ["tr"] ++ (listTr ys)
  else if ((x == 't') && ((head ys) == 'd')) then ["td"] ++ (listTr ys)
  else if ((x == 't') && ((head ys) == 'h')) then ["td"] ++ (listTr ys)
  else listTr ys
positions :: Eq a => a  -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']
diffTag :: [Int] -> Bool
diffTag [] = False
diffTag [y,z] = True
diffTag (y:z:zs) = (((zs !! 1) - (zs !! 0)) == (z-y)) && diffTag zs
equalLength :: TableHtml -> Bool
equalLength xs = diffTag(positions "tr" (listTr(showTable(xs))))

--trueDirect :: [TableHtml] ->] Bool
--trueDirect (Element table x) y:ys = canBeDirectElement table  getSubTable(y) ++ canBeDirectElement 
{- 
Given a Table, row index, and column index, return that cell
-}

getCell :: TableHtml -> Int -> Int -> TableHtml
getCell tablehtml y x = getY (getX tablehtml x) y

getX :: TableHtml -> Int -> TableHtml
getX (Element Table xs) x = xs !! x

getY :: TableHtml -> Int -> TableHtml
getY (Element Tr ys) y = ys !! y 

{-
tableOk :: TableHtml -> Bool
tableOk (Element Table (x:xs)) = (equalLength x xs) && (trueDirect x xs)

equalLength :: TableHtml -> [TableHtml] -> Bool
equalLength Element table x xs  = (length x) == (length y)

trueDirect :: TableHtml -> [TableHtml] -> Bool
trueDirect (Element table x) y:ys = canBeDirectElement table  getSubTable(y) ++ canBeDirectElement 
-}