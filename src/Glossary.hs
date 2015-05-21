{-
----
title: Глоссарий
prevChapter: /ru/appendix/index.html
nextChapter: /ru/index.html
----
[О форматировании](/ru/miscellaneous/about-formatting.html)

Данный документ где то здесь:
Чем является конструкция ----------------------------------------------*--- Что делает конструкция в этой книге

Предназначен он не для того, чтобы в подробностях узнать что делает та или иная конструкция, поскольку классификация ограничена материалом этой книги. Для подробностей есть другие ресурсы. Это указатель для облегченной навигации. 
-}
module Glossary (
    instructions
    ,functions
    ,dirWalk
) where 
    
    
import System.FilePath
import System.Directory
import Control.Monad 
import Data.List.Split
import qualified Data.List as L



pathToChapters = "../chapters/ru"

type Contents = String


--ищем во всех поддиректориях файлики с расширением .md
dirWalk :: FilePath -> IO [FilePath]
dirWalk top   = do
  isDirectory <- doesDirectoryExist top
  if isDirectory
    then do
      files <- getDirectoryContents top
      let nonDotFiles = filter (not . (`elem` [".", ".."])) files 
      mdFilesFrom <-  mapM (\file -> dirWalk (top </> file)) nonDotFiles
      let mdFiles = (fmap (top </>) $ filter (L.isSuffixOf ".md") nonDotFiles) ++ concat mdFilesFrom
      return mdFiles      
  else do
    return []


--получаем список файлов в которых встречается конструкция
 
corresponds :: FilePath -> [Construction] -> IO [Correspond]
corresponds path  constrs = do 
    mDs <- dirWalk pathToChapters
   
    return $ fmap (\md -> do
            hs <- hsInFile md
            return $ fmap  (\constr -> Corr (head (tags constr)) (inHackage constr) path) (filter (hasMatches hs) constrs)
            ) mDs
        
 

--Встречается ли конструкция в данном тексте?
hasMatches :: Constructions a => HsCode -> a -> Bool
hasMatches code constr = 
    elem  (inBook constr)  (words code )            

--забираем хаскельный код из файла        
hsInFile :: FilePath -> IO HsInFile 
hsInFile fspath = do
    file <- readFile fspath
    return $ foldl1 (++) $ tail $ fmap head $ fmap  (splitOn "'''") $ splitOn "'''haskell" file


    
        
unpackConstructions :: Constructions a => [a] -> [a]
unpackConstructions (x:xs) =  unpackConstructions xs ++ reconstruct x
unpackConstructions (x:[]) = [x]



--представление конструкций
type FullName = String
type NameInBook = String
type Tag = String


data Construction       = C FullName NameInBook [Tag]

data Correspond     = Corr Tag FullName FilePath --где встречается

type HsCode         = String
type HsInFile       = String

class Constructions c where
    inBook :: c -> NameInBook
    inHackage :: c -> FullName
    tags :: c -> [Tag]
    reconstruct :: c -> [c]
    
instance Constructions Construction where
    inBook (C _ a _) =      a
    inHackage (C a _ _) =   a
    tags (C _ _ a) =        a
    {-    reconstruct C "a" "a" ["1", "2"] = [C "a" "a" ["1"] , C "a" "a" ["2"]]    -}
    reconstruct (C n1 n2 (x:xs)) =  reconstruct (C n1 n2 xs) ++ [C n1 n2 [x]]
    reconstruct (C n1 n2 (x:[])) =  [C n1 n2 [x]]




    

constructions =  
    [C "type"                               "type"              ["Инструкция", "Типы"]
    ,C "data"                               "data"              ["Инструкция", "Типы"]
    ,C "newtype"                            "newtype"           ["Инструкция", "Типы"]
    ,C "class"                              "class"             ["Инструкция", "Типы"]
    ,C "instance"                           "instance"          ["Инструкция", "Типы"]
    ,C "deriving"                           "deriving"          ["Инструкция", "Типы"]
                                                                
    ,C "case"                               "case"              ["Инструкция", "Логика"]
    ,C "if"                                 "if"                ["Инструкция", "Логика"]
    ,C "then"                               "then"              ["Инструкция", "Логика"]
    ,C "else"                               "else"              ["Инструкция", "Логика"]
    ,C "|"                                  "|"                 ["Инструкция", "Логика"]
    ,C "\\"                                 "\\"                ["Инструкция", "Подвыражения"]--это лямбда(?)
    ,C "let"                                "let"               ["Инструкция", "Подвыражения"]
    ,C "in"                                 "in"                ["Инструкция", "Подвыражения"]
    ,C "where"                              "where"             ["Инструкция", "Подвыражения"]    
    ,C "module"                             "module"            ["Инструкция", "Модули"]
    ,C "import"                             "import"            ["Инструкция", "Модули"]
    ,C "hiding"                             "hiding"            ["Инструкция", "Модули"]
    ,C "as"                                 "as"                ["Инструкция", "Модули"]
    ,C "qualified"                          "qualified"         ["Инструкция", "Модули"]
    ,C "->"                                 "->"                ["Инструкция", "Всякое нужное"]
    ,C "<-"                                 "<-"                ["Инструкция", "Всякое нужное"]
    ,C "::"                                 "::"                ["Инструкция", "Всякое нужное"]
                                                                
    ,C "do"                                 "do"                ["Инструкция", "Всякое нужное"] 
    ,C "="                                  "="                 ["Инструкция", "Всякое нужное"] 
    
    ,C "Prelude.maxBound"                   "maxBound"          ["Функция","Типы"]
    ,C "Prelude.minBound"                   "minBound"          ["Функция","Типы"]
    ,C "Prelude.elem"                       "elem"              ["Функция","Списки", "Строки"]
    ,C "Prelude.filter"                     "filter"            ["Функция","Списки", "Строки"]
    ,C "Prelude.length"                     "length"            ["Функция","Списки", "Строки"]
    ,C "Prelude.takeWhile"                  "takeWhile"         ["Функция","Списки", "Строки"]
    ,C "Prelude.foldl"                      "foldl"             ["Функция","Списки"]
    ,C "Prelude.foldr"                      "foldr"             ["Функция","Списки"]
    ,C "Prelude.foldl1"                     "foldl1"            ["Функция","Списки"]
    ,C "Prelude.scanl"                      "scanl"             ["Функция","Списки"]
    ,C "Prelude.scanr"                      "scanr"             ["Функция","Списки"]
    ,C "Prelude.take"                       "take"              ["Функция","Списки", "Строки"]
    ,C "Prelude.replicate"                  "replicate"         ["Функция","Списки", "Строки"]
    ,C "Prelude.(++)"                       "++"                ["Функция","Списки", "Строки"]
    ,C "Prelude.map"                        "map"               ["Функция","Списки"]
    ,C "Prelude.fmap"                       "fmap"              ["Функция","Списки"]
    ,C "Data.Functor.(<$>)"                 "<$>"               ["Функция","Списки"]
    ,C "Prelude.(:)"                        ":"                 ["Функция","Списки"]
    ,C "Prelude.repeat"                     "repeat"            ["Функция","Списки", "Строки"]
    ,C "Prelude.!!"                         "!!"                ["Функция","Списки", "Строки"]
    ,C "Data.List.Split.splitOn"            "splitOn"           ["Функция","Списки"]
                                        
    ,C "Data.Map.Lazy.fromList"             "M.fromList"        ["Функция","Словари"]
    ,C "Data.Map.Lazy.lookup"               "M.lookup"          ["Функция","Словари"]
    ,C "Data.Map.Lazy.empty"                "M.empty"           ["Функция","Словари"]
    ,C "Data.Map.Lazy.insert"               "M.insert"          ["Функция","Словари"]
    ,C "Data.Map.Lazy.insertWith"           "M.insertWith"      ["Функция","Словари"]
    ,C "Data.Map.Lazy.adjust"               "M.adjust"          ["Функция","Словари"]
    ,C "Data.Map.Lazy.delete"               "M.delete"          ["Функция","Словари"]
    ,C "Data.Map.Lazy.elems"                "M.elems"           ["Функция","Словари"]
                                         
    ,C "Data.Set.size"                      "S.size"            ["Функция","Множества(Set)"]
    ,C "Data.Set.fromList"                  "S.fromList"        ["Функция","Множества(Set)"]
                                         
    ,C "Data.Char.toLower"                  "toLower"           ["Функция","Строки"]
    ,C "Data.Char.toUpper"                  "toUpper"           ["Функция","Строки"]
    ,C "Data.Char.digitToInt"               "digitToInt"        ["Функция","Строки"]
    ,C "Data.String.Utils.replace"          "replace"           ["Функция","Строки"]
    ,C "Prelude.read"                       "read"              ["Функция","Строки"]
    ,C "Prelude.show"                       "show"              ["Функция","Строки"]
                                              
    ,C "Data.String.Utils.startswith"       "startswith"        ["Функция","Строки"]
                                       
    ,C "Prelude.concat"                     "concat"            ["Функция","Строки"]
                                             
    ,C "Prelude.not"                        "not"               ["Функция","Логика"]
    ,C "Prelude.(==)"                       "=="                ["Функция","Логика"]
    ,C "Prelude.(/=)"                       "/="                ["Функция","Логика"]
    ,C "Prelude.(&&)"                       "&&"                ["Функция","Логика"]
                                                 
    ,C "Prelude.null"                       "null"              ["Функция","Логика","Строки"]
    ,C "Data.Char.isNumber"                 "isNumber"          ["Функция","Логика","Строки"]
                                            
    ,C "Data.Maybe.isJust"                  "isJust"            ["Функция","Исключения"]
    ,C "Data.Maybe.isNothing"               "isNothing"         ["Функция","Исключения"]
    ,C "Data.List.isPrefixOf"               "isPrefixOf"        ["Функция","Логика","Строки"]
    ,C "Data.List.isSuffixOf"               "isSuffixOf"        ["Функция","Логика","Строки"]
                                                   
    ,C "Prelude.print"                      "print"             ["Функция","Ввод-вывод"]
    ,C "Prelude.putStrLn"                   "putStrLn"          ["Функция","Ввод-вывод", "Строки"]
    ,C "Prelude.getLine"                    "getLine"           ["Функция","Ввод-вывод", "Строки"]
                                                      
    ,C "Prelude.(+)"                        "+"                 ["Функция","Арифметика"]
    ,C "Prelude.(*)"                        "*"                 ["Функция","Арифметика"]
    ,C "Prelude.(-)"                        "-"                 ["Функция","Арифметика"]
    ,C "Prelude.(/)"                        "/"                 ["Функция","Арифметика"]
    ,C "Prelude.div"                        "div"               ["Функция","Арифметика"]
                                                        
    ,C "Prelude.fst"                        "fst"               ["Функция","Кортежи"]
    ,C "Prelude.snd"                        "snd"               ["Функция","Кортежи"]
    ,C "Data.Tuple.Select.sel3"             "sel3"              ["Функция","Кортежи"]
                                                           
    ,C "Prelude.($)"                        "$"                 ["Функция","Композиция"]
    ,C "Prelude.(.)"                        "."                 ["Функция","Композиция"]
                                                          
    ,C "Control.Exception.catch"            "catch"             ["Функция","Исключения"] 
    ,C "Control.Exception.handle"           "handle"            ["Функция","Исключения"]
    ,C "Control.Exception.try"              "try"               ["Функция","Исключения"]
    ,C "Control.Exception.throw"            "throw"             ["Функция","Исключения"]
    ,C "Control.Monad.Error.runErrorT"      "runErrorT"         ["Функция","Исключения"]
    ,C "Control.Monad.Error.throwError"     "throwError"        ["Функция","Исключения"]
    ,C "Control.Monad.Trans.Either.left"    "left"              ["Функция","Исключения"]
    ,C "Control.Monad.Trans.Either.right"   "right"             ["Функция","Исключения"]
    ,C "Data.Maybe.fromJust"                "fromJust"          ["Функция","Исключения"]]
    
{-Section "Веб"
    ["fPutStr"
    ,"acceptLoop"
    ,"getQueryString"
    ,"setResponseStatus"
    ,"setResponseHeader"],
            
Section "Файлы" 
    ["takeExtension"
    ,"readFile"
    ,"getModificationTime"
    ,"fileSize"
    ,"getFileStatus"],

Section "Операционная система" 
    ["lookupEnv"],

Section "Всякое нужное" 
    [
    ,"return"
    ,"evaluate"
    ,">>="
    ,">>"
    ,"=<<"
    ,"<<"
    ,"<*>"
    ,"<*"
    ,"*>"
    ,"pure"
    ,"lift"
    ,"liftIO"
    ,"guard"
    ,"runMaybeT"
    ,"runReaderT"
    ,"runReader"
    ,"runWriterT"
    ,"execWriterT"
    ,"execStateT"
    ,"runEitherT"
    ,"ask"
    ,"tell"
    ,"modify"]
]-}



