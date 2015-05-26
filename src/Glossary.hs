{-
----
title: Глоссарий
prevChapter: /ru/appendix/index.html
nextChapter: /ru/index.html
----
[О форматировании](/ru/miscellaneous/about-formatting.html)

-}
module Glossary (
    constructions
    ,corresponds
    ,pathToChapters
    ,dirWalk
    ,hasMatches
    ,hsInFile
) where 
    
    
import System.FilePath
import System.Directory
import Control.Monad 
import Data.List.Split
import qualified Data.List as L



pathToChapters = "../chapters/ru/"


--представление конструкций
type FullName = String
type NameInBook = String
type Tag = String


data Construction       = C FullName NameInBook [Tag] deriving Eq

data Correspond     = Corr Tag FullName FilePath  deriving (Show, Eq)--где встречается конструкция

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
    reconstruct (C n1 n2 (x:[])) =  [C n1 n2 [x]]
    reconstruct (C n1 n2 (x:xs)) =  reconstruct (C n1 n2 xs) ++ [C n1 n2 [x]]
{-
--
instance  Ord Construction  where
    --compare              :: a -> a -> Ordering
    --(<), (<=), (>=), (>) :: a -> a -> Bool
    --max, min             :: a -> a -> a

        -- Minimal complete definition:
        --      (<=) or compare
        -- Using compare can be more efficient for complex types.
    compare x y
         | inBook x == inBook y    =  EQ
         | inBook x <= inBook y    =  LT
         | otherwise =  GT

    x <= y           =  compare x y /= GT
    x <  y           =  compare x y == LT
    x >= y           =  compare x y /= LT
    x >  y           =  compare x y == GT

-- note that (min x y, max x y) = (x,y) or (y,x)
    max x y 
         | x >= y    =  x
         | otherwise =  y
    min x y
         | x <  y    =  x
         | otherwise =  y-}
    
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
    
 --забираем хаскельный код из файла        
hsInFile :: FilePath -> IO HsInFile 
hsInFile fspath = do
    file <- readFile fspath
    return $ foldl (++) [] $ tail $ fmap head $ fmap  (splitOn "```") $ splitOn "```haskell" file   
    
--Встречается ли конструкция в данном тексте?
hasMatches :: Constructions a => HsCode -> a -> Bool
hasMatches code constr = 
    elem  (inBook constr)  (words code )            

--получаем список файлов в которых встречается конструкция
 
corresponds :: FilePath -> [Construction] -> IO [Correspond]
corresponds path  constrs = do 
    mDs <- dirWalk pathToChapters
    corrs <- mapM (\md -> do
            hs <- hsInFile md
            mapM (\constr -> return $ Corr (head (tags constr)) (inHackage constr) md) (filter (hasMatches hs) constrs)
            ) mDs
    return . concat $ corrs
 

--список конструкций
constructions =  
    [C "<<"                                         "<<"                    ["Инструкции"]
    ,C "type"                                       "type"                  ["Инструкции", "Типы"]
    ,C "data"                                       "data"                  ["Инструкции", "Типы"]
    ,C "newtype"                                    "newtype"               ["Инструкции", "Типы"]
    ,C "class"                                      "class"                 ["Инструкции", "Типы"]
    ,C "instance"                                   "instance"              ["Инструкции", "Типы"]
    ,C "deriving"                                   "deriving"              ["Инструкции", "Типы"]
    ,C "case"                                       "case"                  ["Инструкции", "Логика"]
    ,C "if"                                         "if"                    ["Инструкции", "Логика"]
    ,C "then"                                       "then"                  ["Инструкции", "Логика"]
    ,C "else"                                       "else"                  ["Инструкции", "Логика"]
    ,C "|"                                          "|"                     ["Инструкции", "Логика"]
    ,C "let"                                        "let"                   ["Инструкции", "Подвыражения"]
    ,C "in"                                         "in"                    ["Инструкции", "Подвыражения"]
    ,C "where"                                      "where"                 ["Инструкции", "Подвыражения"]    
    ,C "module"                                     "module"                ["Инструкции", "Модули"]
    ,C "import"                                     "import"                ["Инструкции", "Модули"]
    ,C "hiding"                                     "hiding"                ["Инструкции", "Модули"]
    ,C "as"                                         "as"                    ["Инструкции", "Модули"]
    ,C "qualified"                                  "qualified"             ["Инструкции", "Модули"]
    ,C "->"                                         "->"                    ["Инструкции"]
    ,C "<-"                                         "<-"                    ["Инструкции"]
    ,C "::"                                         "::"                    ["Инструкции"]
    ,C "do"                                         "do"                    ["Инструкции"] 
    ,C "="                                          "="                     ["Инструкции"] 
    ,C "Prelude.maxBound"                           "maxBound"              ["Функции", "Типы"]
    ,C "Prelude.minBound"                           "minBound"              ["Функции", "Типы"]
    ,C "Prelude.elem"                               "elem"                  ["Функции", "Списки", "Строки"]
    ,C "Prelude.filter"                             "filter"                ["Функции", "Списки", "Строки"]
    ,C "Prelude.length"                             "length"                ["Функции", "Списки", "Строки"]
    ,C "Prelude.takeWhile"                          "takeWhile"             ["Функции", "Списки", "Строки"]
    ,C "Prelude.foldl"                              "foldl"                 ["Функции", "Списки"]
    ,C "Prelude.foldr"                              "foldr"                 ["Функции", "Списки"]
    ,C "Prelude.foldl1"                             "foldl1"                ["Функции", "Списки"]
    ,C "Prelude.scanl"                              "scanl"                 ["Функции", "Списки"]
    ,C "Prelude.scanr"                              "scanr"                 ["Функции", "Списки"]
    ,C "Prelude.take"                               "take"                  ["Функции", "Списки", "Строки"]
    ,C "Prelude.replicate"                          "replicate"             ["Функции", "Списки", "Строки"]
    ,C "Prelude.(++)"                               "++"                    ["Функции", "Списки", "Строки"]
    ,C "Prelude.map"                                "map"                   ["Функции", "Списки"]
    ,C "Prelude.fmap"                               "fmap"                  ["Функции", "Списки"]
    ,C "Data.Functor.(<$>)"                         "<$>"                   ["Функции", "Списки"]
    ,C "Prelude.(:)"                                ":"                     ["Функции", "Списки"]
    ,C "Prelude.repeat"                             "repeat"                ["Функции", "Списки", "Строки"]
    ,C "Prelude.!!"                                 "!!"                    ["Функции", "Списки", "Строки"]
    ,C "Data.List.Split.splitOn"                    "splitOn"               ["Функции", "Списки"]
    ,C "Data.Map.Lazy.fromList"                     "M.fromList"            ["Функции", "Словари"]
    ,C "Data.Map.Lazy.lookup"                       "M.lookup"              ["Функции", "Словари"]
    ,C "Data.Map.Lazy.empty"                        "M.empty"               ["Функции", "Словари"]
    ,C "Data.Map.Lazy.insert"                       "M.insert"              ["Функции", "Словари"]
    ,C "Data.Map.Lazy.insertWith"                   "M.insertWith"          ["Функции", "Словари"]
    ,C "Data.Map.Lazy.adjust"                       "M.adjust"              ["Функции", "Словари"]
    ,C "Data.Map.Lazy.delete"                       "M.delete"              ["Функции", "Словари"]
    ,C "Data.Map.Lazy.elems"                        "M.elems"               ["Функции", "Словари"]
    ,C "Data.Map.Lazy.map"                          "M.mao"                 ["Функции", "Словари"]
    ,C "Data.Set.size"                              "S.size"                ["Функции", "Множества(Set)"]
    ,C "Data.Set.fromList"                          "S.fromList"            ["Функции", "Множества(Set)"]
    ,C "Data.Char.toLower"                          "toLower"               ["Функции", "Строки"]
    ,C "Data.Char.toUpper"                          "toUpper"               ["Функции", "Строки"]
    ,C "Data.Char.digitToInt"                       "digitToInt"            ["Функции", "Строки"]
    ,C "Data.String.Utils.replace"                  "replace"               ["Функции", "Строки"]
    ,C "Prelude.read"                               "read"                  ["Функции", "Строки"]
    ,C "Prelude.show"                               "show"                  ["Функции", "Строки"]
    ,C "Data.String.Utils.startswith"               "startswith"            ["Функции", "Строки"]
    ,C "Prelude.concat"                             "concat"                ["Функции", "Строки"]
    ,C "Prelude.not"                                "not"                   ["Функции", "Логика"]
    ,C "Prelude.(==)"                               "=="                    ["Функции", "Логика"]
    ,C "Prelude.(/=)"                               "/="                    ["Функции", "Логика"]
    ,C "Prelude.(&&)"                               "&&"                    ["Функции", "Логика"]
    ,C "Prelude.null"                               "null"                  ["Функции", "Логика","Строки"]
    ,C "Data.Char.isNumber"                         "isNumber"              ["Функции", "Логика","Строки"]
    ,C "Data.Maybe.isJust"                          "isJust"                ["Функции", "Исключения"]
    ,C "Data.Maybe.isNothing"                       "isNothing"             ["Функции", "Исключения"]
    ,C "Data.List.isPrefixOf"                       "isPrefixOf"            ["Функции", "Логика","Строки"]
    ,C "Data.List.isSuffixOf"                       "isSuffixOf"            ["Функции", "Логика","Строки"]
    ,C "Prelude.print"                              "print"                 ["Функции", "Ввод-вывод"]
    ,C "Prelude.putStrLn"                           "putStrLn"              ["Функции", "Ввод-вывод", "Строки"]
    ,C "Prelude.getLine"                            "getLine"               ["Функции", "Ввод-вывод", "Строки"]
    ,C "Prelude.(+)"                                "+"                     ["Функции", "Арифметика"]
    ,C "Prelude.(*)"                                "*"                     ["Функции", "Арифметика"]
    ,C "Prelude.(-)"                                "-"                     ["Функции", "Арифметика"]
    ,C "Prelude.(/)"                                "/"                     ["Функции", "Арифметика"]
    ,C "Prelude.div"                                "div"                   ["Функции", "Арифметика"]
    ,C "Prelude.fst"                                "fst"                   ["Функции", "Кортежи"]
    ,C "Prelude.snd"                                "snd"                   ["Функции", "Кортежи"]
    ,C "Data.Tuple.Select.sel3"                     "sel3"                  ["Функции", "Кортежи"]
    ,C "Prelude.($)"                                "$"                     ["Функции", "Композиция"]
    ,C "Prelude.(.)"                                "."                     ["Функции", "Композиция"]
    ,C "Control.Exception.catch"                    "catch"                 ["Функции", "Исключения"] 
    ,C "Control.Exception.handle"                   "handle"                ["Функции", "Исключения"]
    ,C "Control.Exception.try"                      "try"                   ["Функции", "Исключения"]
    ,C "Control.Exception.throw"                    "throw"                 ["Функции", "Исключения"]
    ,C "Control.Monad.Error.runErrorT"              "runErrorT"             ["Функции", "Исключения"]
    ,C "Control.Monad.Error.throwError"             "throwError"            ["Функции", "Исключения"]
    ,C "Control.Monad.Trans.Either.left"            "left"                  ["Функции", "Исключения"]
    ,C "Control.Monad.Trans.Either.right"           "right"                 ["Функции", "Исключения"]
    ,C "Data.Maybe.fromJust"                        "fromJust"              ["Функции", "Исключения"]
    ,C "Network.FastCGI.fPutStr"                    "fPutStr"               ["Функции", "Веб"]
    ,C "Network.FastCGI.acceptLoop"                 "acceptLoop"            ["Функции", "Веб"]
    ,C "Network.FastCGI.getQueryString"             "getQueryString"        ["Функции", "Веб"]
    ,C "Network.FastCGI.setResponseStatus"          "setResponseStatus"     ["Функции", "Веб"]
    ,C "Network.FastCGI.setResponseHeader"          "setResponseHeader"     ["Функции", "Веб"]
    ,C "System.FilePath.takeExtension"              "takeExtension"         ["Функции", "Файлы"]
    ,C "Prelude.readFile"                           "readFile"              ["Функции", "Файлы","Строки"]
    ,C "System.Directory.getModificationTime"       "getModificationTime"   ["Функции", "Файлы"]
    ,C "System.Posix.Files.fileSize"                "fileSize"              ["Функции", "Файлы"]
    ,C "System.Posix.Files.getFileStatus"           "getFileStatus"         ["Функции", "Файлы"]
    ,C "System.Environment.lookupEnv"               "lookupEnv"             ["Функции", "Операционная система"]
    ,C "Prelude.return"                             "return"                ["Функции"]
    ,C "Control.Exception.evaluate"                 "evaluate"              ["Функции", "Исключения"]
    ,C "Prelude.(>>=)"                              ">>="                   ["Функции"]
    ,C "Prelude.(>>)"                               ">>"                    ["Функции"]
    ,C "Prelude.(=<<)"                              "=<<"                   ["Функции"]
    ,C "Control.Applicative.(<*>)"                  "<*>"                   ["Функции"]
    ,C "Control.Applicative.(<*)"                   "<*"                    ["Функции"]
    ,C "Control.Applicative.(*>)"                   "*>"                    ["Функции"]
    ,C "Control.Applicative.pure"                   "pure"                  ["Функции"]
    ,C "Control.Monad.Trans.Class.lift"             "lift"                  ["Функции"]
    ,C "Control.Monad.Error.liftIO"                 "liftIO"                ["Функции"]
    ,C "Control.Monad.guard"                        "guard"                 ["Функции", "Исключения"]
    ,C "Control.Monad.Trans.Maybe.runMaybeT"        "runMaybeT"             ["Функции", "Исключения"]                  
    ,C "Control.Monad.Reader.runReaderT"            "runReaderT"            ["Функции"]
    ,C "Control.Monad.Reader.runReader"             "runReader"             ["Функции"]  
    ,C "Control.Monad.Writer.Lazy.runWriterT"       "runWriterT"            ["Функции"]
    ,C "Control.Monad.Writer.Lazy.execWriterT"      "execWriterT"           ["Функции"]
    ,C "Control.Monad.State.Lazy.execStateT"        "execStateT"            ["Функции", "Состояние"]
    ,C "Control.Monad.Trans.Either.runEitherT"      "runEitherT"            ["Функции", "Исключения"]
    ,C "Control.Monad.Writer.Lazy.tell"             "tell"                  ["Функции"]
    ,C "Control.Monad.State.Lazy.modify"            "modify"                ["Функции", "Состояние"]
    ,C "Control.Monad.State.Class.get"              "get"                   ["Функции", "Состояние"]
    ,C "Control.Monad.Trans.State.Lazy.runStateT"   "runStateT"             ["Функции", "Состояние"]
    ,C "Control.Monad.Trans.State.Lazy.evalStateT"  "evalStateT"            ["Функции", "Состояние"]
    ,C "Control.Monad.State.Lazy.put"               "put"                   ["Функции", "Состояние"]
    ,C "foreign"                                    "foreign"               ["Инструкции", "Интеграция с другими языками"]
    ,C "ccall"                                      "ccall"                 ["Инструкции", "Интеграция с другими языками"]
    ]                                            

    


