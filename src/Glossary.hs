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
module Glossary {
    instructions
    ,functions
    } where 
    
    
import System.FilePath
import System.Directory

type FullName = String
type NameInBook = String
type Tag = String

data Function       = F FullName NameInBook [Tag]
data Instruction    = I NameInBook [Tag]

class Construction c where
    inBook :: c -> NameInBook
    inHackage :: c -> FullName
    tags :: c -> [Tag]
    
instance Construction Function where
    inBook (F _ a _) =      a
    inHackage (F a _ _) =   a
    tags (F _ _ a) =        a

instance Construction Instruction where
    inBook (I a _) =      a
    inHackage (I a _) =   a
    tags (I _ a) =        a    

pathToChapters = "../chapters/ru"
    

instructions =  
    [I "type"                            ["Типы"]
    ,I "data"                            ["Типы"]
    ,I "newtype"                         ["Типы"]
    ,I "class"                           ["Типы"]
    ,I "instance"                        ["Типы"]
    ,I "deriving"                        ["Типы"]
                                       
    ,I "case"                            ["Логика"]
    ,I "if"                              ["Логика"]
    ,I "then"                            ["Логика"]
    ,I "else"                            ["Логика"]
    ,I "|"                               ["Логика"]
    ,I "\\"                              ["Подвыражения"]--это лямбда(?)
    ,I "let"                             ["Подвыражения"]
    ,I "in"                              ["Подвыражения"]
    ,I "where"                           ["Подвыражения"]    
    ,I "module"                          ["Модули"]
    ,I "import"                          ["Модули"]
    ,I "hiding"                          ["Модули"]
    ,I "as"                              ["Модули"]
    ,I "qualified"                       ["Модули"]
    ,I "->"                              ["Всякое нужное"]
    ,I "<-"                              ["Всякое нужное"]
    ,I "::"                              ["Всякое нужное"]
    
    ,I "do"                              ["Всякое нужное"] 
    ,I "="                               ["Всякое нужное"] 
    ]


functions = 
    [F "Prelude.maxBound"                   "maxBound"          ["Типы"]
    ,F "Prelude.minBound"                   "minBound"          ["Типы"]
    ,F "Prelude.elem"                       "elem"              ["Списки", "Строки"]
    ,F "Prelude.filter"                     "filter"            ["Списки", "Строки"]
    ,F "Prelude.length"                     "length"            ["Списки", "Строки"]
    ,F "Prelude.takeWhile"                  "takeWhile"         ["Списки", "Строки"]
    ,F "Prelude.foldl"                      "foldl"             ["Списки"]
    ,F "Prelude.foldr"                      "foldr"             ["Списки"]
    ,F "Prelude.foldl1"                     "foldl1"            ["Списки"]
    ,F "Prelude.scanl"                      "scanl"             ["Списки"]
    ,F "Prelude.scanr"                      "scanr"             ["Списки"]
    ,F "Prelude.take"                       "take"              ["Списки", "Строки"]
    ,F "Prelude.replicate"                  "replicate"         ["Списки", "Строки"]
    ,F "Prelude.(++)"                       "++"                ["Списки", "Строки"]
    ,F "Prelude.map"                        "map"               ["Списки"]
    ,F "Prelude.fmap"                       "fmap"              ["Списки"]
    ,F "Data.Functor.(<$>)"                 "<$>"               ["Списки"]
    ,F "Prelude.(:)"                        ":"                 ["Списки"]
    ,F "Prelude.repeat"                     "repeat"            ["Списки", "Строки"]
    ,F "Prelude.!!"                         "!!"                ["Списки", "Строки"]
    ,F "Data.List.Split.splitOn"            "splitOn"           ["Списки"]
        
    ,F "Data.Map.Lazy.fromList"             "M.fromList"        ["Словари"]
    ,F "Data.Map.Lazy.lookup"               "M.lookup"          ["Словари"]
    ,F "Data.Map.Lazy.empty"                "M.empty"           ["Словари"]
    ,F "Data.Map.Lazy.insert"               "M.insert"          ["Словари"]
    ,F "Data.Map.Lazy.insertWith"           "M.insertWith"      ["Словари"]
    ,F "Data.Map.Lazy.adjust"               "M.adjust"          ["Словари"]
    ,F "Data.Map.Lazy.delete"               "M.delete"          ["Словари"]
    ,F "Data.Map.Lazy.elems"                "M.elems"           ["Словари"]
            
    ,F "Data.Set.size"                      "S.size"            ["Множества(Set)"]
    ,F "Data.Set.fromList"                  "S.fromList"        ["Множества(Set)"]
            
    ,F "Data.Char.toLower"                  "toLower"           ["Строки"]
    ,F "Data.Char.toUpper"                  "toUpper"           ["Строки"]
    ,F "Data.Char.digitToInt"               "digitToInt"        ["Строки"]
    ,F "Data.String.Utils.replace"          "replace"           ["Строки"]
    ,F "Prelude.read"                       "read"              ["Строки"]
    ,F "Prelude.show"                       "show"              ["Строки"]
        
    ,F "Data.String.Utils.startswith"       "startswith"        ["Строки"]
        
    ,F "Prelude.concat"                     "concat"            ["Строки"]
    
    ,F "Prelude.not"                        "not"               ["Логика"]
    ,F "Prelude.(==)"                       "=="                ["Логика"]
    ,F "Prelude.(/=)"                       "/="                ["Логика"]
    ,F "Prelude.(&&)"                       "&&"                ["Логика"]
        
    ,F "Prelude.null"                       "null"              ["Логика","Строки"]
    ,F "Data.Char.isNumber"                 "isNumber"          ["Логика","Строки"]
            
    ,F "Data.Maybe.isJust"                  "isJust"            ["Исключения"]
    ,F "Data.Maybe.isNothing"               "isNothing"         ["Исключения"]
    ,F "Data.List.isPrefixOf"               "isPrefixOf"        ["Логика","Строки"]
    ,F "Data.List.isSuffixOf"               "isSuffixOf"        ["Логика","Строки"]
        
    ,F "Prelude.print"                      "print"             ["Ввод-вывод"]
    ,F "Prelude.putStrLn"                   "putStrLn"          ["Ввод-вывод", "Строки"]
    ,F "Prelude.getLine"                    "getLine"           ["Ввод-вывод", "Строки"]
            
    ,F "Prelude.(+)"                        "+"                 ["Арифметика"]
    ,F "Prelude.(*)"                        "*"                 ["Арифметика"]
    ,F "Prelude.(-)"                        "-"                 ["Арифметика"]
    ,F "Prelude.(/)"                        "/"                 ["Арифметика"]
    ,F "Prelude.div"                        "div"               ["Арифметика"]
            
    ,F "Prelude.fst"                        "fst"               ["Кортежи"]
    ,F "Prelude.snd"                        "snd"               ["Кортежи"]
    ,F "Data.Tuple.Select.sel3"             "sel3"              ["Кортежи"]
                    
    ,F "Prelude.($)"                        "$"                 ["Композиция"]
    ,F "Prelude.(.)"                        "."                 ["Композиция"]
        
    ,F "Control.Exception.catch"            "catch"             ["Исключения"] 
    ,F "Control.Exception.handle"           "handle"            ["Исключения"]
    ,F "Control.Exception.try"              "try"               ["Исключения"]
    ,F "Control.Exception.throw"            "throw"             ["Исключения"]
    ,F "Control.Monad.Error.runErrorT"      "runErrorT"         ["Исключения"]
    ,F "Control.Monad.Error.throwError"     "throwError"        ["Исключения"]
    ,F "Control.Monad.Trans.Either.left"    "left"              ["Исключения"]
    ,F "Control.Monad.Trans.Either.right"   "right"             ["Исключения"]
    ,F "Data.Maybe.fromJust"                "fromJust"          ["Исключения"]]
    
    

 
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








 





 
 


