----------------------------------------------------------------------------------------------------
---- Упаковка, распаковка и вычисление CRC.                                                     ----
---- Типы данных CompressionMethod, Compressor, UserCompressor - описание метода сжатия.        ----
---- Интерфейс с написанными на Си процедурами, выполняющими всю реальную работу.               ----
----------------------------------------------------------------------------------------------------
module Compression where

import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Pool
import Foreign.Ptr
import System.IO.Unsafe

import CompressionLib hiding (b,kb,mb,gb)
import Errors
import Utils
import Files
import qualified ByteStream


-- |Метод сжатия или препроцессор и его параметры
type CompressionMethod  =  CompressionLib.Method

-- Методы "сжатия", поддерживаемые напрямую, а не через CompressionLib
aSTORING              = "storing"
aFAKE_COMPRESSION     = "fake"
aCRC_ONLY_COMPRESSION = "crc"

-- |Фейковые (нераспаковываемые) методы сжатия
isFakeMethod     method  =  method==aFAKE_COMPRESSION  ||  method==aCRC_ONLY_COMPRESSION
-- |LZP метод сжатия
isLZP_Method     method  =  method_name method == "lzp"
-- |Tornado метод сжатия
isTornado_Method method  =  method_name method == "tor"
-- |DICT метод сжатия
isDICT_Method    method  =  method_name method == "dict"
-- |TTA метод сжатия
isTTA_Method     method  =  method_name method == "tta"
-- |MM метод сжатия
isMM_Method      method  =  method_name method == "mm"
-- |JPG метод сжатия
isJPG_Method     method  =  method_name method == "jpg"
-- |GRZip метод сжатия
isGRZIP_Method   method  =  method_name method == "grzip"
-- |Очень быстрый метод упаковки (>10 mb/s на 1ГГц процессоре)
isVeryFastMethod         =  compressionIs "VeryFast?"
-- |Быстрый метод распаковки
isFastDecMethod          =  not . any_function [=="ppmd", =="ppmm", =="pmm", compressionIs "external?"] . method_name


-- |Последовательность алгоритмов сжатия, используемых для обработки данных
type Compressor = [CompressionMethod]

-- |Метод "storing" (-m0)
aNO_COMPRESSION = [aSTORING] :: Compressor

-- |Это - фейковый компрессор, если в нём ровно один метод сжатия и он - фейковый
isFakeCompressor (method:xs)  =  isFakeMethod method  &&  null xs

-- |Это - LZP компрессор, если в нём ровно один метод сжатия и он - LZP
isLZP_Compressor (method:xs)  =  isLZP_Method method  &&  null xs

-- |Это - очень быстрый упаковщик, если в нём ровно один, очень быстрый метод сжатия
isVeryFastCompressor (method:xs)  =  isVeryFastMethod method  &&  null xs

-- |Это - быстрый распаковщик, если он включает только быстрые методы распаковки
isFastDecompressor = all isFastDecMethod


-- |Выбор компрессора в зависимости от типа обрабатываемых данных.
-- Первый элемент списка безымянен и описывает компрессор, используемый
-- по умолчанию (для файлов всех прочих типов, не описанных в списке явно)
type UserCompressor = [(String,Compressor)]  -- список ассоциаций типа "$text->m3t, $exe->m3x, $compressed->m0"

getCompressors :: UserCompressor -> [Compressor]
getCompressors = map snd

getMainCompressor :: UserCompressor -> Compressor
getMainCompressor = snd.head

-- |Это - метод Storing, если в нём только один компрессор aNO_COMPRESSION для файлов всех типов
isStoring ((_,compressor):xs)  =  compressor==aNO_COMPRESSION  &&  null xs

-- |Это - fake compression, если в нём только один фейковый компрессор для файлов всех типов
isFakeCompression ((_,compressor):xs)  =  isFakeCompressor compressor  &&  null xs

-- |Это - LZP compression, если в нём только один LZP компрессор для файлов всех типов
isLZP_Compression ((_,compressor):xs)  =  isLZP_Compressor compressor  &&  null xs

-- |Это очень быстрая упаковка, если в ней используются только очень быстрые упаковщики для файлов всех типов
isVeryFastCompression = all (isVeryFastCompressor.snd)

-- |Это быстрая распаковка, если в ней используются только быстрые распаковщики для файлов всех типов
isFastDecompression = all (isFastDecompressor.snd)

-- |Найти компрессор, наиболее подходящий для данных типа `ftype`.
-- Если компрессор для файлов этого типа не описан в списке - возвратить компрессор
-- по умолчанию, записанный в первый элемент списка
findCompressor ftype list  =  lookup ftype list  `defaultVal`  snd (head list)

-- |Для записи в оглавление архива информации об использованных алгоритмах сжатия
instance ByteStream.BufferData Compressor where
  write buf x  =  ByteStream.write buf (join_compressor x)
  read  buf    =  ByteStream.read  buf  >>==  split_compressor


-------------------------------------------------------------------------------------------------------------
-- Encode/decode compression method for parsing options/printing info about selected compression method -----
-------------------------------------------------------------------------------------------------------------

-- |Parse command-line option that represents compression method.
-- Декодировать запись метода сжатия в виде текстовой строки, превратив его в список ассоциаций
-- "тип файла -> метод сжатия". Первый элемент этого списка описывает метод сжатия по умолчанию
decode_method configuredMethodSubsts str =
    str                       -- "3/$obj=2b/$iso=ecm+3b"
    .$ subst list             -- "3b/3t/$obj=2b/$iso=ecm+3b"
    .$ split_to_methods       -- [("","exe+3b"), ("$obj","3b"), ("$text","3t"), ("$obj","2b"), ("$iso","ecm+3b")]
    .$ keepOnlyLastOn fst     -- [("","exe+3b"), ("$text","3t"), ("$obj","2b"), ("$iso","ecm+3b")]
    .$ filter (not.null.snd)  -- "-m$bmp=" означает запретить использование спцеиального алгоритма для группы $bmp
    .$ mapSnds (subst2 list)  -- [("",["exe","lzma"]), ("$text",["ppmd"]), ("$obj",["lzma"]), ("$iso",["ecm","lzma"])]

    where list = prepareSubsts (concatMap reorder [configuredMethodSubsts, builtinMethodSubsts])   -- сначала пользовательские замены, затем встроенные, чтобы дать первым приоритет
          reorder list = a++b  where (a,b) = partition (notElem '#') list                          -- внутри этих групп: сначала строчки, не содержащие #, затем с # (сначала конкретные, затем общие замены)

-- Замена по списку для метода сжатия (обобщённого обозначения для файлов всех типов)
subst list method  =  joinWith "/" (main_methods:group_methods++user_methods)
  where -- Из записи типа -m3/$obj=2b выделяем для расшифровки только первую часть, до слеша
        main:user_methods = split '/' method
        -- Расшифровка основных методов сжатия, типа 3x = 3xb/3xt
        main_methods = case (lookup main list) of
            Just x  -> subst list x   -- При успехе повторяем рекурсивно
            Nothing -> main           -- Больше подстановок нет
        -- Найдём в списке подстановок дополнительные методы сжатия для отдельных групп, типа 3x$iso = ecm+exe+3xb
        group_methods = list .$ keepOnlyFirstOn fst                      -- удалим повторные определения (не очень эффективно делать это именно здесь, зато по месту использования)
                             .$ mapMaybe (startFrom main . join2 "=")    -- оставим только определения, начинающиеся с 3x, удалив это 3x
                             .$ filter (("$"==).take 1)                  -- а из них - только начинающиеся с $

-- Замена по списку для алгоритма сжатия (посл-ти компрессоров для конкретного типа файлов)
subst2 list  =  concatMap f . split_compressor
    where f method = let (head,params)  =  break (==':') method
                     in case (lookup head list) of
                          Just new_head -> subst2 list (new_head++params)
                          Nothing       -> [decode_one_method method]

-- |Декодировать явно описанный метод сжатия
decode_one_method method | isFakeMethod method = method
                         | otherwise           = canonizeCompressionMethod method

-- Превращает длинную строку, описывающую методы сжатия для разных типов файлов,
-- в массив ассоциаций (тип файла, метод сжатия)
split_to_methods method = case (split '/' method) of
    [_]                 ->  [("",method)]   -- один метод для файлов всех типов
    x : xs@ (('$':_):_) ->  ("",x) : map (split2 '=') xs   -- m1/$type=m2...
    b : t : xs          ->  [("","exe+"++b), ("$obj",b), ("$text",t)] ++ map (split2 '=') xs   -- m1/m2/$type=m3...

-- Подготовить список замен к использованию в lookup
prepareSubsts x = x
    -- Удалить пустые строки, пробелы и комментарии
    .$ map (filter (not.isSpace) . fst . split2 ';') .$ filter (not.null)
    -- Заменить каждую строку с символом # на 9 строк, где # пробегает значения от 1 до 9
    .$ concatMap (\s -> map (\d->replace '#' d s) ['1'..'9'])
    -- Преобразовать список строк вида "a=b" в список для lookup
    .$ map (split2 '=')

-- Встроенные описания методов сжатия в формате, аналогичном используемому в arc.ini
builtinMethodSubsts = [
      ";High-level method definitions"
    , "x  = 9            ;highest compression mode using only internal algorithms"
    , "ax = 9p           ;highest compression mode involving external compressors"
    , "0  = storing"
    , "1  = 1b  / $exe=exe+1b"
    , "1x = 1"
    , "2  = 2b  / $exe=exe+2b  / $text=2t"
    , "2x = 2xb / $exe=exe+2xb / $text=2xt"
    , "#  = #b/#t        ;equivalent to the following:  exe+#b / $obj=#b / $text=#t"
    , "#r = #br/#tr      ; exe+#b / $obj=#b / $text=#tr"
    , "#x = #bx/#tx      ; exe+#xb/ $obj=#xb/ $text=#xt"
    , ""
    , ";Text files compression with slow decompression"
    , "1t  = 1b"
    , "2t  = grzip:m4:8m:32:h15"
    , "3tr = grzip:m1:8m:32:h15"
    , "3t  = dict:p: 32m:80% + lzp: 32m:055:d1m:s16:h18:87% + ppmd:7:48m"    -- вариант -m3t с использованием ppmd вместе grzip(bwt)
    , "4t  = dict:p: 64m:80% + lzp: 64m:065:d1m:s16:h20:90% + ppmd:8:96m"
    , "5t  = dict:p: 64m:80% + lzp: 64m:105:d1m:s32:h22:92% + ppmd:12:192m"
    , "6t  = dict:p:128m:80% + lzp:128m:145:d1m:s32:h23:92% + ppmd:16:384m"
    , "7t  = dict:p:128m:80% + lzp:128m:185:d1m:s32:h24:92% + ppmd:20:768m"
    , "8t  = dict:p:128m:80% + lzp:128m:225:d1m:s32:h24:92% + ppmd:24:1536m"
    , "9t  = 8t"
    , "4tr = 4t:12:r"     -- вариант m4t с флагом "-r1" для ppmd
    , "5tr = 5t:16:r"     -- вариант m5t с флагом "-r1" для ppmd
    , "6tr = 6t:20:r"     -- вариант m6t с флагом "-r1" для ppmd
    , "#tr = #t"
    , ""
    , ";Binary files compression with slow and/or memory-expensive decompression"
    , "1b  = 1xb"
    , "2b  = 2xb"
    , "2br = rep:  16m + lzma:2m:fastest"
    , "3b  = rep:  32m + 3xb"
    , "4b  = rep:  64m + 4xb"
    , "5b  = rep: 128m + 5xb"
    , "6b  = rep: 256m + 6xb"
    , "7b  = rep: 512m + 7xb"
    , "8b  = rep:1024m + 8xb"
    , "9b  = 8b"
    , "#br = #b"
    , ""
    , ";Text files compression with fast decompression"
    , "1xt = 1xb"
    , "2xt = 2xb"
    , "3xt = dict:  32m:80% + 3xb"
    , "4xt = dict:  64m:75% + 4xb"
    , "5xt = dict: 128m:75% + 5xb"
    , "6xt = dict: 128m:75% + 6xb"
    , "7xt = dict: 128m:75% + 7xb"
    , "8xt = dict: 128m:75% + 8xb"
    , "9xt = dict: 128m:75% + 9xb"
    , ""
    , ";Binary files compression with fast decompression"
    , "1xb = tor:3"
    , "2xb = tor"
    , "3xb = delta + lzma:  4m:fast:mc8"
    , "4xb = delta + lzma:  8m:normal:mc16"
    , "5xb = delta + lzma: 16m:max"
    , "6xb = delta + lzma: 32m:max"
    , "7xb = delta + lzma: 64m:max"
    , "8xb = delta + lzma:128m:max"
    , "9xb = 8xb"
    , ""
    , ";Synonyms"
    , "bcj = exe"
    , "#rt = #tr"
    , "#rb = #br"
    , "#bx = #xb"
    , "#tx = #xt"
    , "x#  = #x"    -- принимаем опции типа "-mx7" для мимикрии под 7-zip
    , ""
    , ";Compression modes involving external PPMONSTR.EXE"
    , "#p  = #pb/#pt"
    , "5pt = dict:p: 64m:80% + lzp: 64m:32:h22:85% + pmm: 8:160m:r0"
    , "6pt = dict:p: 64m:80% + lzp: 64m:64:h22:85% + pmm:16:384m:r1"
    , "7pt = dict:p:128m:80% + lzp:128m:64:h23:85% + pmm:20:768m:r1"
    , "8pt = dict:p:128m:80% + lzp:128m:64:h23:85% + pmm:24:1536m:r1"
    , "9pt = 8pt"
    , "#pt = #t"
    , "#pb = #b"
    , ""
    , "#q  = #qb/#qt"
    , "5qt = dict:p:64m:80% + lzp:64m:64:d1m:24:h22:85% + pmm:10:160m:r1"
    , "5qb = rep: 128m                                  + pmm:16:160m:r1"
    , "6qb = rep: 256m                                  + pmm:20:384m:r1"
    , "7qb = rep: 512m                                  + pmm:22:768m:r1"
    , "8qb = rep:1024m                                  + pmm:24:1536m:r1"
    , "9qb = 8qb"
    , "#qt = #pt"
    , "#qb = #pb"
    , ""
    , ";Sound wave files are best compressed with TTA algorithm"
    , "wav     = tta      ;best compression"
    , "wavfast = tta:m1   ;faster compression and decompression"
    , "1$wav  = wavfast"
    , "2$wav  = wavfast"
    , "#$wav  = wav"
    , "#x$wav = wavfast"
    , "#p$wav = wav"
    , ""
    , ";Bitmap graphics files are best compressed with GRZip algorithm"
    , "bmp        = mm    + grzip:m1:l:a  ;best compression"
    , "bmpfast    = mm    + grzip:m4:l:a  ;faster compression"
    , "bmpfastest = mm:d1 + tor:2         ;fastest one"
    , "1$bmp  = bmpfastest"
    , "2$bmp  = bmpfastest"
    , "3$bmp  = bmpfast"
    , "#$bmp  = bmp"
    , "1x$bmp = bmpfastest"
    , "2x$bmp = bmpfastest"
    , "#x$bmp = mm+#xb"
    , "#p$bmp = bmp"
    ]


-- |Human-readable description of compression method
encode_method uc  =  joinWith ", " (map encode_one_method uc)
encode_one_method (group,compressor)  =  between group " => " (join_compressor compressor)
join_compressor  =  joinWith "+"

-- |Opposite to join_compressor (used to read compression method from archive file)
split_compressor  =  split '+'

-- |Обработать алгоритмы в компрессоре императивной операцией process
process_algorithms process compressor = do
    return (split_compressor compressor)
       >>=  mapM process
       >>== join_compressor

-- |Разбить метод сжатия на заголовок и отдельные параметры
split_method = split ':'

-- |Имя метода сжатия
method_name = head . split_method

-- |Строка, информирующая пользователя об используемом объёме памяти
showMem 0      = "0b"
showMem mem    = showM [(gb,"gb"),(mb,"mb"),(kb,"kb"),(b,"b"),error"showMem"] mem

showMemory 0   = "0 bytes"
showMemory mem = showM [(gb," gbytes"),(mb," mbytes"),(kb," kbytes"),(b," bytes"),error"showMemory"] mem

showM xs@( (val,str) : ~(nextval,_) : _) mem =
  if mem `mod` val==0 || mem `div` nextval>=1000
    then show((mem+val`div` 2) `div` val)++str
    else showM (tail xs) mem

-- |Округлить объём памяти вверх так, чтобы он приобрёл читабельность
roundMem mem | mem>=3999*kb = mem `roundUp` mb
             | otherwise    = mem `roundUp` kb

{-# NOINLINE builtinMethodSubsts #-}
{-# NOINLINE decode_method #-}
{-# NOINLINE showMem #-}


----------------------------------------------------------------------------------------------------
----- Операции над алгоритмами сжатия                                                          -----
----------------------------------------------------------------------------------------------------

-- |Установить словарь / Ограничить используемую при сжатии/распаковке память
-- сразу для всех методов, входящих в UserCompressor
compressorSetDictionary               = map . mapSnd . mapLast . freearcSetDictionary
compressorLimitCompressionMem         = map . mapSnd . map . freearcLimitCompressionMem
compressorLimitDecompressionMem       = map . mapSnd . map . freearcLimitDecompressionMem
compressorLimitDictionary             = map . mapSnd . compressionLimitDictionary
compressorLimitCompressionMemoryUsage = map . mapSnd . compressionLimitMemoryUsage freearcGetCompressionMem

-- |Определить максимальное потребление памяти / размер блока в заданном compressor'е
compressorGetCompressionMem      = maximum . map (calcMem freearcGetCompressionMem . snd)
compressorGetDecompressionMem    = maximum . map (calcMem freearcGetDecompressionMem . snd)
compressorGetBlockSize           = maximum . map (maximum . map freearcGetBlockSize . snd)


-- |Ограничить словари для цепочки алгоритмов, прекратив это делать после первого алгоритма,
-- который может существенно раздуть данные (типа precomp). Среди внутренних алгоритмов
-- таких нет, но мы держим под подозрением все внешние :)
compressionLimitDictionary mem (x:xs) =  new_x : (not(compressionIs "external?" new_x)  &&&  compressionLimitDictionary mem) xs
                                             where new_x = freearcLimitDictionary mem x
compressionLimitDictionary mem []     =  []

-- |Вставляет вызовы tempfile между алгоритмами сжатия, разбивая их на "кластера",
-- умещающиеся в memory_limit+5% (при этом "маленькие" алгоритмы не должны начинать новых кластеров).
-- При этом для dict/dict+lzp используется особый учёт памяти (blocksize*2 на оба, blocksize/2 на выходе),
-- а external compressors обнуляют потребление памяти
compressionLimitMemoryUsage getMem memory_limit = go (0::Double) ""
  where go _   _    []      =  []
        go mem prev (x:xs) | compressionIs "external?" x  =  x: go 0            x xs
                           | mem+newMem < memlimit*1.05   =  x: go (mem+newMem) x xs
                           | otherwise                    =  "tempfile":x: go newMem x xs

           where newMem | mem==0 && isDICT_Method x             =  realToFrac (freearcGetBlockSize x) / 2
                        | isDICT_Method prev && isLZP_Method x  =  0
                        | otherwise                             =  realToFrac$ getMem x
                 memlimit = realToFrac memory_limit

-- |Посчитать потребности в памяти цепочки алгоритмов сжатия с учётом их разбиения
-- на кластеры по compressionIs "external?" и особым учётом dict/dict+lzp
calcMem getMem  = maximum . map getMemSum . splitOn (compressionIs "external?")
  where getMemSum (x:y:xs) | isDICT_Method x && isLZP_Method y  =  max (getMem x) ((freearcGetBlockSize x `div` 2) + getMemSum xs)
        getMemSum (x:xs)   | isDICT_Method x                    =  max (getMem x) ((freearcGetBlockSize x `div` 2) + getMemSum xs)
        getMemSum (x:xs)                                        =  getMem x + getMemSum xs
        getMemSum []                                            =  0

-- |Удаляет все упоминания о "tempfile" из записи алгоритма сжатия
compressionDeleteTempCompressors = filter (/="tempfile")


-- |Установить/ограничить/узнать размер словаря/блока или объём памяти используемой при упаковке/распаковке
freearcSetDictionary          =  liftSetter . CompressionLib.setDictionary
freearcLimitDictionary        =  liftSetter . CompressionLib.limitDictionary
freearcLimitCompressionMem    =  liftSetter . CompressionLib.limitCompressionMem
freearcLimitDecompressionMem  =  liftSetter . CompressionLib.limitDecompressionMem
freearcGetCompressionMem      =  liftGetter   CompressionLib.getCompressionMem
freearcGetDecompressionMem    =  liftGetter   CompressionLib.getDecompressionMem
freearcGetBlockSize           =  liftGetter   CompressionLib.getBlockSize

-- |Превратить функцию из CompressionLib, изменяющую Method, в функцию, изменяющую CompressionMethod
liftSetter action  method | aSTORING ==  method   =  method
liftSetter action  method | isFakeMethod method   =  method
liftSetter action  method                         =  action method

-- |Превратить функцию из CompressionLib, опрашивающую Method, в функцию, опрашивающую CompressionMethod
liftGetter action  method | aSTORING ==  method   =  0
liftGetter action  method | isFakeMethod method   =  0
liftGetter action  method                         =  action method


----------------------------------------------------------------------------------------------------
----- (De)compression of data stream                                                           -----
----------------------------------------------------------------------------------------------------

{-
compress   comprMethod reader writer      - упаковать данные
decompress comprMethod reader writer      - распаковать данные

  comprMethod :: CompressionMethod - алгоритм упаковки
  reader buf size - прочитать входные данные в буфер `buf` длиной `size`
                    Возвращает 0   - конец данных
                               <0  - прервать (рас)паковку (ошибка или больше данных не нужно)
                               >0  - кол-во прочитанных байт
  writer buf size - записать выходные данные
                    Возвращает <0  - прервать (рас)паковку (ошибка или больше данных не нужно)
                               >=0 - всё ок
                    При возвращении из этой функции данные должны быть "использованы", потому что
                      (рас)паковщик может начать запись новых данных на то же место
Входные и выходные буфера выделяются и освобождаются (рас)паковщиком
-}

-- |Процедуры упаковки для различных алгоритмов сжатия
freearcCompress   num method | aSTORING ==  method =  copy_data
freearcCompress   num method | isFakeMethod method =  eat_data
freearcCompress   num method                       =  checkingCtrlBreak num (CompressionLib.compress method)

-- |Процедуры распаковки для различных алгоритмов сжатия
freearcDecompress num method | aSTORING ==  method =  copy_data
freearcDecompress num method | isFakeMethod method =  undecompressible   -- эти типы сжатых данных не подлежат распаковке
freearcDecompress num method                       =  checkingCtrlBreak num (CompressionLib.decompress method)

-- |Поскольку Haskell'овский код, вызываемый из Си, не может получать
-- исключений, добавим к процедурам чтения/записи явные проверки
checkingCtrlBreak num action read write callback = do
  let checked_callback what buf size auxdata = do
        aProgramTerminated <- val programTerminated
        if aProgramTerminated
          then forever doNothing0
          else case what of
                 "read"  -> read  buf size
                 "write" -> write buf size
                 _       -> callback what buf size
  --
  res <- checked_callback "read" nullPtr 0 undefined
  if res<0  then return res
            else action (checked_callback)

-- |Копирование данных без сжатия (-m0)
copy_data reader writer callback = do
  allocaBytes aHUGE_BUFFER_SIZE $ \buf -> do  -- используем `alloca`, чтобы автоматически освободить выделенный буфер при выходе
    let go ptr = do
          len <- reader ptr ((buf+:aHUGE_BUFFER_SIZE)-:ptr)
          if (len>0)
            then do let newptr = ptr+:len
                    if newptr < (buf +: aHUGE_BUFFER_SIZE)
                       then go newptr
                       else do result <- writer buf (newptr-:buf)
                               if (result>=0)
                                 then go buf
                                 else return (result)  -- Возвратим отрицательное число, если произошла ошибка/больше данных не нужно
            else do if (len==0 && ptr>buf)
                      then do result <- writer buf (ptr-:buf)
                              return (if result>0 then 0 else result)
                      else return len  -- Возвратим 0, если данные кончились, и отрицательное число, если произошла ошибка/больше данных не нужно
    go buf -- возвратить результат

-- |Читаем всё, не пишем ничего, а CRC считается в другом месте ;)
eat_data reader writer callback = do
  allocaBytes aBUFFER_SIZE $ \buf -> do  -- используем `alloca`, чтобы автоматически освободить выделенный буфер при выходе
    let go = do
          len <- reader buf aBUFFER_SIZE
          if (len>0)
            then go
            else return len   -- Возвратим 0, если данные кончились, и отрицательное число, если произошла ошибка/больше данных не нужно
    go  -- возвратить результат

undecompressible reader writer callback = do
  return aFREEARC_ERRCODE_GENERAL   -- сразу возвратить ошибку, поскольку этот алгоритм (FAKE/CRC_ONLY) не подлежит распаковке

{-# NOINLINE checkingCtrlBreak               #-}
{-# NOINLINE copy_data                       #-}
{-# NOINLINE eat_data                        #-}
{-# NOINLINE freearcCompress                 #-}
{-# NOINLINE freearcDecompress               #-}
{-# NOINLINE compressorSetDictionary         #-}
{-# NOINLINE compressorLimitCompressionMem   #-}
{-# NOINLINE compressorLimitDecompressionMem #-}
{-# NOINLINE compressorGetCompressionMem     #-}
{-# NOINLINE compressorGetDecompressionMem   #-}
{-# NOINLINE compressorGetBlockSize          #-}


----------------------------------------------------------------------------------------------------
----- CRC calculation ------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |CRC файла
type CRC  = CUInt
aINIT_CRC = 0xffffffff  :: CRC
updateCRC addr len  =  c_UpdateCRC addr (i len)
finishCRC = xor aINIT_CRC

-- |Посчитать CRC данных в буфере
calcCRC addr len  =  updateCRC addr len aINIT_CRC  >>==  finishCRC

-- |Fast C routine for CRC-32 calculation
foreign import ccall unsafe "Environment.h UpdateCRC"
   c_UpdateCRC :: Ptr CChar -> CUInt -> CRC -> IO CRC

