----------------------------------------------------------------------------------------------------
---- Сортировка, группировка, слияние и удаление дубликатов в списках архивируемых файлов.    ------
---- Этот модуль содержит процедуры для:                                                      ------
----   * сортировки списка файлов, найденных на диске                                         ------
----   * разбиения списка файлов на группы, образующие солид-блоки                            ------
----   * слияния списков файлов с одновременным удалением в них дубликатов                    ------
----------------------------------------------------------------------------------------------------
module ArhiveFileList where

import Data.HashTable as Hash
import Data.List
import Data.Maybe
import System.IO.Unsafe

import Utils
import Files
import FileInfo
import Compression
import Cmdline
import ArhiveStructure
import ArhiveDirectory

----------------------------------------------------------------------------------------------------
---- Сортировка списка файлов в соответствии с заданными в команде опциями -------------------------
----------------------------------------------------------------------------------------------------

-- |Sort `filelist` according to order given in `command`
sort_files Command{ opt_sort_order    = sort_order     -- порядок сортировки в виде строки, например "gen" - сортировать по группе, расширению и имени
                  , opt_find_group    = find_group     -- функция "FileInfo -> номер группы"
                  , opt_groups_count  = groups_count   -- количество групп (`find_group` возвращает результаты в диапазоне 0..groups_count-1)
                  }
           filelist = dirs ++ sortBy sort_order files  -- Отсортировать только файлы, собрав каталоги в начало списка
  where
    (dirs,files)  =  partition fiSpecialFile filelist
    sortBy sortOrder =
        case sortOrder of
          ""     -> id
          "n"    -> sortOn' (fpPackedBasename.fiStoredName)
          "s"    -> sortOn' s_key
          "es"   -> sortOn' es_key
          "en"   -> sortOn' en_key
          "ep"   -> sortOn' ep_key
          "epn"  -> sortOn' epn_key
          'e':xs -> concatMap (sortBy xs) . sort_and_groupOn' e_key
          'g':xs -> concatMap (sortBy xs) . partitionList groups_count find_group   -- разбить файлы по группам и отсортировать каждую группу по оставшимся критериям
          'r':xs -> (unsafePerformIO.reorder) . sortBy xs
          'c':xs -> (\(small,large) -> sortBy xs small ++ sortBy "s" large)
                          . partition (\fi -> fiSize fi < i(128*kb))
          _ | sortOrder `contains` 'i'  ->  intellectual_sort sortOrder
          _      -> sortOn' (key_func sortOrder find_group)

    s_key   fi =  fiSize fi                                                   where filename = fiStoredName fi
    e_key   fi =  fpLCExtension filename                                      where filename = fiStoredName fi
    n_key   fi =  fpPackedBasename filename                                   where filename = fiStoredName fi
    ns_key  fi = (fpPackedBasename filename, fiSize fi)                       where filename = fiStoredName fi
    np_key  fi = (fpPackedBasename filename, fpPackedDirectory filename)      where filename = fiStoredName fi
    es_key  fi = (fpLCExtension filename, fiSize fi)                          where filename = fiStoredName fi
    en_key  fi = (fpLCExtension filename, fpPackedBasename  filename)         where filename = fiStoredName fi
    ep_key  fi = (fpLCExtension filename, fpPackedDirectory filename)         where filename = fiStoredName fi
    epn_key fi = (fpLCExtension filename, fpPackedDirectory filename, fpPackedBasename filename)   where filename = fiStoredName fi

    -- Интеллектуальная сортировка списка файлов (-ds=gepin/geipn)
    intellectual_sort sortOrder =
    -- 1. сортировка и группировка по "gep"
    -- 2. файлы с одинаковым расширением группируем по первым трём буквам имени
    -- 3a. те группы, в которых всего один файл - сливаем вместе и сортируем по "s" ("ps", поскольку файлы из одного каталога лучше собирать вместе?)
    -- 3b. остальные группы сортируем внутри по "ns"
    -- 4. выводим группы в порядке возрастания размеров группы? среднего размера файлов в группе?
      concatMap isort . sort_and_groupOn' (key_func o1 find_group)
      where (o1,'i':o2) =  break (=='i') sortOrder
            isort group = -- Отсортировать список файлов, имеющих одинаковое расширение
              let groups = groupOn three (sortOn (key_func o2 find_group) group)  -- сгруппировать по первым 3-м буквам имени
                             where three = take 3 . filenameLower.fpBasename.fiStoredName
                  (singles, full_groups)  =  partition (null.tail) groups  -- разбить на группы, состоящие из одного-единственного файла, и "настоящие группы" :)
                  list1  =  sortOn' s_key (concat singles)  -- список одиночных файлов в правильном порядке
              in list1 ++ concat full_groups


-- |Map `sort_order` to function returning ordering key
key_func sort_order find_group  =  map_functions (map key sort_order)
  where
        key 'p' = OrderPackedStr . fpPackedDirectory . fiStoredName
        key 'n' = OrderPackedStr . fpPackedBasename  . fiStoredName
        key 'e' = OrderFilePath  . fpLCExtension     . fiStoredName
        key 's' = OrderFileSize                      . fiSize
        key 't' = OrderFileTime                      . fiTime
        key 'g' = OrderGroup                         . find_group
        key 'c' = key 's'
        key 'i' = key 's'
        key 'r' = OrderGroup                         . const 1

-- |Структура данных для описания ключа сортировки
data SortOrder =   OrderFilePath  !FilePath
                 | OrderPackedStr !MyPackedString
                 | OrderFileSize  !FileSize
                 | OrderFileTime  !FileTime
                 | OrderGroup     Int               deriving (Eq, Ord)


----------------------------------------------------------------------------------------------------
---- Переупорядочить файлы так, чтобы рядом оказались похожие --------------------------------------
----------------------------------------------------------------------------------------------------

-- |Переупорядочить файлы так, чтобы расположить рядом одинаковые или похожие, а именно -
-- имеющие одинаковое расширение и размер (предположительно тот же самый файл под разными именами),
-- или одинаковое имя и близкие размеры (предположительно разные версии одного файла)
reorder files = do
    -- Проверить, что имена файлов одинаковы, а размеры отличаются не более, чем в два раза (или совершенно одинаковы, если файлы небольшие)
    let near_size (name1,size1) (name2,size2)  =  name1==name2 &&
               (if size1 <= i(16*kb)
                 then size1==size2
                 else size1 `inRange` (size2 `div` 2, size2*2))
    -- Первый хеш - по совпадению расширения и размера, второй - по совпадению имени и близости размера
    hash1 <- Hash.new (==)      (\(ext,size)  -> i$ filenameHash size ext)
    hash2 <- Hash.new near_size (\(name,size) ->    filenameHash 0 (myUnpackStr name))
    -- Занести каждый файл в хеш и найти для него, если возможно, похожий из уже обработанных.
    let renumber (num,file) = do
            if fiSize file <= 1024
              then return (num, file)
              else do
            -- Найти файл, похожий на нынешний, предпочтительно в первой хеш-таблице
            let key1 = (fpLCExtension$ fiStoredName file, fiSize file)
                key2 = (fpPackedBasename$ fiStoredName file, fiSize file)
            newnum <- (if fiSize file <= i(16*kb)
                         then return Nothing
                         else Hash.lookup hash1 key1) `defaultValM`
                      (Hash.lookup hash2 key2  `defaultValM` return num)
            -- Номер найденного похожего файла (или собственный номер файла, если ничего похожего
            -- найти не удалось), становится его "номером группы". Вставить файл в обе хеш-таблицы
            -- под этим номером, чтобы последующие похожие на него файлы смогли получить тот же
            -- номер группы, и возвратить этот номер - он будет использоваться при сортировке
            -- чтобы собрать вместе похожие файлы
            Hash.insert hash1 key1 newnum
            Hash.insert hash2 key2 newnum
            return (newnum, file)
    -- numbered_files - список пар (номер, файл), где похожие файлы получили одинаковые номера
    numbered_files <- mapM renumber (zip [0..] files)
    -- Отсортировать файлы по номеру группы/размеру/имени/пути
    let ordering (num,file) = (num, fiSize file, fpPackedBasename filename, fpPackedDirectory filename)   where filename = fiStoredName file
    return $ map snd $ sortOn ordering numbered_files


----------------------------------------------------------------------------------------------------
---- Слияние списков файлов с одновременным удалением в них дубликатов -----------------------------
----------------------------------------------------------------------------------------------------

-- |Сконструировать список файлов, которые должны попасть в создаваемый архив,
-- из списков файлов во входном архиве, дополнительных архивах и на диске
-- с предварительным удалением дубликатов из этих списков.
-- Способ слияния списков зависит от алгоритма обновления архива `update_type` (a/f/u/s)
--
join_lists main_archive added_archives added_diskfiles
           Command {            -- данные о выполняемой команде:
               opt_update_type = update_type    -- алгоритм обновления архива (a/f/u/s)
             , opt_append      = append         -- добавлять новые файлы только в конец архива?
             , opt_sort_order  = sort_order     -- порядок сортировки в виде строки, например "gen" - сортировать по группе, расширению и имени
             , opt_find_group  = find_group     -- функция "FileInfo -> номер группы"
             } = do

  -- Алгоритм работы:
  -- 1. Создаём два хеша, содержащие все файлы в архиве и на диске, соответственно.
  --    При создании хеш-таблиц заодно избавляемся от дубликатов (по StoredName) в списках
  -- 2. Если один из списков пуст - просто возвращаем второй
  -- 3. Проходим по списку файлов в архиве, находим соответствующий файл на диске (если есть),
  --    и выбираем из этих двух файлов тот, который должен попасть в выходной архив
  -- 4. Проходим по списку файлов с диска и добавляем в выходной архив те из них,
  --    которых не было во входном архиве

  let -- Список файлов в основном архиве, который подвергается обновлению
      main_list   =  arcDirectory main_archive
      -- Список добавляемых к архиву файлов, включающий файлы из дополнительных входных архивов и файлы с диска
      added_list  =  (concatMap arcDirectory added_archives)  ++  (map DiskFile added_diskfiles)

  -- DEBUGGING ------------------------------------------------------
  let typ (DiskFile _)             = "Disk: "
      typ (CompressedFile _ _ _ _) = "Archive: "
      name  fi = fpFullname(fiStoredName fi)
      names fi = fpFullname(fiDiskName fi)++" "++fpFullname(fiFilteredName fi)++" "++fpFullname(fiStoredName fi)
  --print$ map (names.cfFileInfo) arcdir
  --print$ map (names.cfFileInfo) diskfiles
  -- DEBUGGING ------------------------------------------------------

  -- 1. Создадим два хеша: содержащий файлы из обновляемого архива, и содержащий добавляемые файлы,
  --    и заодно исключим из исходных списков дубликаты файлов
  let keyFunc  =  fiStoredName . cfFileInfo  -- ключ, по которому индексируются эти хеш-таблицы
  (main_list,  main_hash )  <-  removeDuplicates main_list  keyFunc (==) fpHash
  (added_list, added_hash)  <-  removeDuplicates added_list keyFunc (==) fpHash

  -- 2. Если один из списков пуст - просто возвращаем второй
  case () of
   _ | null main_list   -> return added_list  -- если один из списков пуст, то просто возвращаем второй
     | null added_list  -> return (if update_type=='s' then [] else main_list)  -- почти аналогично
     | otherwise        -> do
    -- Иначе придётся сливать эти списки:

    -- 3. Пройтись по файлам в обновляемом архиве и заменить их, если нужно, добавляемыми файлами
    let newer_file arcfile diskfile =   -- Возвратить тот файл, который свежее
          if (fiTime (cfFileInfo arcfile) >= fiTime (cfFileInfo diskfile))
            then arcfile   -- файл в архиве новее или тот же, поэтому берём его
            else diskfile  -- файл на диске новее, поэтому берём его

    let sync_file arcfile diskfile =   -- Возвратить архивный файл, если он не отличается от дискового
          if (fiTime (cfFileInfo arcfile) == fiTime (cfFileInfo diskfile))
            then arcfile   -- файл в архиве тот же, поэтому берём его
            else diskfile  -- файл на диске новее или старее, поэтому берём его

    let select_file arcfile = do
          diskfile <- Hash.lookup added_hash (keyFunc arcfile)
          case (diskfile, update_type) of
            (Nothing, 's')        ->  return$ Nothing        -- Файла с таким именем на диске нет: режим "--sync" удаляет из архива файлы, которых нет на диске
            (Nothing,  _ )        ->  return$ Just arcfile   --   остальные режимы оставляют в архиве уже существующий файл
            (Just diskfile, 'a')  ->  return$ Just diskfile  -- Файл есть и в архиве, и на диске:  режим "a" всегда берёт файл с диска
            (Just diskfile, 's')  ->  return$ Just (sync_file  arcfile diskfile)  -- режим "--sync" оставляет архивный файл, если он не отличается от файла с диска
            (Just diskfile,  _ )  ->  return$ Just (newer_file arcfile diskfile)  -- остальные режимы берут более свежий из двух файлов

    list1 <- mapMaybeM select_file main_list  -- выбрать между файлом из входного архива и файлом с диска


    -- 4. Добавить те файлы из второго списка, которых в обновляемом архиве вообще не было
    let new_files_only diskfile = do
          -- Возвратить истину, если этого файла не было в исходном архиве
          isNothing ==<< Hash.lookup main_hash (keyFunc diskfile)

    list2 <- case (update_type) of
                 -- Режим "f": файлы, отсутствовавшие во входном архиве, не брать
               'f' -> return []
                 -- Взять те файлы с диска, которых не было во входном архиве
               _   -> Utils.filterM new_files_only added_list

    -- DEBUGGING ------------------------------------------------------
    --print$ map (\f -> typ f ++ (fpFullname.fiStoredName.cfFileInfo) f) list1
    --print$ map (fpFullname.fiStoredName.cfFileInfo) list2
    -- DEBUGGING ------------------------------------------------------

    -- Слить список файлов в архиве со списком файлов на диске
    let mergeFunction = case () of     -- Добавить новые файлы в конец архива в двух случаях:
          _ | append         -> (++)   --    при установленной опции "--append"
            | sort_order=="" -> (++)   --    при пустом ключе сортировки ("-ds")
            | otherwise      -> mergeFilelists sort_order find_group  -- иначе использовать полноценное слияние
    return$ mergeFunction list1 list2


-- |Соединение списков (файлы из архива + файлы с диска)
mergeFilelists sort_order find_group filelist1 filelist2  =  dirs ++ files
  where -- Слить файлы в порядке, определяемом опцией -ds, а каталоги - в порядке "путь+имя"
    (dirs1,files1)  =  partition (fiSpecialFile.cfFileInfo) filelist1
    (dirs2,files2)  =  partition (fiSpecialFile.cfFileInfo) filelist2
    dirs  = merge (map2cmp (key_func "pn"       find_group . cfFileInfo)) dirs1  dirs2
    files = merge (map2cmp (key_func sort_order find_group . cfFileInfo)) files1 files2


-- | Создать из списка `originalList` хеш-таблицу без дубликатов,
-- одновременно вычистив дубликаты в самом списке
--
removeDuplicates originalList keyFunc eqFunc hashFunc = do
  table <- Hash.new eqFunc hashFunc

  -- Вставить в хеш `table` элемент `value` только, если в нём ещё нет элемента с таким же ключом.
  -- Из дублирующих значений в списке и хеше оставляется первое, поэтому нельзя использовать `update`
  -- (to do: `reverse` позволит использовать `update`)
  let insert_no_dups value = do
        let key  =  keyFunc value
        found <- Hash.lookup table key
        case found of
          Nothing       ->  do Hash.insert table key value
                               return True
          Just oldfile  ->  return False

  list <- Utils.filterM insert_no_dups originalList
  return (list,table)

{-# NOINLINE splitBy #-}
{-# NOINLINE removeDuplicates #-}


----------------------------------------------------------------------------------------------------
---- Выбор подходящего компрессора и разбиение списка файлов по блокам каталога/солид-блокам -------
----------------------------------------------------------------------------------------------------

-- |Компрессор для служебных блоков архива (кроме HEADER_BLOCK, который мы не сжимаем)
dir_compressor command  =  opt_dir_compressor command

-- |Разбить список файлов на части, каждая из которых должна иметь свой блок каталога
splitToDirBlocks command  =  splitBy (opt_group_dir command) True


-- |Наиболее подходящий компрессор для файлов из списка (определяется по первому файлу в списке)
-- (специальные файлы, вроде каталогов, не сжимаются, а файлы, сжатые фейковыми алгоритмами,
-- остаются без изменений)
data_compressor _       []                                = aNO_COMPRESSION
data_compressor command (file:_) | fiSpecialFile fi       = aNO_COMPRESSION
                                 | isCompressedFake file  = cfCompressor file
                                 | otherwise              = snd (types_of_compressors !! find_type fi)
  where fi = cfFileInfo file
        types_of_compressors = opt_data_compressor command
        find_type = opt_find_type command

-- |Разбить список файлов на солид-блоки с учётом типов файлов (splitByType)
-- и заданной в opt_group_data группировки для солид-сжатия (splitOneType).
-- Кроме того, для блочных алгоритмов (bwt, lzp) размер солид-блоков
-- ограничивается размером блока в алгоритме сжатия.
-- Наконец, каталоги и файлы, сжимаемые фейковыми компрессорами (nodata/crconly),
-- на отдельные солид-блоки не разбиваются.
splitToSolidBlocks command filelist  =  (dirs &&& [dirs]) ++ groupOn cfArcBlock solidBlocksToKeep
                                        ++ concatMap splitOneType  (splitByType filesToSplit)
  where
    -- Каталоги - отдельным блоком
    (dirs,files)  =  partition (fiSpecialFile.cfFileInfo) filelist
    -- В командах копирования архива и при --append нельзя перепаковывать уже сжатые файлы
    (solidBlocksToKeep, filesToSplit) | opt_keep_original command = partition isCompressedFile files
                                      | otherwise                 = ([],files)

    -- Разбить список по типам файлов ($binary, $text...).
    -- При этом файлы, уже сжатые фейковыми алгоритмами, должны получить прежний тип (т.е. алгоритм сжатия)
    splitByType filelist  =  map concat groups
      where
        (fake,normal) = partition isCompressedFake filelist
        normalGroups  = partitionList (opt_types_count command) (opt_find_type command . cfFileInfo) normal
        -- Фейковые файлы разбиваются на группы по используемым алгоритмам сжатия
        fakeGroups    = sort_and_groupOn cfCompressor fake
        -- ... и эти группы объединяются с группами нормальных файлов, которые предстоит сжать теми же алгоритмами
        groups        = sort_and_groupOn (data_compressor command) (fakeGroups++filter (not.null) normalGroups)

    -- Разбить на солид-блоки список файлов одного типа (то есть сжимаемых одним алгоритмом)
    splitOneType files | isFakeCompressor compressor = [files]  -- Для фейковых компрессоров нет смысла разбивать блок на части
                       | compressor==aNO_COMPRESSION = [files]  -- Для не-сжатия в этом тоже нет смысла
                       | otherwise =
        splitBy (opt_group_data command .$ addBlockSizeCrit) (opt_recompress command) files
      where
        -- Упаковщик, используемый для файлов этого типа (т.е. бинарных или текстовых)
        compressor = data_compressor command files
        -- Для цепочек алгоритмов, начинающихся с TTA/MM/JPG, отключить солид-сжатие
        -- Для цепочек алгоритмов, начинающихся с DICT - ограничить солид-блок размером блока DICT
        -- Для остальных блочных алгоритмов (grzip, lzp) alone - размером блока в алгоритме сжатия
        addBlockSizeCrit = case compressor of
            algorithm:_ | makeNonSolid  algorithm          ->  const [GroupNone]
            algorithm:_ | isDICT_Method algorithm          ->  ([GroupByBlockSize $ freearcGetBlockSize algorithm]++)
            [algorithm] | freearcGetBlockSize algorithm>0  ->  ([GroupByBlockSize $ freearcGetBlockSize algorithm]++)
            _                                              ->  id

-- |Необходимо ли поместить файлы, сжимаемые этим мультимедийным алгоритмом,
-- в отдельные solid-блоки? Исключение может быть сделано только в случае,
-- когда алгоритм содержит строку "*8" и не содержит строки ":o", то есть
-- все данные сжимаются как непрерывная последовательность байтов, которую
-- нет нужды разбивать на отдельные блоки
makeNonSolid m = any_function [isTTA_Method, isMM_Method, isJPG_Method] m
                 && (not (m `substr` "*8")  ||  (m `substr` ":o"))


----------------------------------------------------------------------------------------------------
---- Разбиение списка файлов на группы в соответствии с заданными критериями -----------------------
----------------------------------------------------------------------------------------------------

-- |Разбить список файлов на группы в соответствии с заданными критериями (для блоков данных/каталога)
-- При этом в качестве каждой очередной группы берётся сегмент минимальной длины, отвечающий хотя бы одному из заданных критериев,
-- а при обновлении солид-архивов делается всё возможное для того, чтобы не перепаковывать те солид-блоки, которые остались целыми после вставки новых файлов
-- crits - критерии разбиения на группы, recomress=True - игнорировать границы старых солид-блоков
splitBy []    _          files = [files]  -- Если нет критериев разбиения на подсписки - пустить все файлы общим списком
splitBy crits recompress files = splitByLen (computeLen) files where
  -- Возвратить количество файлов из начала files, которые должны войти в очередной солид-блок
  computeLen files = case () of
     _ | recompress     -> newLen   -- при --recompress мы вообще не интересуемся всякими старыми солид-блоками
       | Just n<-oldLen -> n        -- скопировать существующий солид-блок из n файлов
       | Just n<-oldPos -> if n<=newLen
                             then n        -- сделать новый солид-блок ровно до начала следующего старого
                             else minLen
       | otherwise      -> newLen   -- подстроиться под старый солид-блок не удалось
    where
      newLen = minimum$ map (`splitLen`    files) crits   -- длина нового солид-блока, вычисленная соответственно критериям разбиения на солид-блоки
      minLen = minimum$ map (`splitLenMin` files) crits   -- минимально допустимая длина солид-блока (с уменьшенными в 4 раза критериями)
      maxLen = minimum$ map (`splitLenMax` files) crits   -- ...
      oldLen = solidBlockLen files                        -- длина существующего солид-блока, с которого начинается переданный список файлов, или Nothing
      oldPos = findSolidBlock minLen maxLen files         -- поискать среди файлов, которые предполагается включить в новый солид-блок, начало старого солид-блока

-- |Длина минимальной группы (начального сегмента списка файлов), отвечающей заданному критерию
splitLen  GroupNone              = const 1
splitLen  GroupByExt             = length.head.groupOn (fpLCExtension.fiFilteredName.cfFileInfo)
splitLen (GroupBySize      size) = (1+)      . groupLen (fiSize.cfFileInfo) (+) (<i size)
splitLen (GroupByBlockSize size) = atLeast 1 . groupLen (fiSize.cfFileInfo) (+) (<i size)
splitLen (GroupByNumber       n) = atLeast 1 . const n
splitLen  GroupAll               = const maxBound

-- |Длина минимальной группы файлов, позволенной при заданном критерии (вдвое меньше номинала)
splitLenMin (GroupBySize      size) = splitLen (GroupBySize      (size `div` 2))
splitLenMin (GroupByBlockSize size) = splitLen (GroupByBlockSize (size `div` 2))
splitLenMin (GroupByNumber       n) = splitLen (GroupByNumber    (n    `div` 2))
splitLenMin x                       = splitLen x

-- |Длина максимальной группы файлов, позволенной при заданном критерии (в 1.5 раза больше номинала)
splitLenMax (GroupBySize      size) = splitLen (GroupBySize      (size+(size `div` 2)))
splitLenMax (GroupByBlockSize size) = splitLen (GroupByBlockSize (size+(size `div` 2)))
splitLenMax (GroupByNumber       n) = splitLen (GroupByNumber    (n   +(n    `div` 2)))
splitLenMax x                       = splitLen x


----------------------------------------------------------------------------------------------------
---- Распознавание существующих solid-блоков в списке сжимаемых файлов -----------------------------
----------------------------------------------------------------------------------------------------

-- |Проверить, что переданный список файлов - полный список файлов в solid-блоке
isWholeSolidBlock files @ (CompressedFile {cfArcBlock=solidBlock, cfPos=pos}:_) =
  pos == 0                            &&    -- Если первый файл в списке является началом солид-блока (pos = номеру первого принадлежащего этому файлу байта в солид-блоке)
  blFiles solidBlock == length files  &&    --   список имеет ту же длину, что и солид-блок, к которому принадлежит первый файл в списке,
  all        isCompressedFile  files  &&    --   состоит только из сжатых файлов,
  isEqOn     cfArcBlock        files  &&    --   принадлежащих одному блоку
  isSortedOn cfPos             files        --   и отсортированных по позиции в солид-блоке

isWholeSolidBlock _ = False

-- |Длина начального сегмента списка файлов, содержащего файлы из одного солид-блока (точнее, целиком один солид-блок)
solidBlockLen []    = Nothing
solidBlockLen files = let n = blFiles (cfArcBlock (head files)) in   -- количество файлов в солид-блоке, которому принадлежит 'head files'
                      if isCompressedFile (head files)           -- Переданный список начинается со сжатого файла
                         && isWholeSolidBlock (take n files)     --   и является полным солид-блоком
                      then Just n
                      else Nothing

-- |Найти солид-блок, начинающийся в одном из первых min..max файлов списка,
-- и возвратить индекс его первого файла
findSolidBlock min max = fmap (+min)                       -- компенсировать 'drop min'
                       . findIndex (isJust.solidBlockLen)
                       . take (max-min)
                       . tails
                       . drop min

