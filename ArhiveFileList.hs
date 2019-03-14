----------------------------------------------------------------------------------------------------
---- ����������, �����������, ������� � �������� ���������� � ������� ������������ ������.    ------
---- ���� ������ �������� ��������� ���:                                                      ------
----   * ���������� ������ ������, ��������� �� �����                                         ------
----   * ��������� ������ ������ �� ������, ���������� �����-�����                            ------
----   * ������� ������� ������ � ������������� ��������� � ��� ����������                    ------
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
---- ���������� ������ ������ � ������������ � ��������� � ������� ������� -------------------------
----------------------------------------------------------------------------------------------------

-- |Sort `filelist` according to order given in `command`
sort_files Command{ opt_sort_order    = sort_order     -- ������� ���������� � ���� ������, �������� "gen" - ����������� �� ������, ���������� � �����
                  , opt_find_group    = find_group     -- ������� "FileInfo -> ����� ������"
                  , opt_groups_count  = groups_count   -- ���������� ����� (`find_group` ���������� ���������� � ��������� 0..groups_count-1)
                  }
           filelist = dirs ++ sortBy sort_order files  -- ������������� ������ �����, ������ �������� � ������ ������
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
          'g':xs -> concatMap (sortBy xs) . partitionList groups_count find_group   -- ������� ����� �� ������� � ������������� ������ ������ �� ���������� ���������
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

    -- ���������������� ���������� ������ ������ (-ds=gepin/geipn)
    intellectual_sort sortOrder =
    -- 1. ���������� � ����������� �� "gep"
    -- 2. ����� � ���������� ����������� ���������� �� ������ ��� ������ �����
    -- 3a. �� ������, � ������� ����� ���� ���� - ������� ������ � ��������� �� "s" ("ps", ��������� ����� �� ������ �������� ����� �������� ������?)
    -- 3b. ��������� ������ ��������� ������ �� "ns"
    -- 4. ������� ������ � ������� ����������� �������� ������? �������� ������� ������ � ������?
      concatMap isort . sort_and_groupOn' (key_func o1 find_group)
      where (o1,'i':o2) =  break (=='i') sortOrder
            isort group = -- ������������� ������ ������, ������� ���������� ����������
              let groups = groupOn three (sortOn (key_func o2 find_group) group)  -- ������������� �� ������ 3-� ������ �����
                             where three = take 3 . filenameLower.fpBasename.fiStoredName
                  (singles, full_groups)  =  partition (null.tail) groups  -- ������� �� ������, ��������� �� ������-������������� �����, � "��������� ������" :)
                  list1  =  sortOn' s_key (concat singles)  -- ������ ��������� ������ � ���������� �������
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

-- |��������� ������ ��� �������� ����� ����������
data SortOrder =   OrderFilePath  !FilePath
                 | OrderPackedStr !MyPackedString
                 | OrderFileSize  !FileSize
                 | OrderFileTime  !FileTime
                 | OrderGroup     Int               deriving (Eq, Ord)


----------------------------------------------------------------------------------------------------
---- ��������������� ����� ���, ����� ����� ��������� ������� --------------------------------------
----------------------------------------------------------------------------------------------------

-- |��������������� ����� ���, ����� ����������� ����� ���������� ��� �������, � ������ -
-- ������� ���������� ���������� � ������ (���������������� ��� �� ����� ���� ��� ������� �������),
-- ��� ���������� ��� � ������� ������� (���������������� ������ ������ ������ �����)
reorder files = do
    -- ���������, ��� ����� ������ ���������, � ������� ���������� �� �����, ��� � ��� ���� (��� ���������� ���������, ���� ����� ���������)
    let near_size (name1,size1) (name2,size2)  =  name1==name2 &&
               (if size1 <= i(16*kb)
                 then size1==size2
                 else size1 `inRange` (size2 `div` 2, size2*2))
    -- ������ ��� - �� ���������� ���������� � �������, ������ - �� ���������� ����� � �������� �������
    hash1 <- Hash.new (==)      (\(ext,size)  -> i$ filenameHash size ext)
    hash2 <- Hash.new near_size (\(name,size) ->    filenameHash 0 (myUnpackStr name))
    -- ������� ������ ���� � ��� � ����� ��� ����, ���� ��������, ������� �� ��� ������������.
    let renumber (num,file) = do
            if fiSize file <= 1024
              then return (num, file)
              else do
            -- ����� ����, ������� �� ��������, ��������������� � ������ ���-�������
            let key1 = (fpLCExtension$ fiStoredName file, fiSize file)
                key2 = (fpPackedBasename$ fiStoredName file, fiSize file)
            newnum <- (if fiSize file <= i(16*kb)
                         then return Nothing
                         else Hash.lookup hash1 key1) `defaultValM`
                      (Hash.lookup hash2 key2  `defaultValM` return num)
            -- ����� ���������� �������� ����� (��� ����������� ����� �����, ���� ������ ��������
            -- ����� �� �������), ���������� ��� "������� ������". �������� ���� � ��� ���-�������
            -- ��� ���� �������, ����� ����������� ������� �� ���� ����� ������ �������� ��� ��
            -- ����� ������, � ���������� ���� ����� - �� ����� �������������� ��� ����������
            -- ����� ������� ������ ������� �����
            Hash.insert hash1 key1 newnum
            Hash.insert hash2 key2 newnum
            return (newnum, file)
    -- numbered_files - ������ ��� (�����, ����), ��� ������� ����� �������� ���������� ������
    numbered_files <- mapM renumber (zip [0..] files)
    -- ������������� ����� �� ������ ������/�������/�����/����
    let ordering (num,file) = (num, fiSize file, fpPackedBasename filename, fpPackedDirectory filename)   where filename = fiStoredName file
    return $ map snd $ sortOn ordering numbered_files


----------------------------------------------------------------------------------------------------
---- ������� ������� ������ � ������������� ��������� � ��� ���������� -----------------------------
----------------------------------------------------------------------------------------------------

-- |��������������� ������ ������, ������� ������ ������� � ����������� �����,
-- �� ������� ������ �� ������� ������, �������������� ������� � �� �����
-- � ��������������� ��������� ���������� �� ���� �������.
-- ������ ������� ������� ������� �� ��������� ���������� ������ `update_type` (a/f/u/s)
--
join_lists main_archive added_archives added_diskfiles
           Command {            -- ������ � ����������� �������:
               opt_update_type = update_type    -- �������� ���������� ������ (a/f/u/s)
             , opt_append      = append         -- ��������� ����� ����� ������ � ����� ������?
             , opt_sort_order  = sort_order     -- ������� ���������� � ���� ������, �������� "gen" - ����������� �� ������, ���������� � �����
             , opt_find_group  = find_group     -- ������� "FileInfo -> ����� ������"
             } = do

  -- �������� ������:
  -- 1. ������ ��� ����, ���������� ��� ����� � ������ � �� �����, ��������������.
  --    ��� �������� ���-������ ������ ����������� �� ���������� (�� StoredName) � �������
  -- 2. ���� ���� �� ������� ���� - ������ ���������� ������
  -- 3. �������� �� ������ ������ � ������, ������� ��������������� ���� �� ����� (���� ����),
  --    � �������� �� ���� ���� ������ ���, ������� ������ ������� � �������� �����
  -- 4. �������� �� ������ ������ � ����� � ��������� � �������� ����� �� �� ���,
  --    ������� �� ���� �� ������� ������

  let -- ������ ������ � �������� ������, ������� ������������ ����������
      main_list   =  arcDirectory main_archive
      -- ������ ����������� � ������ ������, ���������� ����� �� �������������� ������� ������� � ����� � �����
      added_list  =  (concatMap arcDirectory added_archives)  ++  (map DiskFile added_diskfiles)

  -- DEBUGGING ------------------------------------------------------
  let typ (DiskFile _)             = "Disk: "
      typ (CompressedFile _ _ _ _) = "Archive: "
      name  fi = fpFullname(fiStoredName fi)
      names fi = fpFullname(fiDiskName fi)++" "++fpFullname(fiFilteredName fi)++" "++fpFullname(fiStoredName fi)
  --print$ map (names.cfFileInfo) arcdir
  --print$ map (names.cfFileInfo) diskfiles
  -- DEBUGGING ------------------------------------------------------

  -- 1. �������� ��� ����: ���������� ����� �� ������������ ������, � ���������� ����������� �����,
  --    � ������ �������� �� �������� ������� ��������� ������
  let keyFunc  =  fiStoredName . cfFileInfo  -- ����, �� �������� ������������� ��� ���-�������
  (main_list,  main_hash )  <-  removeDuplicates main_list  keyFunc (==) fpHash
  (added_list, added_hash)  <-  removeDuplicates added_list keyFunc (==) fpHash

  -- 2. ���� ���� �� ������� ���� - ������ ���������� ������
  case () of
   _ | null main_list   -> return added_list  -- ���� ���� �� ������� ����, �� ������ ���������� ������
     | null added_list  -> return (if update_type=='s' then [] else main_list)  -- ����� ����������
     | otherwise        -> do
    -- ����� ������� ������� ��� ������:

    -- 3. �������� �� ������ � ����������� ������ � �������� ��, ���� �����, ������������ �������
    let newer_file arcfile diskfile =   -- ���������� ��� ����, ������� ������
          if (fiTime (cfFileInfo arcfile) >= fiTime (cfFileInfo diskfile))
            then arcfile   -- ���� � ������ ����� ��� ��� ��, ������� ���� ���
            else diskfile  -- ���� �� ����� �����, ������� ���� ���

    let sync_file arcfile diskfile =   -- ���������� �������� ����, ���� �� �� ���������� �� ���������
          if (fiTime (cfFileInfo arcfile) == fiTime (cfFileInfo diskfile))
            then arcfile   -- ���� � ������ ��� ��, ������� ���� ���
            else diskfile  -- ���� �� ����� ����� ��� ������, ������� ���� ���

    let select_file arcfile = do
          diskfile <- Hash.lookup added_hash (keyFunc arcfile)
          case (diskfile, update_type) of
            (Nothing, 's')        ->  return$ Nothing        -- ����� � ����� ������ �� ����� ���: ����� "--sync" ������� �� ������ �����, ������� ��� �� �����
            (Nothing,  _ )        ->  return$ Just arcfile   --   ��������� ������ ��������� � ������ ��� ������������ ����
            (Just diskfile, 'a')  ->  return$ Just diskfile  -- ���� ���� � � ������, � �� �����:  ����� "a" ������ ���� ���� � �����
            (Just diskfile, 's')  ->  return$ Just (sync_file  arcfile diskfile)  -- ����� "--sync" ��������� �������� ����, ���� �� �� ���������� �� ����� � �����
            (Just diskfile,  _ )  ->  return$ Just (newer_file arcfile diskfile)  -- ��������� ������ ����� ����� ������ �� ���� ������

    list1 <- mapMaybeM select_file main_list  -- ������� ����� ������ �� �������� ������ � ������ � �����


    -- 4. �������� �� ����� �� ������� ������, ������� � ����������� ������ ������ �� ����
    let new_files_only diskfile = do
          -- ���������� ������, ���� ����� ����� �� ���� � �������� ������
          isNothing ==<< Hash.lookup main_hash (keyFunc diskfile)

    list2 <- case (update_type) of
                 -- ����� "f": �����, ��������������� �� ������� ������, �� �����
               'f' -> return []
                 -- ����� �� ����� � �����, ������� �� ���� �� ������� ������
               _   -> Utils.filterM new_files_only added_list

    -- DEBUGGING ------------------------------------------------------
    --print$ map (\f -> typ f ++ (fpFullname.fiStoredName.cfFileInfo) f) list1
    --print$ map (fpFullname.fiStoredName.cfFileInfo) list2
    -- DEBUGGING ------------------------------------------------------

    -- ����� ������ ������ � ������ �� ������� ������ �� �����
    let mergeFunction = case () of     -- �������� ����� ����� � ����� ������ � ���� �������:
          _ | append         -> (++)   --    ��� ������������� ����� "--append"
            | sort_order=="" -> (++)   --    ��� ������ ����� ���������� ("-ds")
            | otherwise      -> mergeFilelists sort_order find_group  -- ����� ������������ ����������� �������
    return$ mergeFunction list1 list2


-- |���������� ������� (����� �� ������ + ����� � �����)
mergeFilelists sort_order find_group filelist1 filelist2  =  dirs ++ files
  where -- ����� ����� � �������, ������������ ������ -ds, � �������� - � ������� "����+���"
    (dirs1,files1)  =  partition (fiSpecialFile.cfFileInfo) filelist1
    (dirs2,files2)  =  partition (fiSpecialFile.cfFileInfo) filelist2
    dirs  = merge (map2cmp (key_func "pn"       find_group . cfFileInfo)) dirs1  dirs2
    files = merge (map2cmp (key_func sort_order find_group . cfFileInfo)) files1 files2


-- | ������� �� ������ `originalList` ���-������� ��� ����������,
-- ������������ �������� ��������� � ����� ������
--
removeDuplicates originalList keyFunc eqFunc hashFunc = do
  table <- Hash.new eqFunc hashFunc

  -- �������� � ��� `table` ������� `value` ������, ���� � ��� ��� ��� �������� � ����� �� ������.
  -- �� ����������� �������� � ������ � ���� ����������� ������, ������� ������ ������������ `update`
  -- (to do: `reverse` �������� ������������ `update`)
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
---- ����� ����������� ����������� � ��������� ������ ������ �� ������ ��������/�����-������ -------
----------------------------------------------------------------------------------------------------

-- |���������� ��� ��������� ������ ������ (����� HEADER_BLOCK, ������� �� �� �������)
dir_compressor command  =  opt_dir_compressor command

-- |������� ������ ������ �� �����, ������ �� ������� ������ ����� ���� ���� ��������
splitToDirBlocks command  =  splitBy (opt_group_dir command) True


-- |�������� ���������� ���������� ��� ������ �� ������ (������������ �� ������� ����� � ������)
-- (����������� �����, ����� ���������, �� ���������, � �����, ������ ��������� �����������,
-- �������� ��� ���������)
data_compressor _       []                                = aNO_COMPRESSION
data_compressor command (file:_) | fiSpecialFile fi       = aNO_COMPRESSION
                                 | isCompressedFake file  = cfCompressor file
                                 | otherwise              = snd (types_of_compressors !! find_type fi)
  where fi = cfFileInfo file
        types_of_compressors = opt_data_compressor command
        find_type = opt_find_type command

-- |������� ������ ������ �� �����-����� � ������ ����� ������ (splitByType)
-- � �������� � opt_group_data ����������� ��� �����-������ (splitOneType).
-- ����� ����, ��� ������� ���������� (bwt, lzp) ������ �����-������
-- �������������� �������� ����� � ��������� ������.
-- �������, �������� � �����, ��������� ��������� ������������� (nodata/crconly),
-- �� ��������� �����-����� �� �����������.
splitToSolidBlocks command filelist  =  (dirs &&& [dirs]) ++ groupOn cfArcBlock solidBlocksToKeep
                                        ++ concatMap splitOneType  (splitByType filesToSplit)
  where
    -- �������� - ��������� ������
    (dirs,files)  =  partition (fiSpecialFile.cfFileInfo) filelist
    -- � �������� ����������� ������ � ��� --append ������ �������������� ��� ������ �����
    (solidBlocksToKeep, filesToSplit) | opt_keep_original command = partition isCompressedFile files
                                      | otherwise                 = ([],files)

    -- ������� ������ �� ����� ������ ($binary, $text...).
    -- ��� ���� �����, ��� ������ ��������� �����������, ������ �������� ������� ��� (�.�. �������� ������)
    splitByType filelist  =  map concat groups
      where
        (fake,normal) = partition isCompressedFake filelist
        normalGroups  = partitionList (opt_types_count command) (opt_find_type command . cfFileInfo) normal
        -- �������� ����� ����������� �� ������ �� ������������ ���������� ������
        fakeGroups    = sort_and_groupOn cfCompressor fake
        -- ... � ��� ������ ������������ � �������� ���������� ������, ������� ��������� ����� ���� �� �����������
        groups        = sort_and_groupOn (data_compressor command) (fakeGroups++filter (not.null) normalGroups)

    -- ������� �� �����-����� ������ ������ ������ ���� (�� ���� ��������� ����� ����������)
    splitOneType files | isFakeCompressor compressor = [files]  -- ��� �������� ������������ ��� ������ ��������� ���� �� �����
                       | compressor==aNO_COMPRESSION = [files]  -- ��� ��-������ � ���� ���� ��� ������
                       | otherwise =
        splitBy (opt_group_data command .$ addBlockSizeCrit) (opt_recompress command) files
      where
        -- ���������, ������������ ��� ������ ����� ���� (�.�. �������� ��� ���������)
        compressor = data_compressor command files
        -- ��� ������� ����������, ������������ � TTA/MM/JPG, ��������� �����-������
        -- ��� ������� ����������, ������������ � DICT - ���������� �����-���� �������� ����� DICT
        -- ��� ��������� ������� ���������� (grzip, lzp) alone - �������� ����� � ��������� ������
        addBlockSizeCrit = case compressor of
            algorithm:_ | makeNonSolid  algorithm          ->  const [GroupNone]
            algorithm:_ | isDICT_Method algorithm          ->  ([GroupByBlockSize $ freearcGetBlockSize algorithm]++)
            [algorithm] | freearcGetBlockSize algorithm>0  ->  ([GroupByBlockSize $ freearcGetBlockSize algorithm]++)
            _                                              ->  id

-- |���������� �� ��������� �����, ��������� ���� �������������� ����������,
-- � ��������� solid-�����? ���������� ����� ���� ������� ������ � ������,
-- ����� �������� �������� ������ "*8" � �� �������� ������ ":o", �� ����
-- ��� ������ ��������� ��� ����������� ������������������ ������, �������
-- ��� ����� ��������� �� ��������� �����
makeNonSolid m = any_function [isTTA_Method, isMM_Method, isJPG_Method] m
                 && (not (m `substr` "*8")  ||  (m `substr` ":o"))


----------------------------------------------------------------------------------------------------
---- ��������� ������ ������ �� ������ � ������������ � ��������� ���������� -----------------------
----------------------------------------------------------------------------------------------------

-- |������� ������ ������ �� ������ � ������������ � ��������� ���������� (��� ������ ������/��������)
-- ��� ���� � �������� ������ ��������� ������ ������ ������� ����������� �����, ���������� ���� �� ������ �� �������� ���������,
-- � ��� ���������� �����-������� �������� �� ��������� ��� ����, ����� �� �������������� �� �����-�����, ������� �������� ������ ����� ������� ����� ������
-- crits - �������� ��������� �� ������, recomress=True - ������������ ������� ������ �����-������
splitBy []    _          files = [files]  -- ���� ��� ��������� ��������� �� ��������� - ������� ��� ����� ����� �������
splitBy crits recompress files = splitByLen (computeLen) files where
  -- ���������� ���������� ������ �� ������ files, ������� ������ ����� � ��������� �����-����
  computeLen files = case () of
     _ | recompress     -> newLen   -- ��� --recompress �� ������ �� ������������ ������� ������� �����-�������
       | Just n<-oldLen -> n        -- ����������� ������������ �����-���� �� n ������
       | Just n<-oldPos -> if n<=newLen
                             then n        -- ������� ����� �����-���� ����� �� ������ ���������� �������
                             else minLen
       | otherwise      -> newLen   -- ������������ ��� ������ �����-���� �� �������
    where
      newLen = minimum$ map (`splitLen`    files) crits   -- ����� ������ �����-�����, ����������� �������������� ��������� ��������� �� �����-�����
      minLen = minimum$ map (`splitLenMin` files) crits   -- ���������� ���������� ����� �����-����� (� ������������ � 4 ���� ����������)
      maxLen = minimum$ map (`splitLenMax` files) crits   -- ...
      oldLen = solidBlockLen files                        -- ����� ������������� �����-�����, � �������� ���������� ���������� ������ ������, ��� Nothing
      oldPos = findSolidBlock minLen maxLen files         -- �������� ����� ������, ������� �������������� �������� � ����� �����-����, ������ ������� �����-�����

-- |����� ����������� ������ (���������� �������� ������ ������), ���������� ��������� ��������
splitLen  GroupNone              = const 1
splitLen  GroupByExt             = length.head.groupOn (fpLCExtension.fiFilteredName.cfFileInfo)
splitLen (GroupBySize      size) = (1+)      . groupLen (fiSize.cfFileInfo) (+) (<i size)
splitLen (GroupByBlockSize size) = atLeast 1 . groupLen (fiSize.cfFileInfo) (+) (<i size)
splitLen (GroupByNumber       n) = atLeast 1 . const n
splitLen  GroupAll               = const maxBound

-- |����� ����������� ������ ������, ����������� ��� �������� �������� (����� ������ ��������)
splitLenMin (GroupBySize      size) = splitLen (GroupBySize      (size `div` 2))
splitLenMin (GroupByBlockSize size) = splitLen (GroupByBlockSize (size `div` 2))
splitLenMin (GroupByNumber       n) = splitLen (GroupByNumber    (n    `div` 2))
splitLenMin x                       = splitLen x

-- |����� ������������ ������ ������, ����������� ��� �������� �������� (� 1.5 ���� ������ ��������)
splitLenMax (GroupBySize      size) = splitLen (GroupBySize      (size+(size `div` 2)))
splitLenMax (GroupByBlockSize size) = splitLen (GroupByBlockSize (size+(size `div` 2)))
splitLenMax (GroupByNumber       n) = splitLen (GroupByNumber    (n   +(n    `div` 2)))
splitLenMax x                       = splitLen x


----------------------------------------------------------------------------------------------------
---- ������������� ������������ solid-������ � ������ ��������� ������ -----------------------------
----------------------------------------------------------------------------------------------------

-- |���������, ��� ���������� ������ ������ - ������ ������ ������ � solid-�����
isWholeSolidBlock files @ (CompressedFile {cfArcBlock=solidBlock, cfPos=pos}:_) =
  pos == 0                            &&    -- ���� ������ ���� � ������ �������� ������� �����-����� (pos = ������ ������� �������������� ����� ����� ����� � �����-�����)
  blFiles solidBlock == length files  &&    --   ������ ����� �� �� �����, ��� � �����-����, � �������� ����������� ������ ���� � ������,
  all        isCompressedFile  files  &&    --   ������� ������ �� ������ ������,
  isEqOn     cfArcBlock        files  &&    --   ������������� ������ �����
  isSortedOn cfPos             files        --   � ��������������� �� ������� � �����-�����

isWholeSolidBlock _ = False

-- |����� ���������� �������� ������ ������, ����������� ����� �� ������ �����-����� (������, ������� ���� �����-����)
solidBlockLen []    = Nothing
solidBlockLen files = let n = blFiles (cfArcBlock (head files)) in   -- ���������� ������ � �����-�����, �������� ����������� 'head files'
                      if isCompressedFile (head files)           -- ���������� ������ ���������� �� ������� �����
                         && isWholeSolidBlock (take n files)     --   � �������� ������ �����-������
                      then Just n
                      else Nothing

-- |����� �����-����, ������������ � ����� �� ������ min..max ������ ������,
-- � ���������� ������ ��� ������� �����
findSolidBlock min max = fmap (+min)                       -- �������������� 'drop min'
                       . findIndex (isJust.solidBlockLen)
                       . take (max-min)
                       . tails
                       . drop min
