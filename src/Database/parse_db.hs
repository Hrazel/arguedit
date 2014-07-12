module SimpleDBI
    where

import Database.HDBC
import Database.HDBC.SqlValue
import Database.HDBC.Sqlite3
import Data.Maybe

default_dbfile = "argumentation.db"
db_file = "argumentation.db"

--extract_line :: [(String,SqlValue)] -> [(String,SqlValue)] 
extract_line line = map extract_elem line
--extract_elem :: (String,SqlValue) -> Data.ByteString.Internal.ByteString 
extract_elem (egal, SqlByteString the_string ) = the_string

extract_second (a, b) = b

connectToDB :: FilePath -> IO Connection
connectToDB file = do
    conn <- connectSqlite3 file
    putStrLn ("------------Connected to " ++ show file ++".")
    return conn

disconnectDB :: IConnection conn => conn -> IO ()
disconnectDB conn = do
    disconnect conn 
    putStrLn "------------Disconnected." 

--createTable :: IConnection conn => conn -> [Char] -> [SqlValue] -> IO Integer
{-createTable conn table_name = do
    --run conn ("CREATE TABLE "++ table_name ++" (id INTEGER NOT NULL, descr VARCHAR(80))") []
    run conn ("CREATE TABLE "++ table_name ++" (id INTEGER NOT NULL, term VARCHAR(80), op VARCHAR(80), descr VARCHAR(80))") []
    putStrLn ("------------Created table  " ++ table_name ++ ".")
-}
--createDB :: IConnection conn => conn -> [Char] -> [SqlValue] -> IO Integer
createDB conn dbname = do
    run conn ("CREATE DATABASE " ++ dbname ) []
    putStrLn ("------------Created DB  " ++ dbname ++ ".")

saveChanges :: IConnection conn => conn -> IO ()
saveChanges conn = do
    commit conn
    putStrLn "------------Saved changes."

selectAll conn table_name = do
--    conn <- connectToDB dbfile 
    putStrLn ("------------Select all from table " ++ table_name ++".")
    select_stmt <- prepare conn ("select * from " ++ table_name )
    execute select_stmt []
    results <- fetchAllRowsAL select_stmt
--    mapM_ print results 
    mapM_ print ( map (extract_line) results)
 
link_term_descr conn id t_id d_id = do
    ins_stmt <- prepare conn "INSERT INTO lnk_term_tbl VALUES (?,? ,?)"
    execute ins_stmt [ id, t_id, d_id]

insertTerm conn t_id term = do
    ins_stmt <- prepare conn "INSERT INTO term_tbl VALUES (? ,?)"
    execute ins_stmt [t_id, toSql term]

insertDef conn id lnk_term_id = do
    ins_stmt <- prepare conn "INSERT INTO defs_tbl VALUES (? ,?)"
    execute ins_stmt [ id,  lnk_term_id]

insertDescr conn d_id descr = do
    ins_stmt <- prepare conn "INSERT INTO descr_tbl VALUES (?,?)"
    execute ins_stmt [ d_id, toSql descr]
  
insertDefinition conn id term descr = do --[TEST]
    putStrLn ("------------Insert Def " ++ term ++ " = " ++ descr )
    raw_term_id <- getMaxId conn "term_tbl" 
    let term_id = incSqlID ( raw_term_id  )
    raw_descr_id <-  (getMaxId conn "descr_tbl" )
    let descr_id = incSqlID ( raw_descr_id  )
    raw_lnk_term_id <- (getMaxId conn "lnk_term_tbl")
    let lnk_term_id = incSqlID ( raw_lnk_term_id )
    insertTerm conn term_id term 
    insertDescr conn descr_id descr
    link_term_descr conn lnk_term_id term_id descr_id
    insertDef conn (toSql id) lnk_term_id

--incSqlID SqlString -> SqlString
incSqlID s = toSql ((fromSql (s)::Integer) +1)

--link_toplevel conn id from_id from_type_id to_id to_type_id = do

--defaultTo :: SqlValue -> SqlValue -> SqlValue
defaultTo v d -- = case v of {SqlNull -> d;Just v  -> x}
--    | [] = d
    | v == SqlNull = SqlString d
    | otherwise = v


showAll conn = do
    selectAll conn "lnk_toplevel_tbl"
    selectAll conn "defs_tbl"
    selectAll conn "lnk_term_tbl"
    selectAll conn "descr_tbl"
    selectAll conn "term_tbl"
       
getMaxId conn table = do
--    putStrLn "---------------GetMaxID"
    stmt <- prepare conn ("SELECT max(id) FROM " ++ table )
    execute stmt [] 
    results:rest <- fetchAllRowsAL stmt
    let secs = map (extract_second) results
--    let checked = fromMaybe   secs
    let maxId= (head secs)
    return ( (defaultTo maxId "1"))
--    return maxId
{- 
convRow :: [SqlValue] -> String
convRow [sqlId, sqlDesc] = 
    show intid ++ ": " ++ desc
    where   intid = (fromSql sqlId)::Integer
            desc = case fromSql sqlDesc of
                Just x -> x
                Nothing -> "NULL"
convRow x = fail $ "Unexpected result: " ++ show x

stringRows r = map convRow r
-}

test = do
    putStrLn "Initializing..."
    conn <- connectToDB "arg.db"
    {- create tables 
        -- fields in use {
            INTEGER NOT NULL
            VARCHAR(80)
        } 
        -- lnk_toplevel_tbl [ id| from_id| from_type_id| to_id | to_type_id ] 1.
        -- theses_tbl       [ id                                            ] 2.
        -- meta_tbl         [ id| author | date | ..                        ] 3.
        -- defs_tbl         [ id| lnk_term_id                               ] 4.
        -- lnk_term_tbl     [ id| term_id| descr_id                         ] 5.
        -- term_tbl         [ id| term                                      ] 6.
        -- descr_tbl        [ id| descr                                     ] 7.
    -}
    let id_modifiers = "PRIMARY_KEY AUTO_INCREMENT"
    run conn ("CREATE TABLE lnk_toplevel_tbl    (id INTEGER "++id_modifiers ++", from_id INTEGER NOT NULL, from_type_id INTEGER NOT NULL, to_id INTEGER NOT NULL, to_type_id INTEGER NOT NULL)") [] -- 1.
    run conn ("CREATE TABLE theses_tbl          (id INTEGER "++id_modifiers ++")") [] -- 2.
    run conn ("CREATE TABLE meta_tbl            (id INTEGER "++id_modifiers ++")") [] -- 3. 
    run conn ("CREATE TABLE defs_tbl            (id INTEGER "++id_modifiers ++", lnk_term INTEGER NOT NULL )") [] -- 4. 
    run conn ("CREATE TABLE lnk_term_tbl        (id INTEGER "++id_modifiers ++", term_id INTEGER NOT NULL, descr_id INTEGER NOT NULL)") [] -- 5.
    run conn ("CREATE TABLE term_tbl            (id INTEGER "++id_modifiers ++", term VARCHAR(30) )") [] -- 6. 
    run conn ("CREATE TABLE descr_tbl           (id INTEGER "++id_modifiers ++", descr VARCHAR(255) )") [] -- 7. 

    --(getTables conn)

    insertDefinition conn "1" "term1" "description1"
    insertDefinition conn "2" "term2" "description2"
    insertDefinition conn "3" "term3" "description3"
    insertDefinition conn "50" "term5" "description5"
    showAll conn
    maxId_str <- getMaxId conn "defs_tbl"
    putStrLn (fromSql maxId_str)
    --show maxId :: Int

--    saveChanges conn
    disconnectDB conn
    putStrLn "Init Done."

