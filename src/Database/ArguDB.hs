module Database.ArguDB
    where

import Database.HDBC
import Database.HDBC.SqlValue
import Database.HDBC.Sqlite3
import Data.Maybe

default_dbfile = "argumentation.db"
db_file = "argumentation.db"

--extract_line :: [(String,SqlValue)] -> [(String,SqlValue)] 
--extract_line line = map extract_elem line
--extract_elem :: (String,SqlValue) -> Data.ByteString.Internal.ByteString 
--extract_elem (egal, SqlByteString the_string ) = the_string

extract_second (a, b) = b

convertFromSql a = fromSql a
convertPairFromSql (a,b) = (a, convertFromSql b)

--convertListFromSql [] = []
--convertListFromSql (x:xs) = [a]:b  
  --  where   a = convertPairFromSql x
    --        b = convertListFromSql xs


connectToDB :: FilePath -> IO Connection
connectToDB file = do
    conn <- connectSqlite3 file
    putStrLn ("------------Connected to " ++ show file ++".")
    return conn

disconnectDB :: IConnection conn => conn -> IO ()
disconnectDB conn = do
    disconnect conn 
    putStrLn "------------Disconnected." 

--createDB :: IConnection conn => conn -> [Char] -> [SqlValue] -> IO Integer
createDB conn dbname = do
    run conn ("CREATE DATABASE " ++ dbname ) []
    putStrLn ("------------Created DB  " ++ dbname ++ ".")

saveChanges :: IConnection conn => conn -> IO ()
saveChanges conn = do
    commit conn
    putStrLn "------------Saved changes."

selectAll conn table_name = do
    putStrLn ("selectAll:-----------Select all from table " ++ table_name ++".")
    select_stmt <- prepare conn ("select * from " ++ table_name )
    execute select_stmt []
    results <- sFetchAllRows select_stmt
--    end_results <- mapM_ (\s -> fromSql( s )) results
    mapM_ print ( results )
    return results 

getTheses conn = do
    putStrLn ("getTheses:-----------Select id, text from table theses_tbl IF toplevel")
    select_stmt <- prepare conn 
        ("select ths.id, ths.text from theses_tbl ths, toplevel_froms_view fv WHERE ths.id = fv.from_id" )
    execute select_stmt []
    results <- sFetchAllRows select_stmt
    mapM_ print ( results )
    return results 


getDefs conn = do
    putStrLn ("getDefs:-----------Select id, text from table defs_tbl IF toplevel")
    select_stmt <- prepare conn 
        ("select defs.id, defs.term, defs.descr from defs_view defs, toplevel_froms_view fv WHERE defs.id = fv.from_id AND fv.type_name = \'DEFINITION\'" )
    execute select_stmt []
    results <- sFetchAllRows select_stmt
    mapM_ print ( results )
    return results 


selectTopLevel conn typestr  = do
    let table_name = "toplevel_froms_view"
    putStrLn ("selectTopLevel:------------Select all from table " ++ table_name ++".")
    select_stmt <- prepare conn ("select * from " ++ table_name ++" WHERE type_name = '" ++ typestr ++ "'" )
    execute select_stmt []
    results <- sFetchAllRows select_stmt
    mapM_ print ( results )
    return results 

link_toplevel conn id from_id from_type_id to_id to_type_id = do
    ins_stmt <- prepare conn "INSERT INTO lnk_toplevel_tbl VALUES (?, ? ,?, ?, ?)"
    execute ins_stmt [ toSql id, toSql from_id, toSql from_type_id, toSql to_id, toSql to_type_id ]
 
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
 
insertTh conn id text = do
    insertStraight conn "theses_tbl" id text
--    ins_stmt <- prepare conn "INSERT INTO theses_tbl VALUES (?,?)"
--    execute ins_stmt [ id, toSql text] 

insertStraight conn table id text = do
    ins_stmt <- prepare conn ("INSERT INTO "++ table ++" VALUES (?,?)")
    execute ins_stmt [ toSql id, toSql text] 


nextId conn tbl = do
    raw_nid <- getMaxId conn tbl 
    let n_id = incSqlID ( raw_nid  )
    return n_id

insertDefinition conn id term descr = do
    term_id <- nextId conn "term_tbl"
    descr_id <- nextId conn "descr_tbl"
    lnk_term_id <- nextId conn "lnk_term_tbl"
    insertTerm conn term_id term 
    insertDescr conn descr_id descr
    link_term_descr conn lnk_term_id term_id descr_id
    insertDef conn (toSql id) lnk_term_id

insertTheses conn id text = do
    --t_id <- nextId conn "theses_tbl"
    insertTh conn id text

--incSqlID SqlString -> SqlString
incSqlID s = toSql ((fromSql (s)::Integer) +1)

--link_toplevel conn id from_id from_type_id to_id to_type_id = do

--defaultTo :: SqlValue -> SqlValue -> SqlValue
defaultTo v d 
    | v == SqlNull = SqlString d
    | otherwise = v


showAll conn = do 
    putStrLn "##########Show all tables################"
    selectAll conn "lnk_toplevel_tbl"
    selectAll conn "toplevel_froms_view"
    selectAll conn "defs_tbl"
    selectAll conn "lnk_term_tbl"
    selectAll conn "descr_tbl"
    selectAll conn "term_tbl"
    selectAll conn "defs_view"
    selectAll conn "theses_tbl"
    selectAll conn "types_tbl"
    
       
getMaxId conn table = do
    stmt <- prepare conn ("SELECT max(id) FROM " ++ table )
    execute stmt [] 
    results:rest <- fetchAllRowsAL stmt
    let secs = map (extract_second) results
    let maxId= (head secs)
    return ( (defaultTo maxId "1"))
{-
fromSqlList :: [SqlValue] -> [String]
fromSqlList (sv:rest) = 
--    frommmed_sv  fromSql sv
    return ((fromSql sv):(fromSqlList rest))
fromSqlList (sv:[])   = 
    return ([(fromSql sv)]++[])
-} 

fill_dummy_data conn = do
    insertDefinition conn "1" "Free-Software" "description1"
    insertDefinition conn "2" "Commercial-Software" "description2"
    insertDefinition conn "3" "Open-Source-Software" "description3"
    insertDefinition conn "50" "term5" "description5"
    insertTheses conn "70" "My inserted thesis!"
    insertTheses conn "71" "My second inserted thesis!"
    insertTheses conn "72" "And a third inserted thesis!"
    insertStraight conn "types_tbl" "1" "THESIS"
    insertStraight conn "types_tbl" "2" "DEFINITION"
    insertStraight conn "types_tbl" "3" "PREMISE"
    insertStraight conn "types_tbl" "4" "LOGIC"
    insertStraight conn "types_tbl" "5" "META"
    
    link_toplevel conn "1" "70" "1"  "3" "2"
    link_toplevel conn "2" "72" "1"  "2" "2"
--    link_toplevel conn "2" "72" "1"  "2" "2"
--    link_toplevel conn "3" "3" "2"  "2" "2"

test_db conn = do
    fill_dummy_data conn
    showAll conn

init_db conn = do
    putStrLn "Initializing..."
--    conn <- connectToDB filename 
    {- create tables 
        -- fields in use {
            INTEGER NOT NULL
            VARCHAR(80)
        } 
        -- lnk_toplevel_tbl [ id| from_id| from_type_id| to_id | to_type_id ] 1.
        -- theses_tbl       [ id| text                                      ] 2.
        -- meta_tbl         [ id| author | date | ..                        ] 3.
        -- defs_tbl         [ id| lnk_term_id                               ] 4.
        -- lnk_term_tbl     [ id| term_id| descr_id                         ] 5.
        -- term_tbl         [ id| term                                      ] 6.
        -- descr_tbl        [ id| descr                                     ] 7.
        -- defs_view        [ defs.id| l.id| t.erm| d.descr                 ] 8.
        -- types_tbl        [ id| name                                      ] 9.
        -- toplevel_froms_view    [ id| from_id| from_type_name             ] 10.
    -}
    let id_modifiers = "PRIMARY_KEY AUTO_INCREMENT"
    run conn ("CREATE TABLE lnk_toplevel_tbl    (id INTEGER "++id_modifiers ++", from_id INTEGER NOT NULL, from_type_id INTEGER NOT NULL, to_id INTEGER NOT NULL, to_type_id INTEGER NOT NULL)") [] -- 1.
    run conn ("CREATE TABLE theses_tbl           (id INTEGER "++id_modifiers ++", text VARCHAR(255) )") [] -- 2. 
    run conn ("CREATE TABLE meta_tbl            (id INTEGER "++id_modifiers ++")") [] -- 3. 
    run conn ("CREATE TABLE defs_tbl            (id INTEGER "++id_modifiers ++", lnk_term_id INTEGER NOT NULL )") [] -- 4. 
    run conn ("CREATE TABLE lnk_term_tbl        (id INTEGER "++id_modifiers ++", term_id INTEGER NOT NULL, descr_id INTEGER NOT NULL)") [] -- 5.
    run conn ("CREATE TABLE term_tbl            (id INTEGER "++id_modifiers ++", term VARCHAR(30) )") [] -- 6. 
    run conn ("CREATE TABLE descr_tbl           (id INTEGER "++id_modifiers ++", descr VARCHAR(255) )") [] -- 7. 
    run conn ("CREATE VIEW defs_view AS SELECT defs.id as id, l.id as l_id, t.term as term, d.descr as descrFROM defs_tbl defs, lnk_term_tbl l, term_tbl t, descr_tbl d WHERE defs.lnk_term_id = l.id AND l.term_id = t.id AND l.descr_id = d.id ") [] -- 8. 
    run conn ("CREATE TABLE types_tbl           (id INTEGER "++id_modifiers ++", name VARCHAR(255) )") [] -- 9.
    run conn ("CREATE VIEW toplevel_froms_view AS SELECT lnk_toplevel_tbl.id as id, from_id, types.name as type_name FROM lnk_toplevel_tbl, types_tbl types WHERE from_type_id = types.id") [] -- 10.

    --(getTables conn)
    
    --show maxId :: Int
    
    putStrLn "Init Done."
--    return conn 
--    saveChanges conn
    --disconnectDB conn

