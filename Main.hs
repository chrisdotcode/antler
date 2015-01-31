module Main where

import Control.Applicative ((<$>), (<|>))
import Data.ByteString as B (readFile)
import Data.Time.Calendar (Day, fromGregorian)
import System.Environment (getArgs)

import Data.Attoparsec.Binary (anyWord16le, anyWord32le)
import Data.Attoparsec.ByteString (Parser, anyWord8, parseOnly, word8)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (x:_) -> B.readFile x >>= print . parseOnly xbase
        _      -> error "Please supply a .dbf file as the first arg."

data DBF = DBF
    { version               :: Version
    , lastUpdate            :: Day
    , numRecords            :: Int
    , lengthHeader          :: Int
    , lengthRecords         :: Int
    , incompleteTransaction :: Bool
    , encrypted             :: Bool
    } deriving (Show)

xbase :: Parser DBF
xbase = do
    version'               <- versionParser
    lastUpdate'            <- lastUpdateParser
    numRecords'            <- numRecordsParser
    lengthHeader'          <- lengthHeaderParser
    lengthRecords'         <- lengthRecordsParser
    reservedParser
    incompleteTransaction' <- incompleteTransactionParser
    encrypted'             <- encryptedParser
    return $ DBF
        version'
        lastUpdate'
        numRecords'
        lengthHeader'
        lengthRecords'
        incompleteTransaction'
        encrypted'

versionParser :: Parser Version
versionParser = toEnum . fromIntegral <$> anyWord8

-- Little-endian; Year value has a range within 0x0-0xFF, and 1900 is added to
-- that value. Therefore the date range is 1900-2155.
lastUpdateParser :: Parser Day
lastUpdateParser = do
    year  <- (+ 1900) . fromIntegral <$> anyWord8
    month <- fromIntegral <$> anyWord8
    day   <- fromIntegral <$> anyWord8
    return $ fromGregorian year month day

numRecordsParser :: Parser Int
numRecordsParser = fromIntegral <$> anyWord32le

lengthHeaderParser :: Parser Int
lengthHeaderParser = fromIntegral <$> anyWord16le

-- Sum of lengths of all fields + 1 (deletion flag), so - 1 from the field
-- value.
lengthRecordsParser :: Parser Int
lengthRecordsParser = (subtract 1) . fromIntegral <$> anyWord16le

-- Reserved for dBASE IV (value is 0x0000). Ignored.
reservedParser :: Parser ()
reservedParser = anyWord16le >> return ()

-- For dBASE IV.
incompleteTransactionParser :: Parser Bool
incompleteTransactionParser =
    (word8 0x00 >> return False) <|> (word8 0x01 >> return True)

-- For dBASE IV.
encryptedParser :: Parser Bool
encryptedParser = (word8 0x00 >> return False) <|> (word8 0x01 >> return True)

data Version = FoxBase             -- FoxBase
             | NoDBT               -- File without DBT
             | DBASEIVNoMemo       -- dBASE IV w/o memo file
             | DBASEVNoMemo        -- dBASE V w/o memo file
             | VISUALOBJECTSNoMemo -- VISUAL OBJECTS (first 1.0 versions) for
                                   -- the Dbase III files w/o memo file
             | VisualFoxPro
             | VisualFoxProDBC     -- Visual FoxPro w. DBC
             | VisualFoxProAutoInc -- Visual FoxPro w. AutoIncrement field
             | DBVMemo             -- .dbv memo var size (Flagship)
             | DBASEIVMemo         -- dBASE IV with memo
             | DBT                 -- File with DBT
             | DBASEIIIMemo        -- dBASE III+ with memo file
             | VISUALOBJECTSMemo   -- VISUAL OBJECTS (first 1.0 versions) for
                                   -- the Dbase III files (NTX clipper driver)
                                   -- with memo file
             | DBASEIVSQL          -- dBASE IV w. SQL table
             | DBVAndDBTMemo       -- .dbv and .dbt memo (Flagship)
             | ClipperSIXSMTMemo   -- Clipper SIX driver w. SMT memo file. Note!
                                   -- Clipper SIX driver sets lowest 3 bytes to
                                   -- 110 in descriptor of crypted databases.
                                   -- So, 3->6, 83h->86h, F5->F6, E5->E6 etc.
             | FoxProMemo          -- FoxPro w. memo file
             | FoxProUnknown       -- FoxPro ???
             deriving (Show)

instance Enum Version where
    toEnum 0x02 = FoxBase
    toEnum 0x03 = NoDBT
    toEnum 0x04 = DBASEIVNoMemo
    toEnum 0x05 = DBASEVNoMemo
    toEnum 0x07 = VISUALOBJECTSNoMemo
    toEnum 0x30 = VisualFoxPro -- 0x30 also == VisualFoxProDBC
    toEnum 0x31 = VisualFoxProAutoInc
    toEnum 0x43 = DBVMemo
    toEnum 0x7B = DBASEIVMemo
    toEnum 0x83 = DBT
    -- Matches the above, and there is no factor to discern between the two, so
    -- just assume every field that matches 83 is DBT:
    -- toEnum 0x83 = DBASEIIIMemo
    toEnum 0x87 = VISUALOBJECTSMemo
    toEnum 0x8B = DBASEIVMemo
    toEnum 0x8E = DBASEIVSQL
    toEnum 0xB3 = DBVAndDBTMemo
    toEnum 0xE5 = ClipperSIXSMTMemo
    toEnum 0xF5 = FoxProMemo
    toEnum 0xFB = FoxProUnknown

    fromEnum FoxBase             = 0x02
    fromEnum NoDBT               = 0x03
    fromEnum DBASEIVNoMemo       = 0x04
    fromEnum DBASEVNoMemo        = 0x05
    fromEnum VISUALOBJECTSNoMemo = 0x07
    fromEnum VisualFoxPro        = 0x30
    fromEnum VisualFoxProDBC     = 0x30
    fromEnum VisualFoxProAutoInc = 0x31
    fromEnum DBVMemo             = 0x43
    fromEnum DBASEIVMemo         = 0x7B -- or 0x8B; what's the difference?
    fromEnum DBT                 = 0x83
    fromEnum DBASEIIIMemo        = 0x83
    fromEnum VISUALOBJECTSMemo   = 0x87
    fromEnum DBASEIVSQL          = 0x8E
    fromEnum DBVAndDBTMemo       = 0xB3
    fromEnum ClipperSIXSMTMemo   = 0xE5
    fromEnum FoxProMemo          = 0xF5
    fromEnum FoxProUnknown       = 0xFB
