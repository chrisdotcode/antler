module Main where

import Control.Applicative ((<$>))
import Data.ByteString as B (readFile)
import System.Environment (getArgs)

import Data.Attoparsec.ByteString

main :: IO ()
main = do
    args <- getArgs
    case args of
        (x:_) -> B.readFile x >>= print . parseOnly xbase
        _      -> error "Please supply a .dbf file as the first arg."

data DBF = DBF { version :: Version } deriving (Show)

xbase :: Parser DBF
xbase = DBF <$> versionParser

versionParser :: Parser Version
versionParser = toEnum . fromIntegral <$> anyWord8

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
    toEnum 0x30 = VisualFoxPro -- 0x30 also == VisualFoxProDBC; what's the
                               -- difference?
    toEnum 0x31 = VisualFoxProAutoInc
    toEnum 0x43 = DBVMemo
    toEnum 0x7B = DBASEIVMemo
    toEnum 0x83 = DBT
    toEnum 0x83 = DBASEIIIMemo
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
