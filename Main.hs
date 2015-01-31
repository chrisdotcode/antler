module Main where

import Control.Applicative ((<$>), (<|>))
import Data.ByteString as B (readFile)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Word (Word8)
import System.Environment (getArgs)

import qualified Data.Attoparsec.Binary as A (anyWord16le, anyWord32le)
import qualified Data.Attoparsec.ByteString as A
    ( Parser
    , anyWord8
    , parseOnly
    , take
    , word8
    )

main :: IO ()
main = do
    args <- getArgs
    case args of
        (x:_) -> B.readFile x >>= print . A.parseOnly xbase
        _      -> error "Please supply a .dbf file as the first arg."

data DBF = DBF
    { version               :: Version
    , lastUpdate            :: Day
    , numRecords            :: Int
    , lengthHeader          :: Int
    , lengthRecords         :: Int
    , incompleteTransaction :: Bool
    , encrypted             :: Bool
    , mdxFlag               :: Word8
    , languageDriver        :: LanguageDriver
    } deriving (Show)

xbase :: A.Parser DBF
xbase = do
    version'               <- versionParser
    lastUpdate'            <- lastUpdateParser
    numRecords'            <- numRecordsParser
    lengthHeader'          <- lengthHeaderParser
    lengthRecords'         <- lengthRecordsParser
    reservedParser
    incompleteTransaction' <- incompleteTransactionParser
    encrypted'             <- encryptedParser
    freeRecordThreadParser
    multiUserParser
    mdxFlag'               <- mdxFlagParser
    languageDriver'        <- languageDriverParser
    return $ DBF
        version'
        lastUpdate'
        numRecords'
        lengthHeader'
        lengthRecords'
        incompleteTransaction'
        encrypted'
        mdxFlag'
        languageDriver'

versionParser :: A.Parser Version
versionParser = toEnum . fromIntegral <$> A.anyWord8

-- Little-endian; Year value has a range within 0x0-0xFF, and 1900 is added to
-- that value. Therefore the date range is 1900-2155.
lastUpdateParser :: A.Parser Day
lastUpdateParser = do
    year  <- (+ 1900) . fromIntegral <$> A.anyWord8
    month <- fromIntegral <$> A.anyWord8
    day   <- fromIntegral <$> A.anyWord8
    return $ fromGregorian year month day

numRecordsParser :: A.Parser Int
numRecordsParser = fromIntegral <$> A.anyWord32le

lengthHeaderParser :: A.Parser Int
lengthHeaderParser = fromIntegral <$> A.anyWord16le

-- Sum of lengths of all fields + 1 (deletion flag), so - 1 from the field
-- value.
lengthRecordsParser :: A.Parser Int
lengthRecordsParser = (subtract 1) . fromIntegral <$> A.anyWord16le

-- Reserved for dBASE IV (value is 0x0000). Ignored.
reservedParser :: A.Parser ()
reservedParser = A.anyWord16le >> return ()

-- For dBASE IV.
incompleteTransactionParser :: A.Parser Bool
incompleteTransactionParser =
    (A.word8 0x00 >> return False) <|> (A.word8 0x01 >> return True)

-- For dBASE IV.
encryptedParser :: A.Parser Bool
encryptedParser =
    (A.word8 0x00 >> return False) <|> (A.word8 0x01 >> return True)

-- Free record thread. (Reserved for LAN only). Ignored.
freeRecordThreadParser :: A.Parser ()
freeRecordThreadParser = A.take 4 >> return ()

-- Reserved for multi-user dBASE; (dBASE III+ - )
multiUserParser :: A.Parser ()
multiUserParser = A.take 8 >> return ()

-- Little-endian; MDX flag (dBASE IV).
mdxFlagParser :: A.Parser Word8
mdxFlagParser = A.anyWord8

languageDriverParser :: A.Parser LanguageDriver
languageDriverParser = toEnum . fromIntegral <$> A.anyWord8

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
             | UnknownVersion
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
    toEnum _    = UnknownVersion

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

data LanguageDriver = DOSUSA                   -- DOS USA
                    | DOSMultilingual          -- DOS Multilingual
                    | WindowsANSI              -- Windows ANSI
                    | StandardMacintosh        -- Standard Macintosh
                    | EEMSDOS                  -- EE MS-DOS
                    | NordicMSDOS              -- Nordic MS-DOS
                    | RussianMSDOS             -- Russian MS-DOS
                    | IcelandicMSDOS           -- Icelandic MS-DOS
                    | KamenickyMSDOS           -- Kamenicky (Cz0xec) MS-DOS
                    | MazoviaMSDOS             -- Mazovia (Polish) MS-DOS
                    | GreekMSDOS               -- Greek MS-DOS (437G)
                    | TurkishMSDOS             -- Turkish MS-DOS
                    | RussianMacintosh         -- Russian Macintosh
                    | EasternEuropeanMacintosh -- Eastern European Macintosh
                    | GreekMacintosh           -- Greek Macintosh
                    | WindowsEE                -- Windows EE
                    | RussianWindows           -- Russian Windows
                    | TurkishWindows           -- Turkish Windows
                    | GreekWindows             -- Greek Windows
                    | UnknownLanguageDriver
                    deriving (Show)

instance Enum LanguageDriver where
    toEnum 0x01 = DOSUSA
    toEnum 0x02 = DOSMultilingual
    toEnum 0x03 = WindowsANSI
    toEnum 0x04 = StandardMacintosh
    toEnum 0x64 = EEMSDOS
    toEnum 0x65 = NordicMSDOS
    toEnum 0x66 = RussianMSDOS
    toEnum 0x67 = IcelandicMSDOS
    toEnum 0x68 = KamenickyMSDOS
    toEnum 0x69 = MazoviaMSDOS
    toEnum 0x6A = GreekMSDOS
    toEnum 0x6B = TurkishMSDOS
    toEnum 0x96 = RussianMacintosh
    toEnum 0x97 = EasternEuropeanMacintosh
    toEnum 0x98 = GreekMacintosh
    toEnum 0xC8 = WindowsEE
    toEnum 0xC9 = RussianWindows
    toEnum 0xCA = TurkishWindows
    toEnum 0xCB = GreekWindows
    toEnum _    = UnknownLanguageDriver

    fromEnum DOSUSA                   = 0x01
    fromEnum DOSMultilingual          = 0x02
    fromEnum WindowsANSI              = 0x03
    fromEnum StandardMacintosh        = 0x04
    fromEnum EEMSDOS                  = 0x64
    fromEnum NordicMSDOS              = 0x65
    fromEnum RussianMSDOS             = 0x66
    fromEnum IcelandicMSDOS           = 0x67
    fromEnum KamenickyMSDOS           = 0x68
    fromEnum MazoviaMSDOS             = 0x69
    fromEnum GreekMSDOS               = 0x6A
    fromEnum TurkishMSDOS             = 0x6B
    fromEnum RussianMacintosh         = 0x96
    fromEnum EasternEuropeanMacintosh = 0x97
    fromEnum GreekMacintosh           = 0x98
    fromEnum WindowsEE                = 0xC8
    fromEnum RussianWindows           = 0xC9
    fromEnum TurkishWindows           = 0xCA
    fromEnum GreekWindows             = 0xCB
