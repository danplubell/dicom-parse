{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DICOM.Parse
-- Copyright   :  (C) 2016 Dan Plubell
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Dan Plubell <danplubell@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.DICOM.Parse where

import           Control.Monad.Loops
import           Data.Binary
import           Data.Binary.Get
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BL
import           Data.Char
import           Data.DICOM.Dictionary
import           Data.DICOM.Model
import           Data.Int
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as E
import           Debug.Trace
import           Numeric
defaultTransferSyntax::TransferSyntax
defaultTransferSyntax= TransferSyntax LittleEndian Explicit ExplicitVRLittleEndian
-- | Parse dicom data that is from a dicom file; including preamble, dicm file indicator, and file header meta information
--   An error is thrown if the "DICM" indicator is missing
parseDicomFileContent::BL.ByteString -> [DataElement]
parseDicomFileContent bs = let dd             =  loadElementDictionary
                               uidd           =  loadUIDDictionary
                               headerElements = parseDicomFileHeader  bs
                               ts             = getTransferSyntax uidd (fst headerElements)
                           in fst headerElements `mappend` parseDicomContent dd ts (BL.drop (fromIntegral (snd headerElements)) bs)

-- | Parse binary  content into a list of dicom data elements
parseDicomContent :: DicomDictionary   -> TransferSyntax -> BL.ByteString -> [DataElement]
parseDicomContent dicomdict ts = runGet (decodeElements dicomdict  ts)

parseDicomFileHeader ::  BL.ByteString -> ([DataElement],Int64)
parseDicomFileHeader   = runGet deserializeHeader

-- | Get the transfer syntax information from the dicom file meta information
getTransferSyntax::UIDDictionary -> [DataElement] -> TransferSyntax
getTransferSyntax uiddict header  =
    let tsElement = head $ filter filterby  header  --TODO don't assume that there is a transfer syntax tag
        rawUID  (UIVal v) = E.encodeUtf8 v
        rawUID  _         = BS.empty
        tsUID = lookupUIDType uiddict (rawUID $ getDicomValue defaultTransferSyntax tsElement)
    in case tsUID of
          ImplicitVRLittleEndian         -> TransferSyntax LittleEndian Implicit tsUID
          ExplicitVRLittleEndian         -> TransferSyntax LittleEndian Implicit tsUID
          DeflatedExplicitVRLittleEndian -> TransferSyntax LittleEndian Implicit tsUID
          ExplicitVRBigEndian            -> TransferSyntax BigEndian    Explicit tsUID
          _                              -> TransferSyntax LittleEndian Explicit tsUID
    where filterby e  = deTag e  == (0x0002,0x0010)

deserializeHeader:: Get ([DataElement],Int64)
deserializeHeader  = do
  _                     <- getByteString 128 -- preamble
  dicm                  <- getByteString 4   -- 'DICM'
  case dicm of
    "DICM" -> do
             metaLenElem           <- decodeHeaderElement
             let len               = runGet getWord32le (BL.fromStrict $ deRawValue metaLenElem)
             headerBytes           <- getByteString (fromIntegral len)

             let deList                =   metaLenElem: runGet decodeHeaderElements (BL.fromStrict headerBytes)
             parseByteCnt          <- bytesRead
             return (deList,parseByteCnt)
    _      -> error "The data doesn't appear to be from a DICOM file"

-- | Parse the dicom file header into the meta information structure
{-deserializeHeader':: DicomDictionary -> Get FileMetaInformation
deserializeHeader' dd = do
  let ts = TransferSyntax LittleEndian Explicit ExplicitVRLittleEndian
  _                  <- getByteString 128 -- preamble
  dicm                  <- getByteString 4   -- 'DICM'
  traceM $ show dicm
  metaLen            <- decodeElement dd ts
  let len = runGet getWord32le (BL.fromStrict $ deRawValue metaLen)
  headerBytes <- getByteString (fromIntegral len)
  let headerDeList = runGet (decodeElements dd ts) (BL.fromStrict headerBytes)
  traceM $ show headerDeList
  metaVersion        <- decodeElement dd ts
  storageSOP         <- decodeElement dd ts
  storageSopI        <- decodeElement dd ts
  transferSyntax     <- decodeElement dd ts
  implementation     <- decodeElement dd ts
  implementVersion   <- decodeElement dd ts
  sourceAE           <- decodeElement dd ts
  sendingAE          <- decodeElement dd ts
  receivingAE        <- decodeElement dd ts
  privateInfoCreator <- decodeElement dd ts
  privInfo           <- decodeElement dd ts
  return $ FileMetaInformation metaLen metaVersion storageSOP storageSopI transferSyntax implementation
                               implementVersion sourceAE sendingAE receivingAE privateInfoCreator privInfo
-}

decodeHeaderElement::Get DataElement
decodeHeaderElement = do
  tagv <- getTag getWord16le
  vrv  <- getByteString 2
  vlv <- if vrv `elem` ["OW","OB","OF","OD","SQ","UC","UR","UT","UN"]
           then do
                _ <- getWord16le
                getWord32le
           else do
                l <- getWord16le
                return (fromIntegral l)
  rw <- if vlv == 0 || vlv == 0xFFFFFFFF || vrv == "SQ"
          then return BS.empty
          else getByteString (fromIntegral vlv)
  return $! Element tagv (toVR vrv) vlv rw

decodeElement :: DicomDictionary  -> TransferSyntax -> Get [DataElement]
decodeElement dd ts  = do
  let fword16 = case tsEndianType ts of
                     BigEndian    -> getWord16be
                     LittleEndian -> getWord16le

  let fword32 = case tsEndianType ts of
                     BigEndian    -> getWord32be
                     LittleEndian -> getWord32le

  tagv <- getTag fword16
  traceM $ "decodeElement tagv: " ++ showHex (fst tagv) ""  ++ " " ++ showHex (snd tagv) ""
  if fst tagv == 0xFFFE
    then decodeItem fword32 tagv
--      vlv <- fword32
--      return $! Item tagv vlv BS.empty
    else
      case tsVREncoding ts of
        Implicit -> do
                   vlv <- fword32
                   vrv <- lookupVRByTag dd tagv
                   rw  <- if vlv == 0 || vlv == 0xFFFFFFFF || vrv == "SQ"
                            then return BS.empty
                            else getByteString (fromIntegral vlv)
                   return  [Element tagv (toVR vrv) vlv rw]

        Explicit -> do
                   vrv       <- getByteString 2
                   vlv <- if vrv `elem` ["OW","OB","OF","OD","SQ","UC","UR","UT","UN"]
                            then do
                                 _ <- fword16
                                 fword32
                            else do
                                 l <- fword16
                                 return (fromIntegral l)
                   rw <- if vlv == 0 || vlv == 0xFFFFFFFF || vrv == "SQ" || vrv == "OB"
                           then return BS.empty
                           else getByteString (fromIntegral vlv)
                   return  [Element tagv (toVR vrv) vlv rw]

-- | Lookup the VR by the element tag.  An error is thrown if the tag is not found in the dictionary
lookupVRByTag:: DicomDictionary->(Word16,Word16) -> Get BS.ByteString
lookupVRByTag dict tagv = do
  let e = lookupElementByTag dict tagv
  case e of
     Nothing -> error $  "The tag was not found in the dictionary: " ++ show tagv
     Just elem' -> return $ vr elem'

-- | Decode an item in a nested element
decodeItem :: Get Word32->(Word16,Word16)-> Get [DataElement]
decodeItem fendian tagv = do
  traceM $ "item tag: " ++ show tagv
  vlv  <- fendian
--  rw <- if vlv == 0 || vlv == 0xFFFFFFFF
--           then return BS.empty
--           else getByteString (fromIntegral vlv)

  return  [Item tagv  vlv BS.empty] --rw

-- | The tag tuple parser
getTag :: Get Word16 -> Get (Word16,Word16)
getTag f = do
    grpNbr <- f
    elemNbr <- f
    return (grpNbr,elemNbr)


decodeElements::DicomDictionary -> TransferSyntax -> Get [DataElement]
decodeElements dict ts = do elementLists <- untilM  (decodeElement dict ts) isEmpty
                            return $ concat elementLists

decodeHeaderElements ::Get [DataElement]
decodeHeaderElements = untilM decodeHeaderElement isEmpty

{-
-- | Parser for decoding elements
decodeElements'::DicomDictionary -> TransferSyntax -> Get [DataElement]
decodeElements' dict ts = do
  empty <- isEmpty
  if empty
     then return []
     else do element  <- decodeElement dict ts
             elements <- decodeElements dict ts
             return (element:elements)

-}
getDicomValue::TransferSyntax -> DataElement -> DicomValue a
getDicomValue ts d =
  let fword32  =
                case tsEndianType ts of
                     BigEndian    -> getWord32be
                     LittleEndian -> getWord32le
  in
     case deVR d of
         UL -> ULVal (runGet fword32 (BL.fromStrict $ deRawValue d))
         UI -> UIVal ((T.dropAround isControl .E.decodeUtf8) (deRawValue d))
         _  -> RawVal (deRawValue d)
