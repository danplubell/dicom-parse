{-# LANGUAGE OverloadedStrings #-}
module Data.DICOM.Parse where

import           Data.Binary
import           Data.Binary.Get
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import           Data.DICOM.Dictionary
import           Data.DICOM.Model.DataElement
import           Data.DICOM.Model.Dictionary
import           Data.DICOM.Model.Parser
import           Data.DICOM.Model.RegisteredUID


parseDicomContent :: DicomDictionary -> UIDDictionary -> BL.ByteString -> [DataElement]
parseDicomContent dicomdict uiddict = runGet (makeSoup dicomdict uiddict)

makeSoup:: DicomDictionary -> UIDDictionary -> Get [DataElement]
makeSoup dicomdict uiddict = do
  header <- deserializeHeader dicomdict
  --lookup the transfer syntax based on the UID
  let ts = getTransferSyntax uiddict header
  decodeElements dicomdict ts


getTransferSyntax::UIDDictionary -> FileMetaInformation -> TransferSyntax
getTransferSyntax uiddict fmi  =
  let tsUID = lookupUIDType uiddict ((deRawValue.transferSyntaxUID) fmi)  -- this is the DicomUID of the transfer syntax
  in case tsUID of
          ImplicitVRLittleEndian         -> TransferSyntax LittleEndian Implicit tsUID
          ExplicitVRLittleEndian         -> TransferSyntax LittleEndian Implicit tsUID
          DeflatedExplicitVRLittleEndian -> TransferSyntax LittleEndian Implicit tsUID
          ExplicitVRBigEndian            -> TransferSyntax BigEndian    Explicit tsUID
          _                              -> TransferSyntax LittleEndian Explicit tsUID


deserializeHeader:: DicomDictionary -> Get FileMetaInformation
deserializeHeader dd = do
  let ts = TransferSyntax LittleEndian Explicit ExplicitVRLittleEndian
  _                  <- getByteString 128 -- preamble
  _                  <- getByteString 4   -- 'DICM'
  metaLen            <- decodeElement dd ts
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


decodeElement :: DicomDictionary  -> TransferSyntax -> Get DataElement
decodeElement dd ts  = do
  let fword16 = case tsEndianType ts of
                     BigEndian    -> getWord16be
                     LittleEndian -> getWord16le

  let fword32 = case tsEndianType ts of
                     BigEndian    -> getWord32be
                     LittleEndian -> getWord32le

  tagv <- getTag fword16
  if fst tagv == 0xFFFE
    then decodeItem fword32 tagv
    else
      case tsVREncoding ts of
        Implicit -> do
                   vlv <- fword32
                   vrv <- lookupVRByTag dd tagv
                   rw  <- if vlv == 0 || vlv == 0xFFFFFFFF || vrv == "SQ"
                            then return BS.empty
                            else getByteString (fromIntegral vlv)
                   return $! Element tagv vrv vlv rw

        Explicit -> do
                   vrv       <- getByteString 2
                   vlv <- if vrv `elem` ["OW","OB","OF","OD","SQ","UC","UR","UT","UN"]
                            then do
                                 _ <- fword16
                                 fword32
                            else do
                                 l <- fword16
                                 return (fromIntegral l)
                   rw <- if vlv == 0 || vlv == 0xFFFFFFFF || vrv == "SQ"
                           then return BS.empty
                           else getByteString (fromIntegral vlv)
                   return $! Element tagv vrv vlv rw

lookupVRByTag:: DicomDictionary->(Word16,Word16) -> Get BS.ByteString
lookupVRByTag dict tagv = do
  let e = lookupElementByTag dict tagv
  case e of
     Nothing -> error $  "The tag was not found in the dictionary: " ++ show tagv
     Just elem' -> return $ vr elem'
{-
decodeElement'::Get DataElement
decodeElement'= do
  tagv <- getTag
  if fst tagv == 0xFFFE
   then decodeItem tagv
   else do

       vrv       <- getByteString 2
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
       return $! DataElement tagv vrv vlv rw
-}
decodeItem :: Get Word32->(Word16,Word16)-> Get DataElement
decodeItem fendian tagv = do

  vlv  <- fendian
  rw <- if vlv == 0 || vlv == 0xFFFFFFFF
           then return BS.empty
           else getByteString (fromIntegral vlv)

  return $ Item tagv  vlv rw

getTag :: Get Word16 -> Get (Word16,Word16)
getTag f = do
    grpNbr <- f
    elemNbr <- f
    return (grpNbr,elemNbr)

decodeElements::DicomDictionary -> TransferSyntax -> Get [DataElement]
decodeElements dict ts = do
  empty <- isEmpty
  if empty
     then return []
     else do element  <- decodeElement dict ts
             elements <- decodeElements dict ts
             return (element:elements)
