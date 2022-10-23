{-# LANGUAGE TypeApplications, AllowAmbiguousTypes #-}

module Onchain (mintSerializedMain, spendSerializedControl, mintSerializedControl, spendSerializedReference) where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.Script.Utils.V2.Scripts as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as Scripts
import Plutus.Script.Utils.V2.Address as Scripts
import qualified Plutus.V2.Ledger.Api as Api
import Plutus.V2.Ledger.Contexts as Api
import Plutus.V1.Ledger.Value as V
import Plutus.V1.Ledger.Interval as I
import Ledger.Value as V
import qualified PlutusTx
import PlutusTx.Prelude
import Prelude (String)
import qualified Prelude as Haskell
import qualified Plutus.MerkleTree as MT
import Data.String (IsString)
import qualified PlutusTx.Builtins as Builtins
import qualified PlutusTx.AssocMap as M
import qualified Data.ByteString as BS

import Data.Digest.CRC as CRC
import Data.Digest.CRC8 as CRC
import qualified Data.Bits as BIT
import qualified Data.Bits.ByteString as BIT
import Numeric (readHex)
import Data.Word
import Data.Int
import qualified Data.ByteString.Char8 as CH8
import qualified Data.ByteString.Builder as BLD
import qualified Data.Binary.Builder as BIN

-- | Labels (CIP-0067) ------------------------------------------------------------------

class HexBuilder a where
  buildHex :: a -> BIN.Builder

instance HexBuilder Word8 where
  buildHex = BLD.word8Hex

-- | Show a bytestring as hex
prettyPrint :: BS.ByteString -> BS.ByteString
prettyPrint = builderToBS . BLD.byteStringHex

-- | Convert a builder type to a bytestring
builderToBS :: BLD.Builder -> BS.ByteString
builderToBS = LBS.toStrict . BLD.toLazyByteString

-- | Convert Int16 to their big endian two byte representation
intLabel :: Int16 -> BS.ByteString
intLabel = builderToBS . BLD.int16BE

-- | returns in hex
checksum :: BS.ByteString -> BS.ByteString
checksum m = (builderToBS . buildHex . crcWord) (CRC.digest m :: CRC8)

-- | Get the asset label for a given Int16 as a bytestring.
getLabel :: Int16 -> BS.ByteString
getLabel n =  let label = BLD.int16BE $ Haskell.fromIntegral n
                  checksumLabel = (Haskell.fst . Haskell.head . readHex . CH8.unpack . checksum  . builderToBS) label
                  checkSum = BLD.int8 $ Haskell.fromIntegral checksumLabel
              in BIT.shift (builderToBS $ label Haskell.<> checkSum Haskell.<> BLD.int8 0) (-4)

toLabel :: Int16 -> Label
toLabel n = toBuiltin $ getLabel n


labelLength :: Integer
labelLength = 4 -- 4 bytes

-- TEST

-- import qualified Plutus.V1.Ledger.Contexts as Api1
-- import qualified Plutus.V1.Ledger.Api as Api1
-- import qualified Plutus.Script.Utils.V1.Scripts as Scripts1
-- import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as Scripts1
-- import qualified Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies as Scripts1

-- instance Eq SwapScriptContext where
--   {-# INLINABLE (==) #-}
--   SwapScriptContext a b == SwapScriptContext c d = a == c && b == d

-- instance Eq SwapScriptPurpose where
--   {-# INLINABLE (==) #-}
--   AMinting a == AMinting b = a == b
--   ASpending a == ASpending b = a == b

-- -- instance Eq AuctionTxInfo where
-- --     {-# INLINABLE (==) #-}
-- --     AuctionTxInfo i o f m c w r s d tid == AuctionTxInfo i' o' f' m' c' w' r' s' d' tid' =
-- --         -- i == i' && o == o' && f == f' && m == m' && 
-- --         c == c' && w == w' && r == r' && s == s' && d == d' && tid == tid'


-- data SwapScriptContext = SwapScriptContext
--   { sScriptContextTxInfo :: AuctionTxInfo,
--     sScriptContextPurpose :: SwapScriptPurpose
--   }

-- data SwapScriptPurpose = AMinting CurrencySymbol | ASpending TxOutRef

-- -- data AuctionTxInInfo = AuctionTxInInfo
-- --   { atxInInfoOutRef           :: TxOutRef
-- --   , atxInInfoResolved         :: AuctionTxOut
-- --   }

-- data AuctionTxInfo = AuctionTxInfo
--   { atxInfoInputs             :: [Api.TxInInfo]
--   , atxInfoReferenceInputs     :: [Api.TxInInfo] 
--   , atxInfoOutputs            :: [Api.TxOut]
--   , atxInfoFee                :: V.Value
--   , atxInfoMint               :: V.Value
--   , atxInfoDCert              :: [Api.DCert]
--   , atxInfoWdrl               :: Api.Map Api.StakingCredential Integer
--   , atxInfoValidRange         :: Api.POSIXTimeRange
--   , atxInfoSignatories        :: [Api.PubKeyHash]
--   , txInfoRedeemers           :: Api.Map Api.ScriptPurpose Api.Redeemer
--   , atxInfoData               :: Api.Map Api.DatumHash Api.Datum
--   , atxInfoId                 :: Api.TxId
--   }

-- PlutusTx.makeLift ''SwapScriptContext
-- -- PlutusTx.makeIsDataIndexed ''SwapScriptContext [('SwapScriptContext, 0)]
-- PlutusTx.unstableMakeIsData ''SwapScriptContext
-- PlutusTx.makeLift ''SwapScriptPurpose
-- PlutusTx.unstableMakeIsData ''SwapScriptPurpose
-- PlutusTx.makeLift ''AuctionTxInfo
-- PlutusTx.unstableMakeIsData ''AuctionTxInfo




-- data TestAction = Mint1 | Burn1
-- PlutusTx.unstableMakeIsData ''TestAction
-- PlutusTx.makeLift ''TestAction


-- {-# INLINEABLE mintTestValidator #-}
-- mintTestValidator :: Integer -> Api.ScriptContext -> Bool
-- mintTestValidator i ctx = i > 1000
--   where
--     txInfo :: Api.TxInfo
--     txInfo = Api.scriptContextTxInfo ctx



-- mintTestInstance :: Scripts.MintingPolicy
-- mintTestInstance =
--   Api.mkMintingPolicyScript $
--     $$(PlutusTx.compile [|| wrap ||])
--       where
--         wrap = Scripts.mkUntypedMintingPolicy $ mintTestValidator
--         -- wrap  :: (Api1.FromData a, Api1.FromData b) => (a -> b -> Bool)
--         --   -> BuiltinData
--         --   -> BuiltinData
--         --   -> ()
--         -- wrap f b c
--         --   = check
--         --     ( f
--         --         (let Just f = PlutusTx.fromBuiltinData b in f) 
--         --         (let Just e = PlutusTx.fromBuiltinData c in e)
--         --     )
--         -- wrap2 = wrap mintTestValidator


-- mintSerializedTest :: String
-- mintSerializedTest = C.unpack $ B16.encode $ serialiseToCBOR 
--                       ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Api.unMintingPolicyScript mintTestInstance) :: PlutusScript PlutusScriptV1)


--

-- | Config ------------------------------------------------------------------

mainDetails :: MainDetails
mainDetails = MainDetails {
                        controlCs           = mintSymbolControl
                    ,   berryCs             = "a65e6e94d1a260dbc6c4d9319b45585fa54b83742a33a2c599df56b9"
                    ,   merkleRootMetadata  = "f77dd3546d8f0fc78d869394c3fbd0f3bb09a9a1988896325f2255fec3fc6fad"
                    ,   merkleRootAssigned  = "c237972884731f38919d629ae8a18c45badd0472f6d59984ba57fb1faf0e1330"
                    ,   refAddress          = spendAddrReference
                    ,   payeeAddress        = Api.Address (Api.PubKeyCredential "b6c8794e9a7a26599440a4d0fd79cd07644d15917ff13694f1f67235") Nothing
                    ,   paymentAmount       = 10000000
                    ,   mintStart           = 1662467777264
                    }

controlOwner :: Api.PubKeyHash
controlOwner = "de467543f7cee91138085797279a458e74020c30be0b325ceada11d2"

controlOref :: Api.TxOutRef
controlOref = Api.TxOutRef "59e82f54f10f1cf823a0c168dd8d977ff9db943b6b5171707f1743a08296582b" 2

-- | Data and Redeemer ------------------------------------------------------------------

type Label = BuiltinByteString

type Metadata = M.Map BuiltinData BuiltinData
data DatumMetadata = DatumMetadata {
                        metadata    :: Metadata
                    ,   version     :: Integer
                    }

data MainDetails = MainDetails { 
                        controlCs            :: Api.CurrencySymbol
                     ,  berryCs              :: Api.CurrencySymbol
                     ,  merkleRootMetadata   :: MT.Hash
                     ,  merkleRootAssigned   :: MT.Hash
                     ,  refAddress           :: Api.Address
                     ,  payeeAddress         :: Api.Address
                     ,  paymentAmount        :: Integer  
                     ,  mintStart            :: Api.POSIXTime
                     }

data Buyer = BerryHolder Api.TokenName MT.Proof | NoHolder

data MainAction = MintNFT MT.Proof Buyer | BurnNFT

data RefAction = Burn | UpdateDescription

data ControlAction = Mint | Destroy

data ControlDatum = MintStatus Integer | DeployScripts

-- | The primary minting policy for the NFT collection ------------------------------------------------------------------
-- MintNFT: Mints a pair of user token and reference NFT according to CIP-0068.
-- BurnNFT: Destroys this pair again. Only the holder of the NFT is allowed to proceed with that action.
{-# INLINEABLE mintValidatorMain #-}
mintValidatorMain :: (Label, Label) -> MainDetails -> MainAction -> Api.ScriptContext -> Bool
mintValidatorMain (label100, label222) c action ctx = case action of
  MintNFT metadataMerkleProof buyer -> checkMintNFT metadataMerkleProof buyer
  BurnNFT                           -> checkBurnNFT
 
  where
    txInfo :: Api.TxInfo
    txInfo = Api.scriptContextTxInfo ctx

    txInputs :: [Api.TxInInfo]
    txInputs = Api.txInfoInputs txInfo

    txMint :: V.Value
    txMint = Api.txInfoMint txInfo

    ownSymbol :: Api.CurrencySymbol
    ownSymbol = Api.ownCurrencySymbol ctx

    checkMintNFT :: MT.Proof -> Buyer -> Bool
    checkMintNFT merkleProofMetadata buyer =
                            let 
                                -- | Output with reference NFT.
                                [((Api.OutputDatumHash (Api.DatumHash refOutDatumHash)), refOutValue)] = scriptOutputsAtAddress (refAddress c) txInfo
                                [(refOutCs,Api.TokenName refOutName,refOutAm)] = V.flattenValue (V.noAdaValue refOutValue)
                                -- | Mint value (reference NFT and user token).
                                [(userCs, Api.TokenName userName, userAm), (refCs, Api.TokenName refName, refAm)] = V.flattenValue txMint
                                -- | Creating data for metadata merkle tree (combination of asset names and datum hash from reference output).
                                merkleEntryMetadata = userName <> refName <> refOutName <> refOutDatumHash
                            in 
                                -- | Berry holders have the privilege to buy before the 'public' mint starts. They are assigned a certain Matrix Berry which they can buy for half the price.
                                (case buyer of
                                    BerryHolder berryName merkleProofAssigned   ->  -- | Only privileges before public sale.
                                                                                    Api.to (mintStart c) `I.contains` Api.txInfoValidRange txInfo                                                           &&
                                                                                    -- | Check if Berry holder.
                                                                                    any (\(TxInInfo _ out) -> V.assetClassValueOf (Api.txOutValue out) (V.assetClass (berryCs c) berryName) == 1) txInputs  &&
                                                                                    -- | Checking if correct Matrix Berry is minted, which was assigned through the merkle tree.
                                                                                    MT.member (userName <> (Api.unTokenName berryName)) (merkleRootAssigned c) merkleProofAssigned                          &&
                                                                                    -- | Pay for NFT (only half the price) and send to correct address.
                                                                                    V.assetClassValueOf (valuePaidToAddress txInfo (payeeAddress c)) (V.assetClass V.adaSymbol V.adaToken) >= paymentAmount c `divide` 2

                                    NoHolder                                    ->  -- | Can only buy at start of public sale.
                                                                                    Api.from (mintStart c) `I.contains` Api.txInfoValidRange txInfo  &&
                                                                                    -- | Pay for NFT and send to correct address.
                                                                                    V.assetClassValueOf (valuePaidToAddress txInfo (payeeAddress c)) (V.assetClass V.adaSymbol V.adaToken) >= paymentAmount c)
                                                                                                                                                            &&
                                -- | Forcing to append datum with metadata to witness set (this will expose the metadata and not only the hash).
                                isJust (Api.findDatum (Api.DatumHash refOutDatumHash) txInfo)                                                               &&
                                -- | Matching policy ids and quantities.
                                1 == refOutAm && 1 == userAm && 1 == refAm                                                                                  &&
                                ownSymbol == refOutCs && ownSymbol == userCs && ownSymbol == refCs                                                          &&
                                -- | Checking if metadata and asset names belong together and are part of the merkle tree.
                                MT.member merkleEntryMetadata (merkleRootMetadata c) merkleProofMetadata                                                    &&
                                -- | Ensuring uniqueness of NFT by checking if control UTxO was spent.
                                any (\(TxInInfo _ out) -> isJust (M.lookup (controlCs c) (getValue (Api.txOutValue out)))) txInputs

    checkBurnNFT :: Bool
    checkBurnNFT =  
                let
                    -- | Allow burning only one pair (reference NFT and user token) at once.
                    [(userCs, Api.TokenName userName, userAm), (refCs, Api.TokenName refName, refAm)] = V.flattenValue txMint
                in
                    -- | Matching policy ids and quantities.
                    -1 == userAm && -1 == refAm &&
                    ownSymbol == userCs && ownSymbol == refCs &&
                    -- | Matching asset names.
                    takeByteString labelLength userName == label222 && takeByteString labelLength refName == label100 &&
                    dropByteString labelLength userName == dropByteString labelLength refName


-- | The spending validator that holds the reference NFTs including the metadata ------------------------------------------------------------------
-- Burn: Destroys the UTxO in order to redeem the min ADA. This is only possible if the locked reference NFT and the belonging user token are burned within the same transaction.
-- UpdateName: Allows the holder of the user token to update the name in the metadata of the reference UTxO
{-# INLINEABLE spendValidatorReference #-}
spendValidatorReference :: (Label, Label) -> DatumMetadata -> RefAction -> Api.ScriptContext -> Bool
spendValidatorReference (label100, label222) datumMetadata action ctx = case action of
  Burn              -> checkBurn
  UpdateDescription -> checkUpdateDescription
  where
    txInfo :: Api.TxInfo
    txInfo = Api.scriptContextTxInfo ctx

    txMint :: V.Value
    txMint = Api.txInfoMint txInfo

    ownValue :: V.Value
    ownValue =  let Just i = Api.findOwnInput ctx
                    out = txInInfoResolved i
                in txOutValue out

    ownOutputDatumMetadata :: DatumMetadata
    ownOutputValue :: V.Value
    (ownOutputDatumMetadata, ownOutputValue) = case getContinuingOutputs ctx of
      [o] -> let (Api.OutputDatumHash h) = txOutDatum o in case Api.findDatum h txInfo of
        Just (Api.Datum d) -> case PlutusTx.fromBuiltinData d of
          Just m -> (m, txOutValue o)

    providesUserToken :: Api.CurrencySymbol -> Api.TokenName -> Integer -> Bool
    providesUserToken cs tn am = any (\(Api.TxInInfo _ out) -> valueOf (txOutValue out) cs tn == am) (txInfoInputs txInfo)

    checkBurn :: Bool
    checkBurn = 
            let
                -- | Allow burning only one pair (reference NFT and user token) at once.
                [(userCs, Api.TokenName userName, userAm), (refCs, Api.TokenName refName, refAm)] = V.flattenValue txMint
                [(ownCs, Api.TokenName ownName, _)] = V.flattenValue (V.noAdaValue ownValue)
            in
                -- | Matching policy ids and quantities.
                -1 == userAm && -1 == refAm &&
                ownCs == userCs && ownCs == refCs && 
                -- | Matching asset names.
                takeByteString labelLength userName == label222 && takeByteString labelLength refName == label100 &&
                dropByteString labelLength userName == dropByteString labelLength refName                       &&
                -- | Burned reference NFT needs to match the one in the own script UTxO
                ownName == refName

    checkUpdateDescription :: Bool
    checkUpdateDescription = 
                    let
                        [(ownCs, Api.TokenName ownName, _)] = V.flattenValue (V.noAdaValue ownValue)
                        [oldName, oldImage, oldId, (oldDescriptionKey, _)] = M.toList (metadata datumMetadata)
                        [newName, newImage, newId, (newDescriptionKey, rawDescr)] = M.toList (metadata ownOutputDatumMetadata)
                        Just descr = (PlutusTx.fromBuiltinData rawDescr) :: Maybe BuiltinByteString
                    in
                        -- | Input and output value of script UTxO stays the same.
                        V.noAdaValue ownValue == V.noAdaValue ownOutputValue                                                    &&
                        -- | Metadata version stays the same.
                        version datumMetadata == version ownOutputDatumMetadata                                                 &&
                        -- | User token belonging to reference NFT is provided.
                        providesUserToken ownCs (Api.TokenName (label222 <> dropByteString labelLength ownName)) 1              &&
                        -- | Keep primary metadata immutable
                        oldName == newName && oldImage == newImage && oldId == newId && oldDescriptionKey == newDescriptionKey  &&
                        -- | Limit size of description
                        lengthOfByteString descr <= 256

-- | A one shot minting policy to authenticate the control UTxOs with control NFTs ------------------------------------------------------------------
-- Mint: Mint 100 control tokens in one batch and send them to their corresponding output address of the 'spendValidatorControl' script.
-- Destroy: Redeeming UTxOs from 'spendValidatorControl' script allows you to also burn these control tokens.
{-# INLINEABLE mintValidatorControl #-}
mintValidatorControl :: Api.TxOutRef -> ControlAction -> Api.ScriptContext -> Bool
mintValidatorControl oref action ctx = case action of
    Mint    -> checkSpending
    Destroy -> checkDestroy
  where
    txInfo :: Api.TxInfo
    txInfo = Api.scriptContextTxInfo ctx

    txMint :: V.Value
    txMint = Api.txInfoMint txInfo

    checkSpending :: Bool
    checkSpending = Api.spendsOutput txInfo (Api.txOutRefId oref) (Api.txOutRefIdx oref)

    checkDestroy :: Bool
    checkDestroy = all (\(_,_,am) -> am < 0) (flattenValue txMint)


-- | The spending validator that checks for uniqueness of NFTs ------------------------------------------------------------------
-- Datum MintStatus:
--  Mint: Initialize 100 UTxOs at this script address with Datum set to 0 and lock in each the corresponding control NFT from the 'mintValidatorControl' minting policy.
--  Destroy: If the UTxO is moved and the Datum is set to 1, we are allowed to destroy the UTxO again in order to redeem the min ADA.
-- Datum DeployScripts: Deploy other necessary scripts to reduce minting costs, which can be redeemed again at the end. 
{-# INLINEABLE spendValidatorControl #-}
spendValidatorControl :: Api.PubKeyHash -> Api.CurrencySymbol -> ControlDatum -> ControlAction -> Api.ScriptContext -> Bool
spendValidatorControl owner userCs datum action ctx = case datum of
  DeployScripts       -> txInfo `txSignedBy` owner
  MintStatus isMinted -> case action of
    Mint    -> checkIsMintable isMinted
    Destroy -> checkDestroy isMinted
  where
      txInfo :: Api.TxInfo
      txInfo = Api.scriptContextTxInfo ctx

      txMint :: V.Value
      txMint = Api.txInfoMint txInfo

      ownValue :: V.Value
      ownValue =  let Just i = Api.findOwnInput ctx
                      out = txInInfoResolved i
                  in txOutValue out

      prefixLength :: Integer
      prefixLength = labelLength + 6  -- label + 'matrix' == 11 bytes

      ownOutputDatum :: Integer
      ownOutputValue :: V.Value
      (ownOutputDatum, ownOutputValue) = case getContinuingOutputs ctx of
          [o] ->  let (Api.OutputDatum (Api.Datum d)) = txOutDatum o in 
                    case PlutusTx.fromBuiltinData d of
                      Just m -> (m, txOutValue o)

      checkIsMintable :: Integer -> Bool
      checkIsMintable isMinted = 
                      let 
                          [(mintUserCs, Api.TokenName mintUserTn, _), _] = flattenValue txMint
                          [(_, Api.TokenName controlTn, _)] = flattenValue (V.noAdaValue ownValue)
                      in
                          if isMinted == 0 
                              then V.noAdaValue ownValue == V.noAdaValue ownOutputValue && 
                                    mintUserCs == userCs && 
                                    dropByteString prefixLength mintUserTn == controlTn && 
                                    ownOutputDatum == 1
                              else False

      checkDestroy :: Integer -> Bool
      checkDestroy isMinted =  
                      let 
                          [(ownCs, ownTn, _)] = flattenValue (V.noAdaValue ownValue)
                      in 
                          txInfo `txSignedBy` owner && isMinted == 1 && V.assetClassValueOf txMint (V.assetClass ownCs ownTn) < 0

-- | Utils ------------------------------------------------------------------

{-# INLINEABLE scriptOutputsAtAddress #-}
scriptOutputsAtAddress :: Api.Address -> Api.TxInfo -> [(Api.OutputDatum, V.Value)]
scriptOutputsAtAddress address p =
    let flt Api.TxOut{Api.txOutDatum=d, txOutAddress=address', txOutValue} | address == address' = Just (d, txOutValue)
        flt _ = Nothing
    in mapMaybe flt (Api.txInfoOutputs p)

{-# INLINEABLE pubKeyOutputsAtAddress #-}
pubKeyOutputsAtAddress :: Api.Address -> Api.TxInfo -> [V.Value]
pubKeyOutputsAtAddress address p =
    let flt Api.TxOut{txOutAddress=address', txOutValue} | address == address' = Just txOutValue
        flt _ = Nothing
    in mapMaybe flt (Api.txInfoOutputs p)

{-# INLINABLE valuePaidToAddress #-}
valuePaidToAddress :: Api.TxInfo -> Api.Address -> V.Value
valuePaidToAddress ptx address = mconcat (pubKeyOutputsAtAddress address ptx)

-- | Instantiate validators ------------------------------------------------------------------

mintInstanceMain :: Scripts.MintingPolicy
mintInstanceMain =
  Api.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])`PlutusTx.applyCode` PlutusTx.liftCode ((toLabel 100, toLabel 222)) `PlutusTx.applyCode` PlutusTx.liftCode mainDetails
      where
        wrap l c = Scripts.mkUntypedMintingPolicy $ mintValidatorMain l c

mintSymbolMain :: Api.CurrencySymbol
mintSymbolMain = Scripts.scriptCurrencySymbol mintInstanceMain

spendInstanceReference :: Scripts.Validator
spendInstanceReference =
  Api.mkValidatorScript $
    $$(PlutusTx.compile [||wrap||]) `PlutusTx.applyCode` PlutusTx.liftCode ((toLabel 100, toLabel 222))
  where
    wrap l = Scripts.mkUntypedValidator $ spendValidatorReference l

spendAddrReference :: Api.Address
spendAddrReference = Scripts.mkValidatorAddress spendInstanceReference


mintInstanceControl :: Scripts.MintingPolicy
mintInstanceControl =
  Api.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||]) `PlutusTx.applyCode` PlutusTx.liftCode controlOref
      where
        wrap c = Scripts.mkUntypedMintingPolicy $ mintValidatorControl c

mintSymbolControl :: Api.CurrencySymbol
mintSymbolControl = Scripts.scriptCurrencySymbol mintInstanceControl


spendInstanceControl :: Scripts.Validator
spendInstanceControl =
  Api.mkValidatorScript $ 
    $$(PlutusTx.compile [||wrap||]) `PlutusTx.applyCode` PlutusTx.liftCode controlOwner `PlutusTx.applyCode` PlutusTx.liftCode mintSymbolMain
  where
    wrap owner cs = Scripts.mkUntypedValidator $ spendValidatorControl owner cs

spendAddrControl :: Api.Address
spendAddrControl = Scripts.mkValidatorAddress spendInstanceControl


-- | Serialization ------------------------------------------------------------------

mintSerializedMain :: String
mintSerializedMain = C.unpack $ B16.encode $ serialiseToCBOR 
                      ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Api.unMintingPolicyScript mintInstanceMain) :: PlutusScript PlutusScriptV2)

mintSerializedControl :: String
mintSerializedControl = C.unpack $ B16.encode $ serialiseToCBOR 
                      ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Api.unMintingPolicyScript mintInstanceControl) :: PlutusScript PlutusScriptV2)

spendSerializedControl :: String
spendSerializedControl = C.unpack $ B16.encode $ serialiseToCBOR 
                        ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Api.unValidatorScript spendInstanceControl) :: PlutusScript PlutusScriptV2)

spendSerializedReference :: String
spendSerializedReference = C.unpack $ B16.encode $ serialiseToCBOR 
                            ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Api.unValidatorScript spendInstanceReference) :: PlutusScript PlutusScriptV2)

-- | Lift ------------------------------------------------------------------

PlutusTx.makeLift ''RefAction
PlutusTx.makeIsDataIndexed ''RefAction [('Burn, 0), ('UpdateDescription, 1)]
PlutusTx.makeLift ''Buyer
PlutusTx.makeIsDataIndexed ''Buyer [('BerryHolder, 0), ('NoHolder, 1)]
PlutusTx.makeLift ''ControlAction
PlutusTx.makeIsDataIndexed ''ControlAction [('Mint, 0), ('Destroy, 1)]
PlutusTx.makeLift ''ControlDatum
PlutusTx.makeIsDataIndexed ''ControlDatum [('MintStatus, 0), ('DeployScripts, 1)]
PlutusTx.makeLift ''DatumMetadata
PlutusTx.makeIsDataIndexed ''DatumMetadata [('DatumMetadata, 0)]
PlutusTx.makeLift ''MainDetails
PlutusTx.makeIsDataIndexed ''MainDetails [('MainDetails, 0)]
PlutusTx.makeLift ''MainAction
PlutusTx.makeIsDataIndexed ''MainAction [('MintNFT, 0), ('BurnNFT, 1)]
PlutusTx.makeLift ''MT.Hash 

-- | This is handy to make use of direct hash string literals.
deriving via Api.LedgerBytes instance IsString MT.Hash