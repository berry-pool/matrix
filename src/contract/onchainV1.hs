module Onchain (mintSerializedMain, spendSerializedControl, mintSerializedControl, spendSerializedReference) where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.Script.Utils.V1.Scripts as Scripts
import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as Scripts
import qualified Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies as Scripts
import Plutus.Script.Utils.V1.Address as Scripts
import qualified Plutus.V1.Ledger.Api as Api
import Plutus.V1.Ledger.Contexts as Api
import Plutus.V1.Ledger.Value as V
import Plutus.V1.Ledger.Interval as I
import Ledger.Value as V
import qualified PlutusTx
import PlutusTx.Prelude
import Prelude (String)
import qualified Plutus.MerkleTree as MT
import Data.String (IsString)
import qualified PlutusTx.Builtins as Builtins
import qualified PlutusTx.AssocMap as M


-- Config ------------------------------------------------------------------

mainDetails :: MainDetails
mainDetails = MainDetails {
                        controlCs           = mintSymbolControl
                    ,   berryCs             = "a65e6e94d1a260dbc6c4d9319b45585fa54b83742a33a2c599df56b9"
                    ,   merkleRootMetadata  = "e44be91d0892234fc46b48878455b34ff9e1bd7d682517be80c4f87ddc7303ae"
                    ,   merkleRootAssigned  = "71be0c95e03f2bb5274c62ff3acb3c92c264f8aa0732dbfecc05657854563e49"
                    ,   refAddress          = spendAddrReference
                    ,   payeeAddress        = Api.Address (Api.PubKeyCredential "b6c8794e9a7a26599440a4d0fd79cd07644d15917ff13694f1f67235") Nothing
                    ,   paymentAmount       = 10000000
                    ,   mintStart           = 1661544401580
                    }

controlOwner :: Api.PubKeyHash
controlOwner = Api.PubKeyHash "b6c8794e9a7a26599440a4d0fd79cd07644d15917ff13694f1f67235"

controlOref :: Api.TxOutRef
controlOref = Api.TxOutRef "dc417f14fd93cf0ed03d6bbf9fe6a30939d9753b3a94b9393e6b82ad69ab1a0c" 0

-- Data and Redeemer ------------------------------------------------------------------

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

data RefAction = Burn | UpdateName

data ControlAction = Mint | Destroy

-- | The primary minting policy for the NFT collection ------------------------------------------------------------------
-- MintNFT: Mints a pair of user token and reference NFT according to CIP-0068.
-- BurnNFT: Destroys this pair again. Only the holder of the NFT is allowed to proceed with that action.
{-# INLINEABLE mintValidatorMain #-}
mintValidatorMain :: MainDetails -> MainAction -> Api.ScriptContext -> Bool
mintValidatorMain c action ctx = case action of
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

    prefixLength :: Integer
    prefixLength = 5

    checkMintNFT :: MT.Proof -> Buyer -> Bool
    checkMintNFT merkleProofMetadata buyer =
                            let 
                                -- | Output with reference NFT.
                                [((Api.DatumHash refOutDatumHash), refOutValue)] = scriptOutputsAtAddress (refAddress c) txInfo
                                [(refOutCs,Api.TokenName refOutName,refOutAm)] = V.flattenValue (V.noAdaValue refOutValue)
                                -- | Mint value (reference NFT and user token).
                                [(userCs, Api.TokenName userName, userAm), (refCs, Api.TokenName refName, refAm)] = V.flattenValue txMint
                                -- | Creating data for metadata merkle tree (combination of asset names and datum hash from reference output).
                                merkleEntryMetadata = userName <> refName <> refOutName <> refOutDatumHash
                            in 
                                -- | Berry holders have the privlige to buy before the 'public' mint starts. They are assigned a certain Matrix Berry which they can buy for half the price.
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
                    takeByteString prefixLength userName == "(222)" && takeByteString prefixLength refName == "(100)" &&
                    dropByteString prefixLength userName == dropByteString prefixLength refName


-- | The spending validator that holds the reference NFTs including the metadata ------------------------------------------------------------------
-- Burn: Destroys the UTxO in order to redeem the min ADA. This is only possible if the locked reference NFT and the belonging user token are burned within the same transaction.
-- UpdateName: Allows the holder of the user token to update the name in the metadata of the reference UTxO
{-# INLINEABLE spendValidatorReference #-}
spendValidatorReference :: DatumMetadata -> RefAction -> Api.ScriptContext -> Bool
spendValidatorReference datumMetadata action ctx = case action of
  Burn          -> checkBurn
  UpdateName    -> checkUpdateName
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
    prefixLength = 5

    ownOutputDatumMetadata :: DatumMetadata
    ownOutputValue :: V.Value
    (ownOutputDatumMetadata, ownOutputValue) = case getContinuingOutputs ctx of
      [o] -> let Just h = txOutDatumHash o in case Api.findDatum h txInfo of
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
                takeByteString prefixLength userName == "(222)" && takeByteString prefixLength refName == "(100)" &&
                dropByteString prefixLength userName == dropByteString prefixLength refName                       &&
                -- | Burned reference NFT needs to match the one in the own script UTxO
                ownName == refName

    checkUpdateName :: Bool
    checkUpdateName = 
                    let
                        [(ownCs, Api.TokenName ownName, _)] = V.flattenValue (V.noAdaValue ownValue)
                    in
                        -- | Input and output value of script UTxO stays the same.
                        ownValue == ownOutputValue &&
                        -- | Metadata stays immutable.
                        metadata datumMetadata == metadata ownOutputDatumMetadata && version datumMetadata == version ownOutputDatumMetadata &&
                        -- | User token belonging to reference NFT is provided.
                        providesUserToken ownCs (Api.TokenName ("(222)" <> dropByteString prefixLength ownName)) 1
                        -- Limit the size of identity TODO!!
                        --lengthOfByteString (Builtins.serialiseData (PlutusTx.toBuiltinData (identity ownOutputDatumMetadata))) <= 2000


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
-- Mint: Initialize 100 UTxOs at this script address with Datum set to 0 and lock in each the corresponding control NFT from the 'mintValidatorControl' minting policy.
-- Destroy: If the UTxO is moved and the Datum is set to 1, we are allowed to destroy the UTxO again in order to redeem the min ADA.
{-# INLINEABLE spendValidatorControl #-}
spendValidatorControl :: Api.PubKeyHash -> Api.CurrencySymbol -> Integer -> ControlAction -> Api.ScriptContext -> Bool
spendValidatorControl owner userCs isMinted action ctx = case action of
    Mint    -> checkIsMintable
    Destroy -> checkDestroy
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
        prefixLength = 11 -- label + 'matrix' == 11 bytes

        ownOutputDatum :: Integer
        ownOutputValue :: V.Value
        (ownOutputDatum, ownOutputValue) = case getContinuingOutputs ctx of
            [o] -> let Just h = txOutDatumHash o in case Api.findDatum h txInfo of
                Just (Api.Datum d) -> case PlutusTx.fromBuiltinData d of
                    Just m -> (m, txOutValue o)

        checkIsMintable :: Bool
        checkIsMintable = 
                        let 
                            [(mintUserCs, Api.TokenName mintUserTn, _), _] = flattenValue txMint
                            [(controlCs, Api.TokenName controlTn, _)] = flattenValue (V.noAdaValue ownValue)
                        in
                            if isMinted == 0 
                                then  ownValue == ownOutputValue && 
                                      mintUserCs == userCs && 
                                      dropByteString prefixLength mintUserTn == controlTn && 
                                      ownOutputDatum == 1
                                else False

        checkDestroy :: Bool
        checkDestroy = txInfo `txSignedBy` owner && isMinted == 1 && null (flattenValue txMint)

-- | Utils ------------------------------------------------------------------

{-# INLINEABLE scriptOutputsAtAddress #-}
scriptOutputsAtAddress :: Api.Address -> Api.TxInfo -> [(Api.DatumHash, V.Value)]
scriptOutputsAtAddress address p =
    let flt Api.TxOut{Api.txOutDatumHash=Just d, txOutAddress=address', txOutValue} | address == address' = Just (d, txOutValue)
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
    $$(PlutusTx.compile [|| wrap ||]) `PlutusTx.applyCode` PlutusTx.liftCode mainDetails
      where
        wrap c = Scripts.mkUntypedMintingPolicy $ mintValidatorMain c

mintSymbolMain :: Api.CurrencySymbol
mintSymbolMain = Scripts.scriptCurrencySymbol mintInstanceMain

spendInstanceReference :: Scripts.Validator
spendInstanceReference =
  Api.mkValidatorScript
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator spendValidatorReference

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
                      ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Api.unMintingPolicyScript mintInstanceMain) :: PlutusScript PlutusScriptV1)

mintSerializedControl :: String
mintSerializedControl = C.unpack $ B16.encode $ serialiseToCBOR 
                      ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Api.unMintingPolicyScript mintInstanceControl) :: PlutusScript PlutusScriptV1)

spendSerializedControl :: String
spendSerializedControl = C.unpack $ B16.encode $ serialiseToCBOR 
                        ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Api.unValidatorScript spendInstanceControl) :: PlutusScript PlutusScriptV1)

spendSerializedReference :: String
spendSerializedReference = C.unpack $ B16.encode $ serialiseToCBOR 
                            ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Api.unValidatorScript spendInstanceReference) :: PlutusScript PlutusScriptV1)

-- | Lift ------------------------------------------------------------------

PlutusTx.makeLift ''RefAction
PlutusTx.makeIsDataIndexed ''RefAction [('Burn, 0), ('UpdateName, 1)]
PlutusTx.makeLift ''Buyer
PlutusTx.makeIsDataIndexed ''Buyer [('BerryHolder, 0), ('NoHolder, 1)]
PlutusTx.makeLift ''ControlAction
PlutusTx.makeIsDataIndexed ''ControlAction [('Mint, 0), ('Destroy, 1)]
PlutusTx.makeLift ''DatumMetadata
PlutusTx.makeIsDataIndexed ''DatumMetadata [('DatumMetadata, 0)]
PlutusTx.makeLift ''MainDetails
PlutusTx.makeIsDataIndexed ''MainDetails [('MainDetails, 0)]
PlutusTx.makeLift ''MainAction
PlutusTx.makeIsDataIndexed ''MainAction [('MintNFT, 0), ('BurnNFT, 1)]
PlutusTx.makeLift ''MT.Hash 

-- | This is handy to make use of direct hash string literals.
deriving via Api.LedgerBytes instance IsString MT.Hash