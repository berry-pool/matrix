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
import qualified Plutus.MerkleTree as MT
import Data.String (IsString)
import qualified PlutusTx.Builtins as Builtins


-- Config

mainContractDetails :: MainContractDetails
mainContractDetails = MainContractDetails {
                        registryOref        = Api.TxOutRef "b26a00bc3e795092f08ffeda4e8d74ae64a9f0690c7edbdc049f7beed4f54046" 2
                    ,   registryTokenName   = "(110)Registry"
                    ,   controlCs           = mintSymbolControl
                    ,   berryCs             = "15be994a64bdb79dde7fe080d8e7ff81b33a9e4860e9ee0d857a8e85"
                    ,   merkleRootMetadata  = "cdea074b1b95cea4f1dec8b70d8b78a490658f816d02fa421469870c7cea5e6f"
                    ,   merkleRootAssigned  = "cdea074b1b95cea4f1dec8b70d8b78a490658f816d02fa421469870c7cea5e6f"
                    ,   refAddress          = spendAddrReference
                    ,   payeeAddress        = spendAddrReference
                    ,   paymentAmount       = 10000000
                    ,   mintStart           = 0
                    }

controlOwner :: Api.PubKeyHash
controlOwner = Api.PubKeyHash ""

controlOref :: Api.TxOutRef
controlOref = Api.TxOutRef "b26a00bc3e795092f08ffeda4e8d74ae64a9f0690c7edbdc049f7beed4f54046" 2

userNamePrefix :: BuiltinByteString
userNamePrefix = "(222)Matrix"

-- Data and Redeemer

type Metadata = Api.Map BuiltinData BuiltinData
data DatumMetadata = DatumMetadata {
                        metadata    :: Metadata
                    ,   version     :: Integer
                    }

data MainContractDetails = MainContractDetails { 
                        registryOref         :: Api.TxOutRef
                     ,  registryTokenName    :: Api.TokenName
                     ,  controlCs            :: Api.CurrencySymbol
                     ,  berryCs              :: Api.CurrencySymbol
                     ,  merkleRootMetadata   :: MT.Hash
                     ,  merkleRootAssigned   :: MT.Hash
                     ,  refAddress           :: Api.Address
                     ,  payeeAddress         :: Api.Address
                     ,  paymentAmount        :: Integer  
                     ,  mintStart            :: Api.POSIXTime
                     }

data Buyer = BerryHolder Api.TokenName MT.Proof | NoHolder

data MainContractAction = MintNFT MT.Proof Buyer | BurnNFT | CreateRegistry | MintRegistryNFT

data RefAction = Burn | UpdateName

data ControlAction = Mint | Destroy

-- Validators

-- The primary minting policy for the NFT collection
{-# INLINEABLE mintValidatorMain #-}
mintValidatorMain :: MainContractDetails -> MainContractAction -> Api.ScriptContext -> Bool
mintValidatorMain c action ctx = case action of
  MintNFT metadataMerkleProof buyer -> checkMintNFT metadataMerkleProof buyer
  BurnNFT                           -> checkBurnNFT
  CreateRegistry                    -> checkRegistryCreation
  MintRegistryNFT                   -> checkRegistryNFT

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

    checkRegistryCreation :: Bool
    checkRegistryCreation = -- Registry NFT can only be minted once by checking if certain UTxO was spent
                            Api.spendsOutput txInfo (Api.txOutRefId (registryOref c)) (Api.txOutRefIdx (registryOref c)) &&
                            txMint == V.singleton ownSymbol (registryTokenName c) 1

    checkRegistryNFT :: Bool
    checkRegistryNFT =  -- Making sure asset names cannot start with relevant prefixes of NFT collection
                        all (\(_, TokenName b, _) -> let prefix = takeByteString prefixLength b in prefix /= "(100)" && prefix /= "(222)") (V.flattenValue txMint) &&
                        -- Making sure UTxO with Registry NFT is spent
                        case filter (\(TxInInfo _ out) -> V.assetClassValueOf (Api.txOutValue out) (V.AssetClass (ownSymbol, registryTokenName c)) == 1) txInputs of 
                          [_] -> True

    checkBurnNFT :: Bool
    checkBurnNFT =  
                let
                    -- Allow burning only one pair (reference NFT and user token) at once
                    [(userCs, Api.TokenName userName, userAm), (refCs, Api.TokenName refName, refAm)] = V.flattenValue txMint
                in
                    -- Matching policy id, quantities
                    -1 == userAm && -1 == refAm &&
                    ownSymbol == userCs && ownSymbol == refCs &&
                    -- Matching asset names
                    takeByteString prefixLength userName == "(222)" && takeByteString prefixLength refName == "(100)" &&
                    dropByteString prefixLength userName == dropByteString prefixLength refName


    checkMintNFT :: MT.Proof -> Buyer -> Bool
    checkMintNFT merkleProofMetadata buyer =
                            let 
                                -- Output with reference NFT
                                [(Api.OutputDatumHash (Api.DatumHash refOutDatumHash), refOutValue)] = scriptOutputsAtAddress (refAddress c) txInfo
                                [(refOutCs,Api.TokenName refOutName,refOutAm)] = V.flattenValue (V.noAdaValue refOutValue)
                                -- Mint value (reference NFT and user token) 
                                [(userCs, Api.TokenName userName, userAm), (refCs, Api.TokenName refName, refAm)] = V.flattenValue txMint
                                -- Create data for merkle tree (combination of asset names and datum hash from ref output)
                                merkleEntryMetadata = userName <> refName <> refOutName <> refOutDatumHash
                            in 
                                -- Berry holders have the privlige to buy before the 'public' mint starts. They are assigned a certain Matrix Berry which they can buy for half the price
                                (case buyer of
                                    BerryHolder berryName merkleProofAssigned   ->  -- Only privileges before public sale
                                                                                    Api.to (mintStart c) `I.contains` Api.txInfoValidRange txInfo                                                           &&
                                                                                    -- Check if holder
                                                                                    any (\(TxInInfo _ out) -> V.assetClassValueOf (Api.txOutValue out) (V.assetClass (berryCs c) berryName) == 1) txInputs  &&
                                                                                    -- Check if minting correctly assigned Matrix Berry
                                                                                    MT.member (userName <> (Api.unTokenName berryName)) (merkleRootAssigned c) merkleProofAssigned                          &&
                                                                                    -- Pay for NFT (only half the price)
                                                                                    V.assetClassValueOf (valuePaidToAddress txInfo (payeeAddress c)) (V.assetClass V.adaSymbol V.adaToken) >= paymentAmount c `divide` 2

                                    NoHolder                                    ->  -- Can only buy at start of public sale
                                                                                    Api.from (mintStart c) `I.contains` Api.txInfoValidRange txInfo  &&
                                                                                    -- Pay for NFT
                                                                                    V.assetClassValueOf (valuePaidToAddress txInfo (payeeAddress c)) (V.assetClass V.adaSymbol V.adaToken) >= paymentAmount c)
                                                                                                                                                            &&
                                -- Forcing to append datum with metadata to witness set (this will expose the metadata and not only the hash)
                                isJust (Api.findDatum (Api.DatumHash refOutDatumHash) txInfo)                                                               &&
                                -- Matching policy id, quantities
                                1 == refOutAm && 1 == userAm && 1 == refAm                                                                                  &&
                                ownSymbol == refOutCs && ownSymbol == userCs && ownSymbol == refCs                                                          &&
                                -- Check if metadata and asset names belong together and are part of the merkle tree
                                MT.member merkleEntryMetadata (merkleRootMetadata c) merkleProofMetadata                                                    &&
                                -- Ensuring uniqueness of NFT by checking if control UTxO was spent (TODO: Maybe needs to filter out ADA to only get one CS?)
                                (case filter (\(TxInInfo _ out) -> let [cs] = V.symbols (V.noAdaValue (Api.txOutValue out)) in cs == controlCs c) txInputs of [_] -> True)

-- The spending validator that holds the reference NFTs including the metadata
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
      [o] -> let (Api.OutputDatumHash h) = txOutDatum o in case Api.findDatum h txInfo of
        Just (Api.Datum d) -> case PlutusTx.fromBuiltinData d of
          Just m -> (m, txOutValue o)

    providesUserToken :: Api.CurrencySymbol -> Api.TokenName -> Integer -> Bool
    providesUserToken cs tn am = any (\(Api.TxInInfo _ out) -> valueOf (txOutValue out) cs tn == am) (txInfoInputs txInfo)

    checkBurn :: Bool
    checkBurn = 
            let
                -- Allow burning only one pair (reference NFT and user token) at once
                [(userCs, Api.TokenName userName, userAm), (refCs, Api.TokenName refName, refAm)] = V.flattenValue txMint
                [(ownCs, Api.TokenName ownName, ownAm)] = V.flattenValue (V.noAdaValue ownValue)
            in
                -- Matching policy id, quantities
                -1 == userAm && -1 == refAm &&
                ownCs == userCs && ownCs == refCs && 
                -- User token belonging to reference NFT is provided
                providesUserToken ownCs (Api.TokenName userName) 1 &&
                -- Matching asset names
                takeByteString prefixLength userName == "(222)" && takeByteString prefixLength refName == "(100)" &&
                dropByteString prefixLength userName == dropByteString prefixLength refName

    checkUpdateName :: Bool
    checkUpdateName = 
                    let
                        [(ownCs, Api.TokenName ownName, ownAm)] = V.flattenValue (V.noAdaValue ownValue)
                    in
                        -- Value matches
                        ownValue == ownOutputValue &&
                        -- Metadata stays immutable
                        metadata datumMetadata == metadata ownOutputDatumMetadata && version datumMetadata == version ownOutputDatumMetadata &&
                        -- User token belonging to reference NFT is provided
                        providesUserToken ownCs (Api.TokenName ("(222)" <> dropByteString prefixLength ownName)) 1
                        -- Limit the size of identity
                        --lengthOfByteString (Builtins.serialiseData (PlutusTx.toBuiltinData (identity ownOutputDatumMetadata))) <= 2000


-- A one shot policy to authenticate the control UTxOs with control NFTs
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


-- The spending validator that checks for uniqueness of NFTs
{-# INLINEABLE spendValidatorControl #-}
spendValidatorControl :: Api.PubKeyHash -> Api.CurrencySymbol -> BuiltinByteString -> Integer -> ControlAction -> Api.ScriptContext -> Bool
spendValidatorControl owner userCs userPrefix isMinted action ctx = case action of
    Mint    -> checkIsMintable
    Destroy -> checkDestroy
    where
        txInfo :: Api.TxInfo
        txInfo = Api.scriptContextTxInfo ctx

        txMint :: V.Value
        txMint = Api.txInfoMint txInfo

        prefixLength :: Integer
        prefixLength = 5

        ownValue :: V.Value
        ownValue =  let Just i = Api.findOwnInput ctx
                        out = txInInfoResolved i
                    in txOutValue out

        ownOutputDatum :: Integer
        ownOutputValue :: V.Value
        (ownOutputDatum, ownOutputValue) = case getContinuingOutputs ctx of
            [o] -> let (Api.OutputDatumHash h) = txOutDatum o in case Api.findDatum h txInfo of
                Just (Api.Datum d) -> case PlutusTx.fromBuiltinData d of
                    Just m -> (m, txOutValue o)

        checkIsMintable :: Bool
        checkIsMintable = 
                        let 
                            [(mintUserCs, Api.TokenName mintUserTn, _), _] = flattenValue txMint
                            [(controlCs, Api.TokenName controlTn, _)] = flattenValue (V.noAdaValue ownValue)
                        in
                            if isMinted == 0 
                                then ownValue == ownOutputValue && mintUserCs == userCs && mintUserTn == userPrefix <> controlTn && ownOutputDatum == 1
                                else False

        checkDestroy :: Bool
        checkDestroy = txInfo `txSignedBy` owner && isMinted == 1 && null (flattenValue txMint)

-- Utils

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

-- Instantiate validators

mintInstanceMain :: Scripts.MintingPolicy
mintInstanceMain =
  Api.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||]) `PlutusTx.applyCode` PlutusTx.liftCode mainContractDetails
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
    $$(PlutusTx.compile [||wrap||]) `PlutusTx.applyCode` PlutusTx.liftCode controlOwner `PlutusTx.applyCode` PlutusTx.liftCode mintSymbolMain `PlutusTx.applyCode` PlutusTx.liftCode userNamePrefix
  where
    wrap owner cs pref = Scripts.mkUntypedValidator $ spendValidatorControl owner cs pref

spendAddrControl :: Api.Address
spendAddrControl = Scripts.mkValidatorAddress spendInstanceControl


-- Serialization

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

-- Lift

-- This is handy to make use of direct hash string literals
deriving via Api.LedgerBytes instance IsString MT.Hash

PlutusTx.makeLift ''RefAction
PlutusTx.makeIsDataIndexed ''RefAction [('Burn, 0), ('UpdateName, 1)]
PlutusTx.makeLift ''Buyer
PlutusTx.makeIsDataIndexed ''Buyer [('BerryHolder, 0), ('NoHolder, 1)]
PlutusTx.makeLift ''ControlAction
PlutusTx.makeIsDataIndexed ''ControlAction [('Mint, 0), ('Destroy, 1)]
PlutusTx.makeLift ''DatumMetadata
PlutusTx.makeIsDataIndexed ''DatumMetadata [('DatumMetadata, 0)]
PlutusTx.makeLift ''MainContractDetails
PlutusTx.makeIsDataIndexed ''MainContractDetails [('MainContractDetails, 0)]
PlutusTx.makeLift ''MainContractAction
PlutusTx.makeIsDataIndexed ''MainContractAction [('MintNFT, 0), ('BurnNFT, 1), ('CreateRegistry, 2), ('MintRegistryNFT, 3)]
PlutusTx.makeLift ''MT.Hash 