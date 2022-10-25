import {
  Address,
  applyParamsToScript,
  Assets,
  Blockfrost,
  concat,
  Constr,
  Data,
  fromHex,
  fromUnit,
  hexToUtf8,
  Json,
  Lucid,
  MerkleTree,
  MintingPolicy,
  PlutusData,
  PolicyId,
  SpendingValidator,
  toLabel,
  toUnit,
  TxHash,
  utf8ToHex,
  UTxO,
} from "lucid-cardano";
import scripts from "./ghc/scripts.json";
import assigned from "../data/berryAssigned.json";
import metadata from "../data/metadata.json";
import { contractDetails } from "./config";
// create secrets.ts file at the root of the project with the constant projectId
import { projectId } from "../../secrets";

const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preview.blockfrost.io/api/v0",
    projectId,
  ),
  "Preview",
);

// -- Utils ------------------------------------------------------------------

function keyAddressToData(address: Address): PlutusData {
  const { paymentCredential } = lucid.utils
    .getAddressDetails(address);
  return new Constr(0, [
    new Constr(0, [paymentCredential?.hash!]),
    new Constr(1, []),
  ]);
}

function keyAddressWithKeyStakeToData(address: Address): PlutusData {
  const { paymentCredential, stakeCredential } = lucid.utils
    .getAddressDetails(address);
  return new Constr(0, [
    new Constr(0, [paymentCredential?.hash!]),
    new Constr(0, [new Constr(0, [new Constr(0, [stakeCredential?.hash!])])]),
  ]);
}

function scriptAddressToData(address: Address): PlutusData {
  const { paymentCredential } = lucid.utils
    .getAddressDetails(address);
  return new Constr(0, [
    new Constr(1, [paymentCredential?.hash!]),
    new Constr(1, []),
  ]);
}

// -- Merkle trees ------------------------------------------------------------------

const metadataData = metadata.map((m, i) =>
  concat(
    fromHex(toLabel(222) + utf8ToHex(`Matrix${i}`)),
    fromHex(toLabel(100) + utf8ToHex(`Matrix${i}`)),
    fromHex(toLabel(100) + utf8ToHex(`Matrix${i}`)),
    fromHex(
      lucid.utils.datumToHash(
        Data.to(new Constr(0, [Data.fromJson(m), 1n])),
      ),
    ), // metadata
  )
);

const assignedData = assigned.map((a) =>
  concat(fromHex(a.matrix), new TextEncoder().encode(a.berry))
);

const merkleTreeMetadata = new MerkleTree(metadataData);
const merkleTreeAssigned = new MerkleTree(assignedData);

// -- Instantiate validators ------------------------------------------------------------------

const spendReference: SpendingValidator = {
  type: "PlutusV2",
  script: applyParamsToScript(
    scripts.spendReference,
    new Constr(0, [toLabel(100), toLabel(222)]),
  ),
};
const referenceAddress: Address = lucid.utils.validatorToAddress(
  spendReference,
);

const mintControl: MintingPolicy = {
  type: "PlutusV2",
  script: applyParamsToScript(
    scripts.mintControl,
    new Constr(0, [
      new Constr(0, [
        contractDetails.controlOref.txHash,
      ]),
      BigInt(contractDetails.controlOref.outputIndex),
    ]),
  ),
};
const controlPolicyId: PolicyId = lucid.utils.mintingPolicyToId(mintControl);

const mintMain: MintingPolicy = {
  type: "PlutusV2",
  script: applyParamsToScript(
    scripts.mintMain,
    new Constr(0, [
      toLabel(100),
      toLabel(222),
      toLabel(500) + utf8ToHex("Royalty"),
    ]),
    new Constr(0, [
      controlPolicyId,
      contractDetails.berryPolicyId,
      new Constr(0, [merkleTreeMetadata.rootHash()]),
      new Constr(0, [merkleTreeAssigned.rootHash()]),
      scriptAddressToData(referenceAddress),
      keyAddressWithKeyStakeToData(contractDetails.payeeAddress),
      contractDetails.paymentAmount,
      BigInt(contractDetails.mintStart),
      new Constr(0, [
        new Constr(0, [
          contractDetails.royaltyOref.txHash,
        ]),
        BigInt(contractDetails.royaltyOref.outputIndex),
      ]),
    ]),
  ),
};
const mainPolicyId: PolicyId = lucid.utils.mintingPolicyToId(mintMain);

const spendControl: SpendingValidator = {
  type: "PlutusV2",
  script: applyParamsToScript(
    scripts.spendControl,
    contractDetails.controlOwner,
    mainPolicyId,
  ),
};
const controlAddress: Address = lucid.utils.validatorToAddress(spendControl);

// -- Endpoints ------------------------------------------------------------------

export const getAllMintedIds = async (): Promise<number[]> => {
  const matrixAssets = await fetch(
    `https://cardano-preview.blockfrost.io/api/v0/assets/policy/${mainPolicyId}`,
    { headers: { project_id: projectId } },
  ).then((r) => r.json());
  if (!matrixAssets || matrixAssets.error) return [];

  return matrixAssets.filter((asset: any) =>
    fromUnit(asset.asset).label === 100
  ).map((asset: any) =>
    parseInt(hexToUtf8(fromUnit(asset.asset).name!).slice(6))
  );
};

export const getMetadata = async (matrixId: number): Promise<Json> => {
  const [refNFTUtxo] = await lucid.utxosAtWithUnit(
    referenceAddress,
    toUnit(mainPolicyId, utf8ToHex(`Matrix${matrixId}`), 100),
  );

  if (!refNFTUtxo) return {};

  const metadataDatum = Data.from(await lucid.datumOf(refNFTUtxo)) as Constr<
    PlutusData
  >;

  const metadata: {
    name: string;
    image: string;
    id: number;
    description: string;
  } = Data
    .toJson(metadataDatum.fields[0]);
  return metadata;
};

export const getRandomAvailable = async (): Promise<[number, UTxO | null]> => {
  lucid.selectWallet(window.walletApi);
  const walletUtxos = await lucid.wallet.getUtxos();

  const berryNames = walletUtxos.reduce(
    (acc, utxo) =>
      acc.concat(
        Object.keys(utxo.assets).filter((asset) =>
          asset.startsWith(contractDetails.berryPolicyId)
        ),
      ),
    [] as Array<string>,
  ).map((unit) => hexToUtf8(unit.slice(56)));

  const matrixAssets = await fetch(
    `https://cardano-preview.blockfrost.io/api/v0/assets/policy/${mainPolicyId}`,
    { headers: { project_id: projectId } },
  ).then((r) => r.json());

  const availableIds: Array<number> = (() => {
    if (!matrixAssets || matrixAssets.error) {
      return [...Array(100)].map((_, i) => i);
    } else {
      const matrixIds: Array<number> = matrixAssets.filter((asset: any) =>
        fromUnit(asset.asset).label === 100
      ).map((asset: any) =>
        parseInt(hexToUtf8(fromUnit(asset.asset).name!).slice(6))
      );
      return [...Array(100)].map((_, i) => i).filter((id) =>
        !matrixIds.includes(id)
      );
    }
  })();

  if (Date.now() < contractDetails.mintStart) {
    const availableAssigned = assigned.filter((a) =>
      berryNames.includes(a.berry) && availableIds.includes(a.id)
    );
    if (availableAssigned.length > 0) {
      const chosen = availableAssigned[0];
      const berryUtxo = walletUtxos.find((utxo) =>
        Object.keys(utxo.assets).some((asset) =>
          asset.endsWith(utf8ToHex(chosen.berry))
        )
      );
      return [chosen.id, berryUtxo!];
    }
  }

  const randomId =
    availableIds[Math.floor(Math.random() * availableIds.length)];

  return [randomId, null];
};

export const deploy = async (): Promise<TxHash> => {
  lucid.selectWallet(window.walletApi);

  const walletUtxos = await lucid.wallet.getUtxos();

  const controlUtxo = walletUtxos.find((utxo) =>
    utxo.txHash === contractDetails.controlOref.txHash &&
    utxo.outputIndex === contractDetails.controlOref.outputIndex
  );

  if (!controlUtxo) throw new Error("NoUTxOError");

  const tx = lucid.newTx();
  for (let i = 0; i < 100; i++) {
    tx.mintAssets(
      { [toUnit(controlPolicyId, utf8ToHex(`${i}`))]: 1n },
      Data.to(new Constr(0, [])),
    );
    tx.payToContract(controlAddress, {
      inline: Data.to(new Constr(1, [0n])),
    }, {
      [toUnit(controlPolicyId, utf8ToHex(`${i}`))]: 1n,
    });
  }
  tx.collectFrom([controlUtxo])
    .attachSpendingValidator(mintControl);

  const finalTx = await tx.complete();
  const signedTx = await finalTx.sign().complete();
  const txHash = await signedTx.submit();
  console.log("Created control UTxOs", txHash);
  console.log("Awaiting confirmation...");
  await lucid.awaitTx(txHash);

  const tx2 = await lucid.newTx().payToContract(
    controlAddress,
    { inline: Data.to(new Constr(0, [])), scriptRef: spendControl },
    {},
  )
    .payToContract(
      controlAddress,
      { inline: Data.to(new Constr(0, [])), scriptRef: mintMain },
      {},
    ).complete();
  const signedTx2 = await tx2.sign().complete();
  return signedTx2.submit();
};

export const mint = async (
  matrixId: number,
  berryUtxo?: UTxO | null,
): Promise<TxHash> => {
  lucid.selectWallet(window.walletApi);

  const m = Data.fromJson(metadata[matrixId]);
  const d = metadataData[matrixId];
  const proof = merkleTreeMetadata.getProof(d);

  const hasBerry: boolean = !!berryUtxo;

  // only relevant if buyer actually holds a Berry and buys during the right time frame
  const berryD = assignedData[matrixId];
  const berryName = new TextEncoder().encode(assigned[matrixId].berry);
  const berryProof = merkleTreeAssigned.getProof(berryD);

  const mintRedeemer = Data.to(
    new Constr(0, [
      proof.map((p) =>
        p.left
          ? new Constr(0, [new Constr(0, [p.left])])
          : new Constr(1, [new Constr(0, [p.right!])])
      ),
      hasBerry
        ? new Constr(0, [
          berryName,
          berryProof.map((p) =>
            p.left
              ? new Constr(0, [new Constr(0, [p.left])])
              : new Constr(1, [new Constr(0, [p.right!])])
          ),
        ])
        : new Constr(1, []),
    ]),
  );

  const controlRedeemer = Data.to(new Constr(0, []));

  const [controlUtxo] = await lucid.utxosAtWithUnit(
    controlAddress,
    toUnit(controlPolicyId, utf8ToHex(`${matrixId}`)),
  );

  if (!controlUtxo) throw new Error("NoUTxOError");

  const isMinted = Data.from(await lucid.datumOf(controlUtxo)) as bigint;
  if (isMinted === 1n) throw new Error("TokenExistsError");

  // Tx hash taken from the inital deploy tx
  const referenceScripts = await lucid.utxosByOutRef([{
    txHash: contractDetails.referenceScriptsTxHash,
    outputIndex: 0,
  }, {
    txHash: contractDetails.referenceScriptsTxHash,
    outputIndex: 1,
  }]);

  const tx = await lucid.newTx()
    .collectFrom([controlUtxo], controlRedeemer)
    .applyIf(hasBerry, (tx) => {
      tx.collectFrom([berryUtxo!]);
    })
    .validFrom(Date.now() - contractDetails.txValidFromThreshold)
    .validTo(Date.now() + contractDetails.txValidToThreshold)
    .mintAssets({
      [toUnit(mainPolicyId, utf8ToHex(`Matrix${matrixId}`), 100)]: 1n,
      [toUnit(mainPolicyId, utf8ToHex(`Matrix${matrixId}`), 222)]: 1n,
    }, mintRedeemer)
    .payToContract(referenceAddress, Data.to(new Constr(0, [m, 1n])), {
      [toUnit(mainPolicyId, utf8ToHex(`Matrix${matrixId}`), 100)]: 1n,
    })
    .payToContract(
      controlAddress,
      { inline: Data.to(new Constr(1, [1n])) },
      controlUtxo.assets,
    )
    .payToAddress(contractDetails.payeeAddress, {
      lovelace: hasBerry
        ? contractDetails.paymentAmount / 2n
        : contractDetails.paymentAmount,
    })
    .readFrom(referenceScripts)
    .complete();

  const signedTx = await tx.sign().complete();

  return signedTx.submit();
};

export const burn = async (matrixId: number): Promise<TxHash> => {
  lucid.selectWallet(window.walletApi);

  const [refNFTUtxo] = await lucid.utxosAtWithUnit(
    referenceAddress,
    toUnit(mainPolicyId, utf8ToHex(`Matrix${matrixId}`), 100),
  );

  if (!refNFTUtxo) throw new Error("NoUTxOError");

  const refRedeemer = Data.to(new Constr(0, []));
  const burnRedeemer = Data.to(new Constr(1, []));

  // Tx hash taken from the inital deploy tx
  const referenceScripts = await lucid.utxosByOutRef([{
    txHash: contractDetails.referenceScriptsTxHash,
    outputIndex: 1,
  }]);

  const tx = await lucid.newTx()
    .collectFrom([refNFTUtxo], refRedeemer)
    .mintAssets({
      [toUnit(mainPolicyId, utf8ToHex(`Matrix${matrixId}`), 100)]: -1n,
      [toUnit(mainPolicyId, utf8ToHex(`Matrix${matrixId}`), 222)]: -1n,
    }, burnRedeemer)
    .readFrom(referenceScripts)
    .attachSpendingValidator(spendReference)
    .complete();

  const signedTx = await tx.sign().complete();
  return signedTx.submit();
};

export const redeemControl = async (): Promise<TxHash> => {
  lucid.selectWallet(window.walletApi);

  const controlUtxos = (await lucid.utxosAt(
    controlAddress,
  )).filter((utxo) =>
    Object.keys(utxo.assets).some((asset) =>
      asset.startsWith(
        controlPolicyId,
      )
    )
  ).slice(0, 10);

  // Tx hash taken from the inital deploy tx
  const referenceScripts = await lucid.utxosByOutRef([{
    txHash: contractDetails.referenceScriptsTxHash,
    outputIndex: 0,
  }, {
    txHash: contractDetails.referenceScriptsTxHash,
    outputIndex: 1,
  }]);

  const controlRedeemer = Data.to(new Constr(1, []));

  const controlAssets: Assets = controlUtxos.reduce(
    (acc, utxo) => ({
      ...acc,
      [Object.keys(utxo.assets).find((asset) => asset !== "lovelace")!]: -1n,
    }),
    {},
  );

  const tx = await lucid.newTx()
    .collectFrom(controlUtxos, controlRedeemer)
    .applyIf(referenceScripts.length > 0, (tx) => {
      tx.collectFrom(referenceScripts, Data.empty());
    })
    .applyIf(referenceScripts.length == 0, (tx) => {
      tx.attachSpendingValidator(spendControl);
    })
    .mintAssets(controlAssets, controlRedeemer)
    .addSigner(
      await lucid.wallet.address(),
    )
    .attachMintingPolicy(mintControl)
    .complete();

  const signedTx = await tx.sign().complete();
  return signedTx.submit();
};

export const mintRoyalty = async (): Promise<TxHash> => {
  lucid.selectWallet(window.walletApi);

  const walletUtxos = await lucid.wallet.getUtxos();

  const royaltyUtxo = walletUtxos.find((utxo) =>
    utxo.txHash === contractDetails.royaltyOref.txHash &&
    utxo.outputIndex === contractDetails.royaltyOref.outputIndex
  );

  if (!royaltyUtxo) throw new Error("NoUTxOError");

  const tx = await lucid.newTx().collectFrom([royaltyUtxo]).mintAssets({
    [toUnit(mainPolicyId, utf8ToHex(`Royalty`), 500)]: 1n,
  }, Data.to(new Constr(2, []))).attachMintingPolicy(mintMain).complete();

  const signedTx = await tx.sign().complete();

  return signedTx.submit();
};

export const updateDescription = async (
  matrixId: number,
  description: string,
): Promise<TxHash> => {
  lucid.selectWallet(window.walletApi);

  const [refNFTUtxo] = await lucid.utxosAtWithUnit(
    referenceAddress,
    toUnit(mainPolicyId, utf8ToHex(`Matrix${matrixId}`), 100),
  );

  const userTokenUtxo = (await lucid.wallet.getUtxos()).find((utxo) =>
    !!utxo.assets[toUnit(mainPolicyId, utf8ToHex(`Matrix${matrixId}`), 222)]
  );

  if (!refNFTUtxo) throw new Error("NoUTxOError");
  if (!userTokenUtxo) throw new Error("NoOwnershipUTxOError");

  const metadataDatum = Data.from(await lucid.datumOf(refNFTUtxo)) as Constr<
    PlutusData
  >;

  const metadata: {
    name: string;
    image: string;
    id: number;
    description: string;
  } = Data
    .toJson(metadataDatum.fields[0]);

  metadata.description = description;

  metadataDatum.fields[0] = Data.fromJson(metadata);

  const tx = await lucid.newTx().collectFrom(
    [refNFTUtxo],
    Data.to(new Constr(1, [])),
  ).collectFrom([userTokenUtxo]).payToContract(
    referenceAddress,
    Data.to(metadataDatum),
    refNFTUtxo.assets,
  )
    .attachSpendingValidator(spendReference)
    .complete();
  const signedTx = await tx.sign().complete();
  return signedTx.submit();
};

export const awaitTx = (txHash: TxHash) => lucid.awaitTx(txHash);
