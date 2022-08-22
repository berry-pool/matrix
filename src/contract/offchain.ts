import {
  Address,
  Assets,
  Blockfrost,
  C,
  concat,
  Construct,
  Data,
  fromHex,
  Lucid,
  MerkleTree,
  MintingPolicy,
  PolicyId,
  SpendingValidator,
  toHex,
  TxHash,
  UTxO,
} from "lucid-cardano";
import { decode } from "../utils/utils";
import scripts from "./ghc/scripts.json";
import assigned from "../data/berryAssigned.json";
import metadata from "../data/metadata.json";
// create secrets.ts file at the root of the project with the constant projectId
import { projectId } from "../../secrets";

const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-testnet.blockfrost.io/api/v0",
    projectId,
  ),
  "Testnet",
);

const contractDetails = {
  registryOref: {
    txHash: "b26a00bc3e795092f08ffeda4e8d74ae64a9f0690c7edbdc049f7beed4f54046",
    outputIndex: 2,
  },
  controlOref: {
    txHash: "f84ff26849f5682165d6967e5b26925fa6035110c22f9a0c886872515735054a",
    outputIndex: 100,
  },
  registryTokenName: "(110)Registry",
  berryPolicyId: "15be994a64bdb79dde7fe080d8e7ff81b33a9e4860e9ee0d857a8e85",
  merkleRootMetadata:
    "e44be91d0892234fc46b48878455b34ff9e1bd7d682517be80c4f87ddc7303ae",
  merkleRootAssigned:
    "71be0c95e03f2bb5274c62ff3acb3c92c264f8aa0732dbfecc05657854563e49",
  payeeAddress:
    "addr_test1vzmvs72wnfazvkv5gzjdpltee5rkgng4j9llzd5578m8ydgkp6edr",
  paymentAmount: 10000000n,
  mintStart: 0,
};

const mintMain: MintingPolicy = {
  type: "PlutusV1",
  script: scripts.mintMain,
};

const spendReference: SpendingValidator = {
  type: "PlutusV1",
  script: scripts.spendReference,
};

const spendControl: SpendingValidator = {
  type: "PlutusV1",
  script: scripts.spendControl,
};

const mintControl: MintingPolicy = {
  type: "PlutusV1",
  script: scripts.mintControl,
};

const mainPolicyId: PolicyId = lucid.utils.mintingPolicyToId(mintMain);
const referenceAddress: Address = lucid.utils.validatorToAddress(
  spendReference,
);
const controlAddress: Address = lucid.utils.validatorToAddress(spendControl);
const controlPolicyId: PolicyId = lucid.utils.mintingPolicyToId(mintControl);

//----- Create Merkle trees

const metadataData = metadata.map((m, i) =>
  concat(
    new TextEncoder().encode(`(222)Matrix${i}`),
    new TextEncoder().encode(`(100)Matrix${i}`),
    new TextEncoder().encode(`(100)Matrix${i}`),
    fromHex(
      lucid.utils.datumToHash(
        Data.to(new Construct(0, [Data.fromJson(m), 1n])),
      ),
    ), // metadata
  )
);

const assignedData = assigned.map((a) =>
  concat(new TextEncoder().encode(a.matrix), new TextEncoder().encode(a.berry))
);

const merkleTreeMetadata = await MerkleTree.new(metadataData);
const merkleTreeAssigned = await MerkleTree.new(assignedData);

//----- Create fake policy which imitates Berries (will be removed in production)

// const { paymentCredential } = lucid.utils.getAddressDetails(
//   await lucid.wallet.address(),
// );

// const fakeOldPolicy: MintingPolicy = {
//   type: "Native",
//   script: toHex(
//     C.NativeScript.new_script_pubkey(
//       C.ScriptPubkey.new(C.Ed25519KeyHash.from_hex(paymentCredential?.hash!)),
//     ).to_bytes(),
//   ),
// };

// const fakeOldPolicyId = lucid.utils.validatorToScriptHash(fakeOldPolicy);

// export const fakeMint = async (budId: number): Promise<TxHash> => {
//   const tx = await lucid.newTx()
//     .mintAssets({ [fakeOldPolicyId + name(`SpaceBud${budId}`)]: 10n })
//     .attachMintingPolicy(fakeOldPolicy)
//     .complete();

//   const signedTx = await tx.sign().complete();
//   return signedTx.submit();
// };

//-------

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
  ).map((unit) => fromName(unit.slice(56)));

  const matrixAssets = await fetch(
    `https://cardano-testnet.blockfrost.io/api/v0/assets/policy/${mainPolicyId}`,
    { headers: { project_id: projectId } },
  ).then((r) => r.json());

  const availableIds: Array<number> = (() => {
    if (!matrixAssets || matrixAssets.error) {
      return [...Array(100)].map((_, i) => i);
    } else {
      console.log(matrixAssets);
      const matrixIds: Array<number> = matrixAssets.filter((asset: any) =>
        asset.asset.slice(56).startsWith(name("(100)"))
      ).map((asset: any) =>
        parseInt(fromName(asset.asset.slice(56)).slice(11))
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
          asset.endsWith(name(chosen.berry))
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
      { [controlPolicyId + name(`${i}`)]: 1n },
      Data.to(new Construct(0, [])),
    );
    tx.payToContract(controlAddress, Data.to(0n), {
      [controlPolicyId + name(`${i}`)]: 1n,
    });
  }
  tx.collectFrom([controlUtxo]).attachSpendingValidator(mintControl);

  const finalTx = await tx.complete();
  const signedTx = await finalTx.sign().complete();
  return signedTx.submit();
};

export const mint = async (
  matrixId: number,
  berryUtxo?: UTxO | null,
): Promise<TxHash> => {
  lucid.selectWallet(window.walletApi);

  const m = Data.fromJson(metadata[matrixId]);
  const d = metadataData[matrixId];
  const proof = await merkleTreeMetadata.getProof(d);

  const hasBerry: boolean = !!berryUtxo;

  // only relevant if buyer actually holds a Berry and buys during the right time frame
  const berryD = assignedData[matrixId];
  const berryName = new TextEncoder().encode(assigned[matrixId].berry);
  const berryProof = await merkleTreeAssigned.getProof(berryD);

  const mintRedeemer = Data.to(
    new Construct(0, [
      proof.map((p) =>
        p.left
          ? new Construct(0, [new Construct(0, [p.left])])
          : new Construct(1, [new Construct(0, [p.right!])])
      ),
      hasBerry
        ? new Construct(0, [
          new Construct(0, [berryName]),
          berryProof.map((p) =>
            p.left
              ? new Construct(0, [new Construct(0, [p.left])])
              : new Construct(1, [new Construct(0, [p.right!])])
          ),
        ])
        : new Construct(1, []),
    ]),
  );

  const controlRedeemer = Data.to(new Construct(0, []));

  const [controlUtxo] = await lucid.utxosAtWithUnit(
    controlAddress,
    controlPolicyId + name(`${matrixId}`),
  );

  if (!controlUtxo) throw new Error("NoUTxOError");

  const isMinted = Data.from(await lucid.datumOf(controlUtxo)) as bigint;
  if (isMinted === 1n) throw new Error("TokenExistsError");

  const tx = await lucid.newTx()
    .collectFrom([controlUtxo], controlRedeemer)
    .applyIf(hasBerry, (tx) => {
      tx.collectFrom([berryUtxo!]);
    })
    .validFrom(Date.now() - 100000)
    .validTo(Date.now() + 1200000)
    .mintAssets({
      [mainPolicyId + name(`(100)Matrix${matrixId}`)]: 1n,
      [mainPolicyId + name(`(222)Matrix${matrixId}`)]: 1n,
    }, mintRedeemer)
    .payToContract(referenceAddress, Data.to(new Construct(0, [m, 1n])), {
      [mainPolicyId + name(`(100)Matrix${matrixId}`)]: 1n,
    })
    .payToContract(controlAddress, Data.to(1n), controlUtxo.assets)
    .payToAddress(contractDetails.payeeAddress, {
      lovelace: contractDetails.paymentAmount,
    })
    .attachSpendingValidator(spendControl)
    .attachMintingPolicy(mintMain)
    .complete();

  const signedTx = await tx.sign().complete();
  return signedTx.submit();
};

export const burn = async (matrixId: number): Promise<TxHash> => {
  lucid.selectWallet(window.walletApi);

  const [refNFTUtxo] = await lucid.utxosAtWithUnit(
    referenceAddress,
    mainPolicyId + name(`(100)Matrix${matrixId}`),
  );

  if (!refNFTUtxo) throw new Error("NoUTxOError");

  const burnRedeemer = Data.to(new Construct(1, []));

  const tx = await lucid.newTx()
    .collectFrom([refNFTUtxo], burnRedeemer)
    .mintAssets({
      [mainPolicyId + name(`(100)Matrix${matrixId}`)]: -1n,
      [mainPolicyId + name(`(222)Matrix${matrixId}`)]: -1n,
    }, burnRedeemer)
    .attachMintingPolicy(mintMain)
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
    Object.keys(utxo.assets).some((asset) => asset.startsWith(controlPolicyId))
  ).slice(0, 10);

  const controlRedeemer = Data.to(new Construct(1, []));

  const controlAssets: Assets = controlUtxos.reduce(
    (acc, utxo) => ({
      ...acc,
      [Object.keys(utxo.assets).find((asset) => asset !== "lovelace")!]: -1n,
    }),
    {},
  );

  const tx = await lucid.newTx().collectFrom(controlUtxos, controlRedeemer)
    .mintAssets(controlAssets, controlRedeemer).addSigner(
      await lucid.wallet.address(),
    ).complete();

  const signedTx = await tx.sign().complete();
  return signedTx.submit();
};

export const awaitTx = (txHash: TxHash) => lucid.awaitTx(txHash);

// helper function
const name = (utf8: string): string => toHex(new TextEncoder().encode(utf8));
const fromName = (hex: string): string =>
  new TextDecoder().decode(decode(new TextEncoder().encode(hex)));
const label = (l: number) => "0" + l.toString(16).padStart(4, "0") + "0";
