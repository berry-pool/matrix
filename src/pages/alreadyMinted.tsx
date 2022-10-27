import * as React from "react";
import metadata from "../data/berryAssigned.json";
import { projectId } from "../../secrets";
import { fromUnit, hexToUtf8 } from "lucid-cardano";

export const getAllMintedIds = async (): Promise<number[]> => {
  const matrixAssets = await fetch(
    `https://cardano-mainnet.blockfrost.io/api/v0/assets/policy/${"01cecfaeda9d846c08675902b55a6371f593d9239744867462c5382e"}`,
    { headers: { project_id: projectId } }
  ).then((r) => r.json());
  if (!matrixAssets || matrixAssets.error) return [];

  return matrixAssets
    .filter((asset: any) => fromUnit(asset.asset).label === 100)
    .map((asset: any) =>
      parseInt(hexToUtf8(fromUnit(asset.asset).name!).slice(6))
    );
};

const AlreadyMinted = () => {
  const [minted, setMinted] = React.useState<string[]>([]);

  const getMinted = async () => {
    const allIds = await getAllMintedIds();
    const berries = allIds.map(
      (id) => metadata.find((m) => m.id === id)!.berry
    );
    setMinted(berries);
  };

  React.useEffect(() => {
    getMinted();
  }, []);

  return (
    <div>
      {minted.map((m) => (
        <div>{m}</div>
      ))}
    </div>
  );
};

export default AlreadyMinted;
