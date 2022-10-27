import * as React from "react";
import { getAllMintedIds } from "../contract/offchain";
import metadata from "../data/berryAssigned.json";

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
