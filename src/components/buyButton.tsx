import * as React from "react";
import { Spinner } from "./spinner";
import metadata from "../data/metadata.json";
import * as Contract from "../contract/offchain";
//@ts-ignore
import ParticleEffectButton from "react-particle-effect-button";

export const BerryBuyButton = ({
  setStart,
  setConfirmed,
  title,
}: {
  setStart: React.Dispatch<React.SetStateAction<boolean>>;
  setConfirmed: React.Dispatch<
    React.SetStateAction<{ state: boolean; id: number }>
  >;
  title: string;
}) => {
  const [loading, setLoading] = React.useState<boolean>(false);
  const [hidden, setHidden] = React.useState<boolean>(false);
  const [error, setError] = React.useState<string>("");

  const isJson = (str: string) => {
    try {
      JSON.parse(str);
    } catch (e) {
      return false;
    }
    return true;
  };

  const preloadImage = (src: string) =>
    new Promise((r) => {
      const image = new Image();
      image.onload = r;
      image.onerror = r;
      image.src = src;
    });

  const errorMap: Record<string, string> = {
    InputsExhaustedError:
      "Insufficent balance. Select another wallet/account or fund your current wallet.",
    "Not enough ADA leftover to cover minADA":
      "Insufficent balance. Select another wallet/account or fund your current wallet.",
    "Not enough ADA leftover to cover fees":
      "Insufficent balance. Select another wallet/account or fund your current wallet.",
    "Missing input or output for some native asset":
      "Something went wrong. Try again.",
    MaxTxSizeError:
      "Transaction too large. Try to consolidate your wallet or shrink it by transferring tokens to another wallet/account.",
    ExUnitsBudgetError:
      "Transaction too large, maximum costs exceeded. Try to consolidate your wallet or shrink it by transferring tokens to another wallet/account.",
  };

  return (
    <div className="relative w-full flex justify-center items-center">
      <ParticleEffectButton
        color="white"
        hidden={hidden}
        duration={500}
        direction="left"
        particlesAmountCoefficient={1}
        oscillationCoefficient={10}
        style="stroke"
      >
        <button
          className="py-4 px-8 rounded-2xl text-white font-medium border-2 border-r-purple-500 border-t-purple-600 border-l-green-500 border-b-green-600"
          onClick={async (e) => {
            setLoading(true);
            setError("");
            try {
              // const image = metadata[randomBerry].image;
              // await preloadImage(image); // TODO

              const [id, berryUtxo] = await Contract.getRandomAvailable();

              const txHash = await Contract.mint(id, berryUtxo);

              setHidden(true);
              setTimeout(() => setStart(true), 1000);
              await Contract.awaitTx(txHash);
              setConfirmed({ state: true, id });
            } catch (e) {
              console.log(e);
              if (!(e as any).code) {
                let err = e as string;
                if ((err as any).message) err = (err as any).message;
                if (isJson(err)) {
                  err = "ExUnitsBudgetError";
                } else if (err.startsWith("Maximum transaction size"))
                  err = "MaxTxSizeError";
                setError(errorMap[err] || "Something went wrong. Try again.");
              }
            }
            setLoading(false);
          }}
        >
          <div className="flex flex-row items-center justify-center w-full h-full">
            <div>{title}</div>
            {loading && (
              <div className="ml-4">
                <Spinner />
              </div>
            )}
          </div>
        </button>
      </ParticleEffectButton>
      {error && (
        <div className="absolute bottom-[-70px] text-red-400 w-[400px] h-[70px] max-w-[90%] text-center">
          {error}
        </div>
      )}
    </div>
  );
};
