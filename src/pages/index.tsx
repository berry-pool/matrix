import * as React from "react";
import type { HeadFC } from "gatsby";
import BerryLogo from "../images/berry.svg";
import DiscordLogo from "../images/github.png";
import MatrixTitle from "../images/matrixTitle.svg";
import Berry16 from "../images/berry16.jpeg";
import { WalletSelector } from "../components/walletSelector";
import { BerryBuyButton } from "../components/buyButton";
// Animation needs to be dynamically imported as it makes use of window quite a few times and so messes up SSR
const { BerryAnimation } =
  typeof window !== "undefined"
    ? await import("../components/animation")
    : { BerryAnimation: null };

const IndexPage = () => {
  const [connected, setConnected] = React.useState<boolean>(false);
  const [start, setStart] = React.useState<boolean>(false);
  const [confirmed, setConfirmed] = React.useState<{
    state: boolean;
    id: number;
  }>({ state: false, id: -1 });
  const [final, setFinal] = React.useState<boolean>(false);
  const [isOpenWalletSelector, setIsOpenWalletSelector] = React.useState(false);
  const [network, setNetwork] = React.useState<number>(1);

  return (
    <>
      <div className="w-screen h-screen bg-purple-800 relative overflow-hidden">
        <div className="absolute top-0 left-0 w-full h-full">
          {start && (
            //@ts-ignore
            <BerryAnimation
              confirmed={confirmed.state}
              onFinal={() => setFinal(true)}
            />
          )}
        </div>
        <div className="absolute top-0 left-0 w-full h-full flex items-center justify-center flex-col">
          <div className="absolute top-12 md:top-16 xl:top-24 font-title text-center">
            <img
              draggable={false}
              src={MatrixTitle}
              className="w-[160px] md:w-[220px]"
            />
            <div className="mt-4 font-bold text-4xl lg:text-5xl text-white">
              Berries
            </div>
          </div>
          {final && (
            <div className="w-full flex justify-center items-center relative">
              <img
                className="w-[300px] xl:w-[400px] max-w-[80%] reveal-img rounded-lg border-4"
                src={Berry16}
              />
              <div className="absolute bottom-[-100px] md:bottom-[-110px] text-white flex flex-col justify-center items-center reveal-img">
                <div className="font-bold text-lg md:text-2xl font-title">
                  Congrats!
                </div>
                <div className="font-medium text-sm md:text-base font-title">
                  You are now the proud owner of
                </div>
                <div className="font-bold text-xl md:text-3xl font-title mt-2">
                  Matrix Berry #{confirmed.id}
                </div>
              </div>
            </div>
          )}
          {!connected && (
            <button
              className="py-4 px-8 rounded-2xl text-white font-medium border-2 border-r-purple-500 border-t-purple-600 border-l-green-500 border-b-green-600"
              onClick={() => {
                setIsOpenWalletSelector(true);
              }}
            >
              Connect Wallet
            </button>
          )}
          {connected &&
            !start &&
            (network === 0 ? (
              <BerryBuyButton
                setStart={setStart}
                setConfirmed={setConfirmed}
                title="Buy for 10 ADA"
              />
            ) : (
              <div className="text-white text-center">
                Wrong network,
                <br /> please switch to mainnet.
              </div>
            ))}
          {start && !final && (
            <div className="font-light text-white absolute bottom-28 opacity-90">
              {confirmed.state
                ? "Waiting for revelation"
                : " Waiting for confirmation"}
            </div>
          )}
          <div className="absolute bottom-6 md:bottom-8 left-8 md:left-10 flex flex-row justify-center items-center">
            <a href={"https://berrypool.io"} target={"_blank"}>
              <img
                draggable={false}
                src={BerryLogo}
                className="w-[30px] md:w-[40px]"
              />
            </a>
            <a href={"https://github.com"} target={"_blank"}>
              <div className="flex flex-row justify-center items-center ml-6 md:ml-10">
                <img
                  draggable={false}
                  src={DiscordLogo}
                  className="w-[20px] md:w-[30px]"
                />
                <div className="text-white text-sm ml-2 md:ml-4">
                  View source
                </div>
              </div>
            </a>
          </div>
        </div>
      </div>
      <WalletSelector
        isOpen={isOpenWalletSelector}
        setIsOpen={setIsOpenWalletSelector}
        onConnected={async () => {
          const network = await window.walletApi.getNetworkId();
          setNetwork(network);
          setConnected(true);
        }}
      />
    </>
  );
};

export default IndexPage;

export const Head: HeadFC = () => <title>Home Page</title>;
