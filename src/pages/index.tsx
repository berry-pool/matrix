import * as React from "react";
import type { HeadFC } from "gatsby";
import BerryLogo from "../images/berry.svg";
import DiscordLogo from "../images/github.png";
import MatrixTitle from "../images/matrixTitle.svg";
import metadata from "../data/metadata.json";
import { WalletSelector } from "../components/walletSelector";
import { HelpDialog } from "../components/helpDialog";
import { BerryBuyButton } from "../components/buyButton";
import { UTxO } from "lucid-cardano";
import { contractDetails } from "../contract/config";
// Animation needs to be dynamically imported as it makes use of window quite a few times and so messes up SSR
const { BerryAnimation } =
  typeof window !== "undefined"
    ? await import("../components/animation")
    : { BerryAnimation: null };

const IndexPage = () => {
  const [connected, setConnected] = React.useState<boolean>(false);
  const [start, setStart] = React.useState<boolean>(false);
  const [confirmed, setConfirmed] = React.useState<boolean>(false);
  const [final, setFinal] = React.useState<boolean>(false);
  const [isOpenWalletSelector, setIsOpenWalletSelector] =
    React.useState<boolean>(false);
  const [isOpenHelpDialog, setIsOpenHelpDialog] =
    React.useState<boolean>(false);
  const [network, setNetwork] = React.useState<number>(1);
  const [selection, setSelection] = React.useState<{
    id: number;
    berryUtxo: UTxO | null;
  }>();

  return (
    <>
      <div className="w-screen h-screen bg-gray-900 relative overflow-hidden">
        <>
          <div className="absolute top-[-150px] left-[180px] bg-green-600 w-[300px] h-[300px] rounded-full"></div>
          <div className="absolute bottom-[-300px] left-[500px] bg-[#782AC9] w-[400px] h-[400px] rounded-full"></div>
          <div className="absolute bottom-[-250px] left-[550px] bg-gray-900 w-[300px] h-[300px] rounded-full"></div>
          <div className="absolute top-[600px] left-[500px] bg-[#782AC9] w-[30px] h-[30px] rounded-full"></div>
          <div className="absolute top-[320px] right-[600px] bg-[#782AC9] w-[60px] h-[60px] rounded-full"></div>
          <>
            <div className="absolute bottom-[200px] right-[500px] bg-green-600 w-[50px] h-[50px] rounded-full"></div>
            <div className="absolute bottom-[205px] right-[505px] bg-gray-900 w-[40px] h-[40px] rounded-full"></div>
          </>
        </>
        <div className="absolute top-0 left-0 w-full h-full">
          {start && (
            //@ts-ignore
            <BerryAnimation
              confirmed={confirmed}
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
                src={
                  "https://spacebudz.mypinata.cloud/ipfs/" +
                  metadata[selection?.id!].image.slice(7)
                }
              />
              <div className="absolute bottom-[-100px] md:bottom-[-110px] text-white flex flex-col justify-center items-center reveal-img">
                <div className="font-bold text-lg md:text-2xl font-title">
                  Congrats!
                </div>
                <div className="font-medium text-sm md:text-base font-title">
                  You are now the proud owner of
                </div>
                <div className="font-bold text-xl md:text-3xl font-title mt-2">
                  Matrix Berry #{selection?.id}
                </div>
              </div>
            </div>
          )}
          {!connected && (
            <>
              <button
                className="py-4 px-8 rounded-2xl text-white font-medium border-2 border-r-purple-500 border-t-purple-600 border-l-green-500 border-b-green-600"
                onClick={() => {
                  setIsOpenWalletSelector(true);
                }}
              >
                Connect Wallet
              </button>
              <button
                className="mt-6 text-white font-light text-sm"
                onClick={() => setIsOpenHelpDialog(true)}
              >
                What am I doing here?
              </button>
            </>
          )}
          {connected &&
            !start &&
            (network === 1 ? (
              !Number.isInteger(selection?.id) ? (
                <div className="text-white text-center w-[90%] max-w-md">
                  Matrix Berries are sold out.
                </div>
              ) : (selection?.berryUtxo &&
                  Date.now() <=
                    contractDetails.mintStart -
                      contractDetails.txValidToThreshold) ||
                Date.now() >=
                  contractDetails.mintStart +
                    contractDetails.txValidFromThreshold ? (
                <BerryBuyButton
                  setStart={setStart}
                  setConfirmed={setConfirmed}
                  title={`Buy for ${
                    selection?.berryUtxo
                      ? contractDetails.paymentAmount / 1000000n / 2n
                      : contractDetails.paymentAmount / 1000000n
                  } ADA`}
                  selection={selection!}
                />
              ) : (
                <div className="text-white text-center w-[90%] max-w-md">
                  Public sale has not started yet. In order to get a Matrix
                  Berry now you need to be a Berry holder (
                  <b>
                    until{" "}
                    {new Date(
                      contractDetails.mintStart -
                        contractDetails.txValidToThreshold
                    ).toLocaleString()}
                  </b>
                  ). If you already bought a Matrix Berry as holder then you
                  have to wait for the public sale as well.
                  <div className="font-medium mt-6">Public sale starts at:</div>
                  <div className="font-bold text-xl mt-2">
                    {new Date(
                      contractDetails.mintStart +
                        contractDetails.txValidFromThreshold
                    ).toLocaleString()}
                  </div>
                </div>
              )
            ) : (
              <div className="text-white text-center w-[90%] max-w-md">
                Wrong network,
                <br /> please switch to mainnet.
              </div>
            ))}
          {start && !final && (
            <div className="font-light text-white absolute bottom-28 opacity-90">
              {confirmed
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
            <a href={"https://github.com/berry-pool/matrix"} target={"_blank"}>
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
            <button
              className="ml-6 md:ml-10 text-white"
              onClick={() => setIsOpenHelpDialog(true)}
            >
              Help
            </button>
          </div>
        </div>
      </div>
      <WalletSelector
        isOpen={isOpenWalletSelector}
        setIsOpen={setIsOpenWalletSelector}
        onConnected={async () => {
          const network = await window.walletApi.getNetworkId();
          const Contract = await import("../contract/offchain");
          const [id, berryUtxo] = await Contract.getRandomAvailable();
          setSelection({ id, berryUtxo });
          setNetwork(network);
          setConnected(true);
        }}
      />
      <HelpDialog
        isOpen={isOpenHelpDialog}
        setIsOpen={setIsOpenHelpDialog}
        contractDetails={contractDetails}
      />
    </>
  );
};

export default IndexPage;

export const Head: HeadFC = () => <title>Matrix Berries</title>;
