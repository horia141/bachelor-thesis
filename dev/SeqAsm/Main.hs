import Data.ByteString.Char8 as C8 (ByteString(..),readFile)

import Core (DevicesCfg(..),Sequencer(..),Component(..),Inst(..),ArgType(..),FormatAtom(..),Device(..))
import Configs (parseSequencersCfg,parseComponentsCfg,parseDeviceCfg)

main :: IO ()
main = do
  sequencersCfg <- C8.readFile "/home/horia/work/batchelor/v6/prj/Sequencers.cfg"
  componentsCfg <- C8.readFile "/home/horia/work/batchelor/v6/prj/Components.cfg"
  deviceText    <- C8.readFile "/home/horia/work/batchelor/v6/prj/Auto2.dev"

  case (parseSequencersCfg sequencersCfg) of
    (Right sequencers) -> do 
         putStrLn $ show sequencers

         case (parseComponentsCfg componentsCfg) of
           (Right components) -> do 
               putStrLn $ show components
               
               case (parseDeviceCfg (DevicesCfg sequencers components) deviceText) of
                 (Right device) -> putStrLn $ show device
                 (Left error) -> putStrLn error
           (Left error) -> putStrLn error
    (Left error) -> putStrLn error



