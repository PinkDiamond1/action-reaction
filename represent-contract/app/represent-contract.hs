import Prelude
import Cardano.Api
import RepresentContract ( representContractScript)

main :: IO ()
main = do
  result <- writeFileTextEnvelope "represent-contract.plutus" Nothing representContractScript
  case result of
    Left  err -> print $ displayError err
    Right ()  -> return ()