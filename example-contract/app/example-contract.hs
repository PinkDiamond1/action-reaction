import Prelude
import Cardano.Api
import ExampleContract ( exampleContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "example-contract.plutus" Nothing exampleContractScript 
  case result of
    Left err -> print $ displayError err
    Right () -> return ()