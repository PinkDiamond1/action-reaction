import Prelude
import Cardano.Api
import IouContract ( mintingPlutusScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "iou-contract.plutus" Nothing mintingPlutusScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()