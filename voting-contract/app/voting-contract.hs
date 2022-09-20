import Prelude
import Cardano.Api
import VotingContract ( votingContractScript)

main :: IO ()
main = do
  result <- writeFileTextEnvelope "voting-contract.plutus" Nothing votingContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()