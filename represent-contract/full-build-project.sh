cabal clean
cabal update
cabal build -w ghc-8.10.7 -O2
cabal run represent-contract
cardano-cli transaction policyid --script-file represent-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes

echo "Validator Hash:" $(cat validator.hash)
echo "Validator Bytes:" $(cat validator.bytes)
echo "DONE"