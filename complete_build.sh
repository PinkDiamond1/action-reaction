#!/bin/bash
# set -e
if [[ $# -eq 0 ]] ; then
    echo 'Please Supply A Token Name That Will Be Used For The Starter And The Action Token Name'
    exit 1
fi
# Complete Build
echo -e "\033[1;35m Starting... \033[0m" 

minter_pkh=$(cat start_info.json | jq -r .minter)

# set up start info
variable=${minter_pkh}; jq --arg variable "$variable" '.scripts[0].keyHash=$variable' scripts/policy/policy.script > scripts/policy/policy-new.script
mv scripts/policy/policy-new.script scripts/policy/policy.script


export CARDANO_NODE_SOCKET_PATH=$(cat scripts/path_to_socket.sh)
cli=$(cat scripts/path_to_cli.sh)
testnet_magic=$(cat testnet.magic)
slot=$(${cli} query tip --testnet-magic ${testnet_magic} | jq -r .slot)
deltaT=$(cat start_info.json | jq -r .cutOff)
cutOff=$((${slot} + ${deltaT}))

variable=${cutOff}; jq -r --argjson variable $variable '.scripts[1].slot=$variable' scripts/policy/policy.script > scripts/policy/policy-new.script
mv scripts/policy/policy-new.script scripts/policy/policy.script

# exit

cardano-cli transaction policyid --script-file scripts/policy/policy.script > scripts/policy/starter.id
policy_id=$(cat scripts/policy/starter.id)

tkn_name=${1}
tkn_name=$(echo ${tkn_name:0:32})
echo -e "\033[1;36m Token Name: ${tkn_name} \033[0m"

token_name=$(echo -n ${tkn_name} | od -A n -t x1 | sed 's/ *//g' | tr -d '\n')
echo -e "\033[1;36m Starter Token ${policy_id}.${token_name} \033[0m"

variable=${policy_id}; jq --arg variable "$variable" '.starterPid=$variable' start_info.json > start_info-new.json
mv start_info-new.json start_info.json
variable=${token_name}; jq --arg variable "$variable" '.starterTkn=$variable' start_info.json > start_info-new.json
mv start_info-new.json start_info.json

variable=${token_name}; jq --arg variable "$variable" '.fields[1].bytes=$variable' scripts/data/datums/vote_datum.json > scripts/data/datums/vote_datum-new.json
mv scripts/data/datums/vote_datum-new.json scripts/data/datums/vote_datum.json

# starter nft data
python3 -c "import binascii;a=$(cat start_info.json | jq .starterPid);s=binascii.unhexlify(a);print([x for x in s])" > start.pid
python3 -c "import binascii;a=$(cat start_info.json | jq .starterTkn);s=binascii.unhexlify(a);print([x for x in s])" > start.tkn


# Adds in the locking token into the contract.
python3 -c "from update_contracts import changeStartLockPid;changeStartLockPid('./locking-contract/src/LockingContract.hs', './locking-contract/src/LockingContract-new.hs', $(cat start.pid))"
mv ./locking-contract/src/LockingContract-new.hs ./locking-contract/src/LockingContract.hs
python3 -c "from update_contracts import changeStartLockTkn;changeStartLockTkn('./locking-contract/src/LockingContract.hs', './locking-contract/src/LockingContract-new.hs', $(cat start.tkn))"
mv ./locking-contract/src/LockingContract-new.hs ./locking-contract/src/LockingContract.hs

python3 -c "from update_contracts import changeStartLockPid;changeStartLockPid('./minting-contract/src/MintingContract.hs', './minting-contract/src/MintingContract-new.hs', $(cat start.pid))"
mv ./minting-contract/src/MintingContract-new.hs ./minting-contract/src/MintingContract.hs
python3 -c "from update_contracts import changeStartLockTkn;changeStartLockTkn('./minting-contract/src/MintingContract.hs', './minting-contract/src/MintingContract-new.hs', $(cat start.tkn))"
mv ./minting-contract/src/MintingContract-new.hs ./minting-contract/src/MintingContract.hs

python3 -c "from update_contracts import changeStartLockPid;changeStartLockPid('./voting-contract/src/VotingContract.hs', './voting-contract/src/VotingContract-new.hs', $(cat start.pid))"
mv ./voting-contract/src/VotingContract-new.hs ./voting-contract/src/VotingContract.hs
python3 -c "from update_contracts import changeStartLockTkn;changeStartLockTkn('./voting-contract/src/VotingContract.hs', './voting-contract/src/VotingContract-new.hs', $(cat start.tkn))"
mv ./voting-contract/src/VotingContract-new.hs ./voting-contract/src/VotingContract.hs

python3 -c "from update_contracts import changeStartLockPid;changeStartLockPid('./represent-contract/src/RepresentContract.hs', './represent-contract/src/RepresentContract-new.hs', $(cat start.pid))"
mv ./represent-contract/src/RepresentContract-new.hs ./represent-contract/src/RepresentContract.hs
python3 -c "from update_contracts import changeStartLockTkn;changeStartLockTkn('./represent-contract/src/RepresentContract.hs', './represent-contract/src/RepresentContract-new.hs', $(cat start.tkn))"
mv ./represent-contract/src/RepresentContract-new.hs ./represent-contract/src/RepresentContract.hs

python3 -c "from update_contracts import changeStartLockPid;changeStartLockPid('./example-contract/src/ExampleContract.hs', './example-contract/src/ExampleContract-new.hs', $(cat start.pid))"
mv ./example-contract/src/ExampleContract-new.hs ./example-contract/src/ExampleContract.hs
python3 -c "from update_contracts import changeStartLockTkn;changeStartLockTkn('./example-contract/src/ExampleContract.hs', './example-contract/src/ExampleContract-new.hs', $(cat start.tkn))"
mv ./example-contract/src/ExampleContract-new.hs ./example-contract/src/ExampleContract.hs


# build
cd locking-contract
rm validator.bytes
rm validator.hash
cabal build -w ghc-8.10.7 -O2
cabal run locking-contract
cardano-cli transaction policyid --script-file locking-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes
# locking validator hash
echo -e "\033[1;36m Validator Hash: $(cat validator.hash) \033[0m"
echo -e "\033[1;36m Validator Bytes: $(cat validator.bytes) \033[0m"


cd ..


# adds in the locking hash into the script
python3 -c "from update_contracts import changeLockHash;changeLockHash('./minting-contract/src/MintingContract.hs', './minting-contract/src/MintingContract-new.hs', $(cat ./locking-contract/validator.bytes))"
mv ./minting-contract/src/MintingContract-new.hs ./minting-contract/src/MintingContract.hs


# build nft minting
cd minting-contract
rm policy.id
rm policy.bytes
cabal build -w ghc-8.10.7 -O2
cabal run minting-contract
cardano-cli transaction policyid --script-file minting-contract.plutus > policy.id
python3 -c "import binascii;a='$(cat policy.id)';s=binascii.unhexlify(a);print([x for x in s])" > policy.bytes

# nft minting validator hash
echo -e "\033[1;36m Policy Id: $(cat policy.id) \033[0m"
echo -e "\033[1;36m Policy Bytes: $(cat policy.bytes) \033[0m"


cd ..

# build the voting contrat
cd voting-contract
rm validator.bytes
rm validator.hash
cabal build -w ghc-8.10.7 -O2
cabal run voting-contract
cardano-cli transaction policyid --script-file voting-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes

echo -e "\033[1;36m Validator Hash: $(cat validator.hash) \033[0m"
echo -e "\033[1;36m Validator Bytes: $(cat validator.bytes) \033[0m"

# update other contracts that need the vote hash here
cd ..

python3 -c "from update_contracts import changeVoteHash;changeVoteHash('./represent-contract/src/RepresentContract.hs', './represent-contract/src/RepresentContract-new.hs', $(cat ./voting-contract/validator.bytes))"
mv ./represent-contract/src/RepresentContract-new.hs ./represent-contract/src/RepresentContract.hs

python3 -c "from update_contracts import changeVoteHash;changeVoteHash('./example-contract/src/ExampleContract.hs', './example-contract/src/ExampleContract-new.hs', $(cat ./voting-contract/validator.bytes))"
mv ./example-contract/src/ExampleContract-new.hs ./example-contract/src/ExampleContract.hs

# build representative and iou contracts
cd represent-contract
rm validator.bytes
rm validator.hash
cabal build -w ghc-8.10.7 -O2
cabal run represent-contract
cardano-cli transaction policyid --script-file represent-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes

echo -e "\033[1;36m Validator Hash: $(cat validator.hash) \033[0m"
echo -e "\033[1;36m Validator Bytes: $(cat validator.bytes) \033[0m"

cd ..
# adds in the locking hash into the script
python3 -c "from update_contracts import changeLockHash;changeLockHash('./iou-contract/src/IouContract.hs', './iou-contract/src/IouContract-new.hs', $(cat ./represent-contract/validator.bytes))"
mv ./iou-contract/src/IouContract-new.hs ./iou-contract/src/IouContract.hs


cd iou-contract
rm policy.id
rm policy.bytes
cabal build -w ghc-8.10.7 -O2
cabal run iou-contract
cardano-cli transaction policyid --script-file iou-contract.plutus > policy.id
python3 -c "import binascii;a='$(cat policy.id)';s=binascii.unhexlify(a);print([x for x in s])" > policy.bytes

# nft minting validator hash
echo -e "\033[1;36m Policy Id: $(cat policy.id) \033[0m"
echo -e "\033[1;36m Policy Bytes: $(cat policy.bytes) \033[0m"

# update datums for ious

# build the example contract
cd ..
cd example-contract
rm validator.bytes
rm validator.hash
cabal build -w ghc-8.10.7 -O2
cabal run example-contract
cardano-cli transaction policyid --script-file example-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes

echo -e "\033[1;36m Validator Hash: $(cat validator.hash) \033[0m"
echo -e "\033[1;36m Validator Bytes: $(cat validator.bytes) \033[0m"

# update datum for example contracts.

# update the datums for the nft generator
cd ../scripts/data
variable=$(cat ../../minting-contract/policy.id); jq --arg variable "$variable" '.fields[0].bytes=$variable' datums/distro_datum.json > datums/distro_datum-new.json
mv datums/distro_datum-new.json datums/distro_datum.json

variable=$(cat ../../minting-contract/policy.id); jq --arg variable "$variable" '.fields[0].bytes=$variable' datums/vote_datum.json > datums/vote_datum-new.json
mv datums/vote_datum-new.json datums/vote_datum.json

variable=$(cat ../../iou-contract/policy.id); jq --arg variable "$variable" '.fields[1].bytes=$variable' datums/rep_datum.json > datums/rep_datum-new.json
mv datums/rep_datum-new.json datums/rep_datum.json

# reset datum
ft=0
variable=${ft}; jq --argjson variable $variable '.fields[1].int=$variable' datums/distro_datum.json > datums/distro_datum-new.json
mv datums/distro_datum-new.json datums/distro_datum.json

ft=1
variable=${ft}; jq --argjson variable $variable '.fields[2].int=$variable' datums/vote_datum.json > datums/vote_datum-new.json
mv datums/vote_datum-new.json datums/vote_datum.json




cd ../..

find . -name '*.hash' -type f -exec sha256sum {} \; > hash.hashes
echo -e "\033[1;36m \nvalidator sha256sum \n \033[0m"
echo -e "\033[1;33m$(cat hash.hashes) \033[0m"

echo

find . -name 'policy.id' -type f -exec sha256sum {} \; > policy.hashes
echo -e "\033[1;36m policy sha256sum \n \033[0m"
echo -e "\033[1;33m$(cat policy.hashes) \033[0m"

echo

find . -name '*.hashes' -type f -exec sha256sum {} \; > final.check
echo -e "\033[1;35m \nfinal sha256sum \n \033[0m"
echo -e "\033[1;32m$(sha256sum final.check) \033[0m"

echo
echo "DONE"
#
# exit
#