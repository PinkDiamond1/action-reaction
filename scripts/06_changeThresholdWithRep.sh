#!/bin/bash
set -e

# check for threshold
if [[ $# -eq 0 ]] ; then
    echo 'Please Supply A New Threshold'
    exit 1
fi

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../testnet.magic)

# get params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json

#
script_path="../voting-contract/voting-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})
#
seller_address=$(cat wallets/seller-wallet/payment.addr)
seller_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/seller-wallet/payment.vkey)
#
collat_address=$(cat wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/collat-wallet/payment.vkey)
#
policy_id=$(cat ../minting-contract/policy.id)
starter_id=$(cat ../start_info.json | jq -r .starterPid)
token_name=$(cat ../start_info.json | jq -r .starterTkn)

# doing this early to query it for an amount of action tokens on the wallet
echo -e "\033[0;36m Gathering Buyer UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${seller_address} \
    --out-file tmp/seller_utxo.json

TXNS=$(jq length tmp/seller_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${seller_address} \033[0m \n";
   exit;
fi
alltkns=0
findTokens=$(jq -r --argjson alltkns 0 --arg policy_id "$policy_id" --arg token_name "$token_name" 'to_entries[] | select(.value.value[$policy_id][$token_name] >= 1) | .value.value[$policy_id][$token_name] | . + $alltkns' tmp/seller_utxo.json)
total=0
stringarray=(${findTokens})
for i in "${stringarray[@]}"
do
  total=$((${total} + ${i}))
done
# this is the correct amount of action tokens
actionAmount=${total}

#
action_asset="${actionAmount} ${policy_id}.${token_name}"
UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${seller_address} + 5000000 + ${action_asset}" | tr -dc '0-9')
#
start_id=$(cat policy/starter.id)
START_ASSET="1 ${start_id}.${token_name}"

script_address_out="${script_address} + 5000000 + ${START_ASSET}"
starter_nft_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/datums/worst_case_datum.json | tr -dc '0-9')
starter_nft_min_utxo=5000000
# echo "Starter NFT Min Fee: "${starter_nft_min_utxo}

script_address_out="${script_address} + ${starter_nft_min_utxo} + ${START_ASSET}"
seller_address_out="${seller_address} + ${UTXO_VALUE} + ${action_asset}"
# echo "Token Script OUTPUT: "${script_address_out}
echo "Mint OUTPUT: "${seller_address_out}
#
# exit
#
alltxin=""
# Gather only UTxO that are ada-only or are holding the correct NFT
TXIN=$(jq -r --arg alltxin "" --arg policy_id "$policy_id" --arg token_name "$token_name" 'to_entries[] | select((.value.value | length < 2) or .value.value[$policy_id][$token_name] > 1) | .key | . + $alltxin + " --tx-in"' tmp/seller_utxo.json)
seller_tx_in=${TXIN::-8}

echo -e "\033[0;36m Gathering Collateral UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${collat_address} \
    --out-file tmp/collat_utxo.json

TXNS=$(jq length tmp/collat_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${collat_address} \033[0m \n";
   exit;
fi
collat_utxo=$(jq -r 'keys[0]' tmp/collat_utxo.json)

echo -e "\033[0;36m Gathering Vote UTxO Information  \033[0m"
vote_path="../voting-contract/voting-contract.plutus"
vote_address=$(${cli} address build --payment-script-file ${vote_path} --testnet-magic ${testnet_magic})
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${vote_address} \
    --out-file tmp/voting_utxo.json
TXNS=$(jq length tmp/voting_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${vote_address} \033[0m \n";
   exit;
fi
TXIN=$(jq -r --arg alltxin "" --arg policy_id "$starter_id" --arg token_name "$token_name" 'to_entries[] | select(.value.value[$policy_id][$token_name] >= 1) | .key | . + $alltxin + " --tx-in"' tmp/voting_utxo.json)
vote_tx_in=${TXIN::-8}
# echo ${vote_tx_in}

echo -e "\033[0;36m Gathering Representor UTxO Information  \033[0m"
rep_path="../represent-contract/represent-contract.plutus"
rep_address=$(${cli} address build --payment-script-file ${rep_path} --testnet-magic ${testnet_magic})
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${rep_address} \
    --out-file tmp/represent_utxo.json
TXNS=$(jq length tmp/represent_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${rep_address} \033[0m \n";
   exit;
fi
TXIN=$(jq -r --arg alltxin "" --arg policy_id "$policy_id" --arg token_name "$token_name" 'to_entries[] | select(.value.value[$policy_id][$token_name] >= 1) | .key | . + $alltxin + " --tx-in"' tmp/represent_utxo.json)
rep_tx_in=${TXIN::-8}
# echo ${rep_tx_in}

echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    --testnet-magic ${testnet_magic} \
    --out-file tmp/script_utxo.json

# transaction variables
TXNS=$(jq length tmp/script_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/script_utxo.json)
script_tx_in=${TXIN::-8}

script_ref_utxo=$(${cli} transaction txid --tx-file tmp/ref-exampl-utxo.signed)

# update the datum into the new state
ft=${1}
variable=${ft}; jq -r --argjson variable $variable '.fields[2].int=$variable' data/datums/vote_datum.json > data/datums/vote_datum-new.json
mv data/datums/vote_datum-new.json data/datums/vote_datum.json

# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${seller_address} \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${seller_tx_in} \
    --read-only-tx-in-reference ${vote_tx_in} \
    --read-only-tx-in-reference ${rep_tx_in} \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file data/redeemers/vote_redeemer.json \
    --tx-out="${seller_address_out}" \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/datums/vote_datum.json \
    --required-signer-hash ${seller_pkh} \
    --required-signer-hash ${collat_pkh} \
    --testnet-magic ${testnet_magic})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/seller-wallet/payment.skey \
    --signing-key-file wallets/collat-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic ${testnet_magic}
#    
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file tmp/tx.signed
