#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../testnet.magic)

# get params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json

#
mint_path="../iou-contract/iou-contract.plutus"
script_path="../represent-contract/represent-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})
#
delegator_address=$(cat wallets/delegator-wallet/payment.addr)
delegator_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/delegator-wallet/payment.vkey)
#
variable=${delegator_pkh}; jq --arg variable "$variable" '.fields[0].fields[1].bytes=$variable' data/redeemers/decrease_redeemer.json > data/redeemers/decrease_redeemer-new.json
mv data/redeemers/decrease_redeemer-new.json data/redeemers/decrease_redeemer.json

#
collat_address=$(cat wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/collat-wallet/payment.vkey)
#

# doing this early to query it for an amount of action tokens on the wallet
echo -e "\033[0;36m Gathering Buyer UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${delegator_address} \
    --out-file tmp/delegator_utxo.json

TXNS=$(jq length tmp/delegator_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${delegator_address} \033[0m \n";
   exit;
fi
policy_id=$(cat ../minting-contract/policy.id)
iou_id=$(cat ../iou-contract/policy.id)
starter_id=$(cat ../start_info.json | jq -r .starterPid)
token_name=$(cat ../start_info.json | jq -r .starterTkn)
iou_name=$(echo -n "iou" | xxd -ps)
alltkns=0
findTokens=$(jq -r --argjson alltkns 0 --arg policy_id "$iou_id" --arg token_name "$iou_name" 'to_entries[] | select(.value.value[$policy_id][$token_name] >= 1) | .value.value[$policy_id][$token_name] | . + $alltkns' tmp/delegator_utxo.json)
total=0
stringarray=(${findTokens})
for i in "${stringarray[@]}"
do
  total=$((${total} + ${i}))
done
# this is the correct amount of action tokens
actionAmount=${total}
# echo $actionAmount
action_asset="${actionAmount} ${policy_id}.${token_name}"
mint_asset="-${actionAmount} ${iou_id}.696f75"

variable=${actionAmount}; jq -r --argjson variable "$variable" '.fields[0].fields[0].int=$variable' data/redeemers/decrease_redeemer.json > data/redeemers/decrease_redeemer-new.json
mv data/redeemers/decrease_redeemer-new.json data/redeemers/decrease_redeemer.json


starter_nft_min_utxo=5000000
echo "Starter NFT Min Fee: "${starter_nft_min_utxo}

script_address_out="${script_address} + ${starter_nft_min_utxo}"
delegator_address_out="${delegator_address} + ${starter_nft_min_utxo} + ${action_asset}"
echo "Token Script OUTPUT: "${script_address_out}
echo "Mint OUTPUT: "${delegator_address_out}
#
# exit
#
alltxin=""
# Gather only UTxO that are ada-only or are holding the correct NFT
TXIN=$(jq -r --arg alltxin "" --arg policy_id "$iou_id" --arg token_name "$iou_name" 'to_entries[] | select((.value.value | length < 2) or .value.value[$policy_id][$token_name] >= 1) | .key | . + $alltxin + " --tx-in"' tmp/delegator_utxo.json)
delegator_tx_in=${TXIN::-8}

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


script_ref_utxo=$(${cli} transaction txid --tx-file tmp/ref-repres-utxo.signed)


echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${delegator_address} \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${delegator_tx_in} \
    --read-only-tx-in-reference ${vote_tx_in} \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file data/redeemers/decrease_redeemer.json \
    --tx-out="${delegator_address_out}" \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/datums/rep_datum.json \
    --required-signer-hash ${delegator_pkh} \
    --required-signer-hash ${collat_pkh} \
    --mint="${mint_asset}" \
    --mint-tx-in-reference="${script_ref_utxo}#2" \
    --mint-plutus-script-v2 \
    --policy-id="${iou_id}" \
    --mint-reference-tx-in-redeemer-file data/redeemers/decrease_redeemer.json \
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
    --signing-key-file wallets/delegator-wallet/payment.skey \
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
