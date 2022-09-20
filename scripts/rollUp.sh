#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../testnet.magic)


# Addresses
sender_address=$(cat wallets/delegator-wallet/payment.addr)
receiver_address=$(cat wallets/delegator-wallet/payment.addr)
# receiver_address="addr_test1qrupt9d9ug2ufnrrajp2q7gwvmrtzzgr80p5ug7q8nt4d66hu0s5mnhxh2853wtsgn9gdz6wuqtaqnkv0yk78p474d6qudapqh"

# Define Asset to be printed here
asset="1 2a0eb5c76f3367580b6a55d35505a6951c93e5218e6cb34b04567a8f.746f6b656e5f33"
change="149000000 ef8ec0648619d43627ab15b11b670cc0a27e4676761d861645dfd8e8.766f74696e675f746f6b656e"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-datum-hash-value 42 \
    --tx-out="${receiver_address} ${asset}" | tr -dc '0-9')
change_to_be_traded="${sender_address} + ${min_utxo} + ${change}"
token_to_be_traded="${receiver_address} + ${min_utxo} + ${asset}"

# echo -e "\nTrading A Token:\n" ${token_to_be_traded}
# echo -e "\nChange:\n" ${change_to_be_traded}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${sender_address} \
    --out-file tmp/sender_utxo.json

TXNS=$(jq length tmp/sender_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${sender_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/sender_utxo.json)
HEXTXIN=${TXIN::-8}

# doing this early to query it for an amount of action tokens on the wallet
echo -e "\033[0;36m Gathering Buyer UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${sender_address} \
    --out-file tmp/sender_utxo.json

TXNS=$(jq length tmp/sender_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${sender_address} \033[0m \n";
   exit;
fi
policy_id=$(cat ../minting-contract/policy.id)
iou_id=$(cat ../iou-contract/policy.id)
starter_id=$(cat ../start_info.json | jq -r .starterPid)
token_name=$(cat ../start_info.json | jq -r .starterTkn)
alltkns=0
findTokens=$(jq -r --argjson alltkns 0 --arg policy_id "$policy_id" --arg token_name "$token_name" 'to_entries[] | select(.value.value[$policy_id][$token_name] >= 1) | .value.value[$policy_id][$token_name] | . + $alltkns' tmp/sender_utxo.json)
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

change_to_be_traded="${sender_address} + 5000000 + ${action_asset}"
echo -e "\nChange:\n" ${change_to_be_traded}

# example_path="../example-contract/example-contract.plutus"
# example_address=$(${cli} address build --payment-script-file ${example_path} --testnet-magic ${testnet_magic})
# example_address_out="${example_address} + 50000000"

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${sender_address} \
    --tx-in ${HEXTXIN} \
    --tx-out="${change_to_be_traded}" \
    --testnet-magic ${testnet_magic})

    # --tx-out="${example_address_out}" \
    # --tx-out-inline-datum-file data/datums/example_datum.json  \
    # --tx-out="${token_to_be_traded}" \
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