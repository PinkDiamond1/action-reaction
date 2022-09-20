#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../testnet.magic)
# get params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json
#
mint_path="policy/policy.script"
#
lock_path="../locking-contract/locking-contract.plutus"
lock_address=$(${cli} address build --payment-script-file ${lock_path} --testnet-magic ${testnet_magic})

vote_path="../voting-contract/voting-contract.plutus"
vote_address=$(${cli} address build --payment-script-file ${vote_path} --testnet-magic ${testnet_magic})

example_path="../example-contract/example-contract.plutus"
example_address=$(${cli} address build --payment-script-file ${example_path} --testnet-magic ${testnet_magic})

# collat, seller, reference
seller_address=$(cat wallets/seller-wallet/payment.addr)
seller_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/seller-wallet/payment.vkey)

policy_id=$(cat policy/starter.id)
token_name=$(cat ../start_info.json | jq -r .starterTkn)
MINT_ASSET="2 ${policy_id}.${token_name}"

lock_ASSET="1 ${policy_id}.${token_name}"
vote_ASSET="1 ${policy_id}.${token_name}"
#
starter_nft_min_utxo=5000000
echo "Starter NFT Min Fee: "${starter_nft_min_utxo}

lock_address_out="${lock_address} + $starter_nft_min_utxo + ${lock_ASSET}"
vote_address_out="${vote_address} + $starter_nft_min_utxo + ${vote_ASSET}"
example_address_out="${example_address} + $starter_nft_min_utxo"
echo "Locking OUTPUT: "${lock_address_out}
echo "Voting OUTPUT: "${vote_address_out}
echo "Example OUTPUT: "${example_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Seller UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${seller_address} \
    --out-file tmp/seller_utxo.json

TXNS=$(jq length tmp/seller_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${seller_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/seller_utxo.json)
seller_tx_in=${TXIN::-8}

slot=$(${cli} query tip --testnet-magic ${testnet_magic} | jq .slot)
current_slot=$(($slot - 1))
final_slot=$(($slot + 100))

# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --invalid-before ${current_slot} \
    --invalid-hereafter ${final_slot} \
    --change-address ${seller_address} \
    --tx-in ${seller_tx_in} \
    --tx-out="${lock_address_out}" \
    --tx-out-inline-datum-file data/datums/distro_datum.json  \
    --tx-out="${vote_address_out}" \
    --tx-out-inline-datum-file data/datums/vote_datum.json  \
    --tx-out="${example_address_out}" \
    --tx-out-inline-datum-file data/datums/example_datum.json  \
    --required-signer-hash ${seller_pkh} \
    --mint-script-file policy/policy.script \
    --mint="${MINT_ASSET}" \
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
