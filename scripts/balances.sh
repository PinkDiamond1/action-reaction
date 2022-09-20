#!/usr/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../testnet.magic)
#
#
ex_script_path="../example-contract/example-contract.plutus"
ex_SCRIPT_ADDRESS=$(${cli} address build --payment-script-file ${ex_script_path} --testnet-magic ${testnet_magic})

rep_script_path="../represent-contract/represent-contract.plutus"
rep_SCRIPT_ADDRESS=$(${cli} address build --payment-script-file ${rep_script_path} --testnet-magic ${testnet_magic})

vote_script_path="../voting-contract/voting-contract.plutus"
vote_SCRIPT_ADDRESS=$(${cli} address build --payment-script-file ${vote_script_path} --testnet-magic ${testnet_magic})

distro_script_path="../locking-contract/locking-contract.plutus"
distro_SCRIPT_ADDRESS=$(${cli} address build --payment-script-file ${distro_script_path} --testnet-magic ${testnet_magic})

#
SELLER_ADDRESS=$(cat wallets/seller-wallet/payment.addr)
BUYER_ADDRESS=$(cat wallets/buyer-wallet/payment.addr)
REFERENCE_ADDRESS=$(cat wallets/reference-wallet/payment.addr)
COLLAT_ADDRESS=$(cat wallets/collat-wallet/payment.addr)
DELEG_ADDRESS=$(cat wallets/delegator-wallet/payment.addr)

#
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json
${cli} query tip --testnet-magic ${testnet_magic} | jq

#
echo
echo -e "\033[1;35m Script Addresses:"
echo -e "\nDistro"
echo -e "\n${distro_SCRIPT_ADDRESS}\n";
${cli} query utxo --address ${distro_SCRIPT_ADDRESS} --testnet-magic ${testnet_magic}

echo -e "\nVote"
echo -e "\n${vote_SCRIPT_ADDRESS}\n";
${cli} query utxo --address ${vote_SCRIPT_ADDRESS} --testnet-magic ${testnet_magic}

echo -e "\nRep"
echo -e "\n${rep_SCRIPT_ADDRESS}\n";
${cli} query utxo --address ${rep_SCRIPT_ADDRESS} --testnet-magic ${testnet_magic}

echo -e "\nExample"
echo -e "\n${ex_SCRIPT_ADDRESS}\n";
${cli} query utxo --address ${ex_SCRIPT_ADDRESS} --testnet-magic ${testnet_magic}

echo -e "\033[0m"

#
echo
echo -e "\033[1;36m Minter Address:" 
echo -e "\n${SELLER_ADDRESS}\n";
${cli} query utxo --address ${SELLER_ADDRESS} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

#
echo
echo -e "\033[1;32m Buyer Address:" 
echo -e "\n${BUYER_ADDRESS}\n";
${cli} query utxo --address ${BUYER_ADDRESS} --testnet-magic ${testnet_magic}
echo -e "\nDelegator Address:" 
echo -e "\n${DELEG_ADDRESS}\n";
${cli} query utxo --address ${DELEG_ADDRESS} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

#
echo
echo -e "\033[1;34m Reference Address:" 
echo -e "\n \033[1;34m ${REFERENCE_ADDRESS}\n";
${cli} query utxo --address ${REFERENCE_ADDRESS} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

#
echo
echo -e "\033[1;33m Collateral Address:" 
echo -e "\n${COLLAT_ADDRESS}\n";
${cli} query utxo --address ${COLLAT_ADDRESS} --testnet-magic ${testnet_magic}
echo -e "\033[0m"
