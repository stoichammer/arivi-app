Allegory/AllPay 'proxy' that generates unique "provably correct" destination addresses. 

- This proxy application is lightweight & independent of Nexa.
- Does not index or serve any Bitcoin blockchain data.
- Confidentially stores users child XPub key and generates a new unique receive address per transaction
- Supplements with the Merkle branch/proof so the client can validate the generated address against Recipients address commitment(Merkle root)
- Interactively produces an atomic transaction to spend the authorized nUTXO and the UTXO/s the sender intends to transfer.
- Provides both TLS endpoint & AriviP2P interface for Recipient and Sender APIs.
- Only uses LevelDB for minimal persistance, the Merkle trees are dynamically recomputed upon application init.
