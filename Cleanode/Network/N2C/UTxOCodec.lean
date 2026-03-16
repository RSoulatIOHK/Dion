import Cleanode.Network.Cbor
import Cleanode.Ledger.UTxO

/-!
# UTxO Query Result CBOR Encoding

Encodes UTxO set entries as a CBOR map for LocalStateQuery's GetFilteredUTxO response.

## Wire Format
The result is a CBOR map:
```
{ [txHash, outputIndex] => [address, value, ...], ... }
```

Where each output value for post-Alonzo is:
```
[address, amount_or_multiasset, datum_option, script_ref]
```

For simplicity (ADA-only outputs without scripts):
```
{ [txHash(32), idx] => [address, lovelace] }
```

## References
- Shelley CDDL: transaction_output
- Babbage CDDL: post_alonzo_transaction_output
-/

namespace Cleanode.Network.N2C.UTxOCodec

open Cleanode.Network.Cbor
open Cleanode.Network.ConwayBlock (TxOutput)
open Cleanode.Ledger.UTxO

-- ====================
-- = Encoding         =
-- ====================

/-- Encode a UTxO ID as CBOR array: [txHash, outputIndex] -/
def encodeUTxOId (id : UTxOId) : ByteArray :=
  encodeArrayHeader 2 ++ encodeBytes id.txHash ++ encodeUInt id.outputIndex

/-- Encode a transaction output as post-Alonzo CBOR map:
    { 0: address, 1: value }
    Value is just lovelace for simple outputs. -/
def encodePostAlonzoOutput (output : TxOutput) : ByteArray :=
  -- Post-Alonzo output is a map with integer keys
  encodeMapHeader 2
    ++ encodeUInt 0 ++ encodeBytes output.address   -- key 0: address
    ++ encodeUInt 1 ++ encodeUInt output.amount      -- key 1: value (lovelace)

/-- Encode a filtered UTxO result as a CBOR map from UTxOId → TxOutput.
    This is the result format for GetFilteredUTxO. -/
def encodeFilteredUTxOResult (entries : List UTxOEntry) : ByteArray :=
  let mapHeader := encodeMapHeader entries.length
  entries.foldl (fun acc entry =>
    acc ++ encodeUTxOId entry.id ++ encodePostAlonzoOutput entry.output
  ) mapHeader

/-- Filter UTxO entries by a set of addresses -/
def filterByAddresses (utxo : UTxOSet) (addresses : List ByteArray) : List UTxOEntry :=
  utxo.toList.filter fun entry =>
    addresses.any (· == entry.output.address)

/-- Filter UTxO entries by a set of TxIns (txHash, outputIndex) -/
def filterByTxIns (utxo : UTxOSet) (txIns : List (ByteArray × Nat)) : List UTxOEntry :=
  txIns.filterMap fun (txHash, idx) =>
    let id : UTxOId := { txHash, outputIndex := idx }
    (utxo.lookup id).map fun output => { id, output }

end Cleanode.Network.N2C.UTxOCodec
