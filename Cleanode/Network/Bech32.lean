/-!
# Bech32 Encoding for Cardano Addresses

Bech32 is a base32 encoding format with error detection checksums.
Cardano uses it to encode addresses in a human-readable format:
- Mainnet payment addresses: addr1...
- Testnet payment addresses: addr_test1...
- Mainnet stake addresses: stake1...
- Testnet stake addresses: stake_test1...

## References
- BIP-173: Bech32 encoding
- CIP-0005: Cardano address encoding
-/

namespace Cleanode.Network.Bech32

-- Bech32 character set (base32)
def bech32Charset : Array Char := #[
  'q', 'p', 'z', 'r', 'y', '9', 'x', '8',
  'g', 'f', '2', 't', 'v', 'd', 'w', '0',
  's', '3', 'j', 'n', '5', '4', 'k', 'h',
  'c', 'e', '6', 'm', 'u', 'a', '7', 'l'
]

-- Bech32m constant (for address checksums)
def bech32mConst : UInt32 := 0x2bc830a3

/-- Convert 8-bit bytes to 5-bit groups -/
def convertBits (data : List UInt8) (fromBits : Nat) (toBits : Nat) (pad : Bool) : Option (List UInt8) :=
  let rec go (acc : Nat) (bits : Nat) (result : List UInt8) (input : List UInt8) : Option (List UInt8) :=
    match input with
    | [] =>
        if bits > 0 && pad then
          let padding := acc <<< (toBits - bits)
          some (result.reverse ++ [padding.toUInt8])
        else if bits > 0 && acc != 0 then
          none  -- Non-zero padding
        else
          some result.reverse
    | byte :: rest =>
        let acc' := (acc <<< fromBits) ||| byte.toNat
        let bits' := bits + fromBits
        if bits' >= toBits then
          let bits'' := bits' - toBits
          let value := acc' >>> bits''
          let mask := (1 <<< bits'') - 1
          let acc'' := acc' &&& mask
          go acc'' bits'' (value.toUInt8 :: result) rest
        else
          go acc' bits' result rest
  go 0 0 [] data

/-- Compute Bech32m checksum -/
def bech32Polymod (values : List UInt8) : UInt32 :=
  let gen : Array UInt32 := #[0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3]

  let rec go (chk : UInt32) (vals : List UInt8) : UInt32 :=
    match vals with
    | [] => chk
    | v :: rest =>
        let b := chk >>> 25
        let chk' := ((chk &&& 0x1ffffff) <<< 5) ^^^ v.toUInt32
        let chk'' := List.range 5 |>.foldl (fun acc i =>
          if (b >>> i.toUInt32) &&& 1 == 1 then
            acc ^^^ gen[i]!
          else
            acc
        ) chk'
        go chk'' rest

  go 1 values

/-- Expand human-readable part for checksum -/
def hrpExpand (hrp : String) : List UInt8 :=
  let chars := hrp.toList
  let high := chars.map (fun c => (c.toNat >>> 5).toUInt8)
  let low := chars.map (fun c => (c.toNat &&& 31).toUInt8)
  high ++ [0] ++ low

/-- Create Bech32m checksum -/
def createChecksum (hrp : String) (data : List UInt8) : List UInt8 :=
  let values := hrpExpand hrp ++ data ++ [0, 0, 0, 0, 0, 0]
  let polymod := bech32Polymod values ^^^ bech32mConst
  -- Generate checksum values (each must be 5-bit: 0-31)
  [0, 1, 2, 3, 4, 5].map (fun i =>
    ((polymod >>> (5 * (5 - i)).toUInt32) &&& 31).toUInt8
  )

/-- Encode data with Bech32m -/
def bech32Encode (hrp : String) (data : List UInt8) : String :=
  let combined := data ++ createChecksum hrp data
  -- Ensure all values are valid 5-bit (0-31)
  let chars := combined.map (fun b =>
    let val := b.toNat % 32  -- Clamp to 0-31
    bech32Charset[val]!
  )
  hrp ++ "1" ++ String.ofList chars

/-- Encode Cardano address to Bech32 format -/
def encodeAddress (addressBytes : ByteArray) (isTestnet : Bool := false) : String :=
  -- Convert bytes to 5-bit groups
  match convertBits addressBytes.toList 8 5 true with
  | none => "invalid_address"
  | some data =>
      -- Determine HRP based on address type and network
      let firstByte := addressBytes[0]!
      let addressType := firstByte >>> 4

      let hrp :=
        if addressType == 0xe || addressType == 0xf then
          -- Stake address
          if isTestnet then "stake_test" else "stake"
        else
          -- Payment address
          if isTestnet then "addr_test" else "addr"

      bech32Encode hrp data

end Cleanode.Network.Bech32
