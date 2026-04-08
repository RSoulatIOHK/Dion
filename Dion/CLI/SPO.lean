import Dion.Crypto.TextEnvelope
import Dion.Crypto.Sign.KESSign
import Dion.Network.Crypto

/-!
# SPO Key Generation and Registration

`dion spo keygen [--dir PATH]`
  Generates all keys needed to run a stake pool:
  - payment.skey / payment.vkey    (Ed25519, for funding the registration tx)
  - stake.skey   / stake.vkey      (Ed25519, for the reward address)
  - pool.skey    / pool.vkey       (Ed25519, cold key — keep offline!)
  - vrf.skey     / vrf.vkey        (ECVRF, for leader election)
  - kes.skey     / kes.vkey        (Sum-KES-6, for block signing)
  - node.cert                      (Operational certificate, period 0)
  - node.counter                   (OpCert issue counter)

All files are written as cardano-cli-compatible TextEnvelope JSON.
After key generation, the typical flow is:

  1. Fund the payment address from the Preview faucet
  2. Run `dion spo register --dir PATH --relay HOST:PORT` to build + submit the
     pool registration transaction via the local N2C socket
  3. Start the node: `dion run --preview --spo-keys PATH --skip-to-tip`
-/

namespace Dion.CLI.SPO

open Dion.Crypto.TextEnvelope
open Dion.Crypto.Sign.KESSign
open Dion.Network.Crypto

-- ========================
-- = Bech32 address utils =
-- ========================

/-- Encode a raw hash as a stake address (e1... on preview/preprod/mainnet).
    This is a simplified address encoder — bech32 encoding is complex,
    so we produce the raw hex for use with cardano-cli. -/
def rawHashHex (bytes : ByteArray) : String :=
  encodeHex bytes


-- ========================
-- = Key generation       =
-- ========================

/-- Generate and write all SPO keys to the given directory. -/
def spoKeygen (keyDir : String) (kesPeriod : Nat := 0) : IO Unit := do
  -- Ensure the directory exists
  IO.FS.createDirAll keyDir

  IO.println s!"[spo] Generating keys in {keyDir}/"

  -- 1. Payment key pair (Ed25519)
  let (payPk, paySk) ← ed25519_keypair_ffi
  -- paySk is NaCl format: seed(32) || pk(32) — write seed as skey
  let paySeed := paySk.extract 0 32
  writeTextEnvelope s!"{keyDir}/payment.skey" .paymentSigningKey "Payment Signing Key" paySeed
  writeTextEnvelope s!"{keyDir}/payment.vkey" .paymentVerificationKey "Payment Verification Key" payPk
  IO.println s!"  ✓ payment.skey / payment.vkey"

  -- 2. Stake key pair (Ed25519)
  let (stakePk, stakeSk) ← ed25519_keypair_ffi
  let stakeSeed := stakeSk.extract 0 32
  writeTextEnvelope s!"{keyDir}/stake.skey" .stakeSigningKey "Stake Signing Key" stakeSeed
  writeTextEnvelope s!"{keyDir}/stake.vkey" .stakeVerificationKey "Stake Verification Key" stakePk
  IO.println s!"  ✓ stake.skey  / stake.vkey"

  -- 3. Cold key pair (Ed25519) — pool operator identity
  let (coldPk, coldSk) ← ed25519_keypair_ffi
  let coldSeed := coldSk.extract 0 32
  writeTextEnvelope s!"{keyDir}/pool.skey" .stakePoolSigningKey "Stake Pool Operator Signing Key" coldSeed
  writeTextEnvelope s!"{keyDir}/pool.vkey" .stakePoolVerificationKey "Stake Pool Operator Verification Key" coldPk
  IO.println s!"  ✓ pool.skey   / pool.vkey"

  -- 4. VRF key pair (ECVRF-ED25519 — same curve as Ed25519)
  let (vrfPk, vrfSk) ← ed25519_keypair_ffi
  let vrfSeed := vrfSk.extract 0 32
  -- VRF skey = seed(32) || vk(32) = 64 bytes
  let vrfSkBytes := vrfSeed ++ vrfPk
  writeTextEnvelope s!"{keyDir}/vrf.skey" .vrfSigningKey "VRF Signing Key" vrfSkBytes
  writeTextEnvelope s!"{keyDir}/vrf.vkey" .vrfVerificationKey "VRF Verification Key" vrfPk
  IO.println s!"  ✓ vrf.skey    / vrf.vkey"

  -- 5. KES key pair (Sum-KES-6, period 0)
  match ← kesKeygen with
  | .error e => IO.eprintln s!"[spo] KES keygen failed: {e}"
  | .ok (kesSk, kesVk) =>
    writeTextEnvelope s!"{keyDir}/kes.skey" .kesSigningKey "KES Signing Key" kesSk
    writeTextEnvelope s!"{keyDir}/kes.vkey" .kesVerificationKey "KES Verification Key" kesVk
    IO.println s!"  ✓ kes.skey    / kes.vkey"

    -- 6. Operational certificate (binds KES hot key to cold key)
    --    opcert_body = [hotVKey, seqNum, kesPeriod]
    --    cold_sig    = Ed25519_sign(coldSk, cbor(opcert_body))
    --
    --    The message to sign is the CBOR encoding of [hotVKey, seqNum, kesPeriod].
    --    In Cardano, this is: 82 58 20 <kesVk> <seqNum> <kesPeriod>
    --    We encode it manually here.
    let encUInt (n : Nat) : ByteArray :=
      if n <= 23 then ByteArray.mk #[n.toUInt8]
      else if n <= 255 then ByteArray.mk #[0x18, n.toUInt8]
      else ByteArray.mk #[0x19, (n / 256).toUInt8, (n % 256).toUInt8]
    -- Message: CBOR array of 3 elements [hotVKey(bytes), seqNum(uint), kesPeriod(uint)]
    let opcertMsg :=
      ByteArray.mk #[0x83]        -- 3-element array
      ++ cborWrapBytes kesVk      -- hotVKey
      ++ encUInt 0                -- seqNum = 0
      ++ encUInt kesPeriod        -- kesPeriod
    -- Sign with the FULL NaCl cold signing key (seed || pk = 64 bytes)
    let coldSig ← ed25519_sign_ffi coldSk opcertMsg
    writeOperationalCert s!"{keyDir}/node.cert" kesVk 0 kesPeriod coldSig coldPk
    writeOpCertCounter s!"{keyDir}/node.counter" 1 coldPk
    IO.println s!"  ✓ node.cert   / node.counter (KES period {kesPeriod})"

  -- 7. Compute pool ID and payment address hash for display
  let poolId ← blake2b_256 coldPk
  let payVkHash ← blake2b_256 payPk
  let stakeVkHash ← blake2b_256 stakePk

  IO.println ""
  IO.println "[spo] Key generation complete!"
  IO.println ""
  IO.println s!"  Pool ID (cold vkey hash): {encodeHex poolId}"
  IO.println s!"  Payment vkey hash:        {encodeHex payVkHash}"
  IO.println s!"  Stake vkey hash:          {encodeHex stakeVkHash}"
  IO.println ""
  IO.println "[spo] Next steps:"
  IO.println s!"  1. Get payment address:  cardano-cli address build --payment-verification-key-file {keyDir}/payment.vkey --testnet-magic 2"
  IO.println s!"  2. Fund from faucet:     https://docs.cardano.org/cardano-testnet/tools/faucet/"
  IO.println s!"  3. Register pool:        dion spo register --dir {keyDir} --relay YOUR_IP:3001 --pledge 1000000"
  IO.println s!"  4. Run node:             dion run --preview --spo-keys {keyDir} --external-addr YOUR_IP:3001"

-- ========================
-- = KES rotation        =
-- ========================

/-- Rotate the KES key and reissue the opcert using the EXISTING cold key.
    Call this whenever:
      - The opcert approaches its 62-evolution limit (see KES expiry warning)
      - You're setting up on a live network where the opcert start period must
        match the current KES period (e.g. preview KES period ≈ 829 as of March 2026)

    Steps performed:
      1. Load the existing cold.skey + cold.vkey (pool identity is preserved)
      2. Load the existing node.counter to get the next sequence number
      3. Generate a fresh KES key pair (at evolution 0)
      4. Issue a new opcert: hotVKey=new KES vk, seqNum=counter+1, kesPeriod=given
      5. Write new kes.skey, kes.vkey, node.cert, node.counter

    After running this, restart the node — no on-chain registration needed.

    To find the current KES period:
      cardano-cli query tip --preview  # shows slot
      # kesPeriod = slot / 129600      # (slotsPerKESPeriod for preview)
-/
def rotateKES (keyDir : String) (kesPeriod : Nat) : IO Unit := do
  -- Load existing cold signing key (32-byte seed stored in file)
  let coldSeed ← match ← loadPoolSigningKey s!"{keyDir}/pool.skey" with
    | .error e => do IO.eprintln s!"[spo] Failed to load pool.skey: {e}"; return
    | .ok seed => pure seed
  -- Load existing cold verification key (32 bytes)
  let coldVk ← match ← loadPoolVerificationKey s!"{keyDir}/pool.vkey" with
    | .error e => do IO.eprintln s!"[spo] Failed to load pool.vkey: {e}"; return
    | .ok vk => pure vk
  -- Reconstruct 64-byte NaCl signing key: seed || pk (required by ed25519_sign_ffi)
  let coldSk := coldSeed ++ coldVk

  -- Read the next sequence number from node.counter
  -- Counter CBOR: 0x82 <seqNum_cbor> <coldVKey_cbor>
  let seqNum ← do
    match ← loadTextEnvelope s!"{keyDir}/node.counter" with
    | .error _ => pure 1  -- fallback if counter file not found
    | .ok te =>
      -- rawBytes[0] = 0x82 (2-element array), rawBytes[1..] = seqNum CBOR uint
      if te.rawBytes.size >= 2 then
        let b1 := te.rawBytes[1]!.toNat
        if b1 / 32 == 0 then  -- major type 0 = unsigned int
          let info := b1 % 32
          if info <= 23 then pure info
          else if info == 24 && te.rawBytes.size >= 3 then
            pure te.rawBytes[2]!.toNat
          else if info == 25 && te.rawBytes.size >= 4 then
            pure (te.rawBytes[2]!.toNat * 256 + te.rawBytes[3]!.toNat)
          else pure 1
        else pure 1
      else pure 1

  IO.println s!"[spo] Rotating KES key in {keyDir}/"
  IO.println s!"[spo]   Cold vkey: {encodeHex coldVk |>.take 16}..."
  IO.println s!"[spo]   KES period: {kesPeriod}"
  IO.println s!"[spo]   Opcert sequence: {seqNum}"

  -- Generate fresh KES key pair
  match ← kesKeygen with
  | .error e => IO.eprintln s!"[spo] KES keygen failed: {e}"
  | .ok (kesSk, kesVk) =>
    writeTextEnvelope s!"{keyDir}/kes.skey" .kesSigningKey "KES Signing Key" kesSk
    writeTextEnvelope s!"{keyDir}/kes.vkey" .kesVerificationKey "KES Verification Key" kesVk
    IO.println s!"  ✓ kes.skey / kes.vkey (fresh, evolution 0)"

    -- Issue new opcert
    let encUInt (n : Nat) : ByteArray :=
      if n <= 23 then ByteArray.mk #[n.toUInt8]
      else if n <= 255 then ByteArray.mk #[0x18, n.toUInt8]
      else ByteArray.mk #[0x19, (n / 256).toUInt8, (n % 256).toUInt8]
    let opcertMsg :=
      ByteArray.mk #[0x83]
      ++ cborWrapBytes kesVk
      ++ encUInt seqNum
      ++ encUInt kesPeriod
    let coldSig ← ed25519_sign_ffi coldSk opcertMsg
    writeOperationalCert s!"{keyDir}/node.cert" kesVk seqNum kesPeriod coldSig coldVk
    writeOpCertCounter s!"{keyDir}/node.counter" (seqNum + 1) coldVk
    IO.println s!"  ✓ node.cert (seqNum={seqNum}, kesPeriod={kesPeriod})"
    IO.println s!"  ✓ node.counter → {seqNum + 1}"
    IO.println ""
    IO.println "[spo] KES rotation complete. Restart the node with:"
    IO.println s!"  dion run --preview --spo-keys {keyDir} --external-addr YOUR_IP:3001"

-- ========================
-- = Pool metadata        =
-- ========================

/-- Generate poolMetadata.json and print its blake2b-256 hash (needed for registration). -/
def spoMetadata
    (name : String := "My Dion Pool")
    (ticker : String := "CNODE")
    (description : String := "A stake pool running Dion")
    (homepage : String := "")
    (outFile : String := "poolMetadata.json")
    : IO Unit := do
  -- Cardano pool metadata JSON (strict field order matters for the hash)
  let json :=
    "{\n" ++
    s!"  \"name\": \"{name}\",\n" ++
    s!"  \"description\": \"{description}\",\n" ++
    s!"  \"ticker\": \"{ticker}\",\n" ++
    s!"  \"homepage\": \"{homepage}\"\n" ++
    "}\n"
  IO.FS.writeFile outFile json
  let hash ← blake2b_256 json.toUTF8
  IO.println s!"[spo] Pool metadata written to {outFile}"
  IO.println s!"[spo] Metadata hash (blake2b-256): {encodeHex hash}"
  IO.println ""
  IO.println "[spo] Host this file at a public HTTPS URL, then pass to registration:"
  IO.println s!"  dion spo register ... --metadata-url https://your-host/{outFile} --metadata-file {outFile}"

-- ========================
-- = Pool registration    =
-- ========================

/-- Run a shell command, print output, return success -/
private def runCmd (cmd : String) (args : Array String) : IO Bool := do
  IO.println s!"$ {cmd} {" ".intercalate args.toList}"
  let out ← IO.Process.output { cmd := cmd, args := args }
  if !out.stdout.isEmpty then IO.print out.stdout
  if !out.stderr.isEmpty then IO.eprint out.stderr
  if out.exitCode != 0 then
    IO.eprintln s!"[spo] Command failed (exit {out.exitCode})"
    return false
  return true

/-- Build and submit a pool registration transaction end-to-end. -/
def spoRegister
    (keyDir : String)
    (relayHost : String) (relayPort : UInt16)
    (pledgeLovelace : Nat := 1_000_000_000)
    (costLovelace : Nat := 340_000_000)
    (margin : Float := 0.05)
    (socketPath : String := "/tmp/dion.socket")
    (metadataUrl : Option String := none)
    (metadataFile : Option String := none)
    (testnetMagic : Nat := 2)
    (update : Bool := false)
    : IO Unit := do
  let magic := toString testnetMagic

  -- Step 1: Payment address
  IO.println "[spo] Step 1: Getting payment address..."
  let addrOut ← IO.Process.output { cmd := "cardano-cli", args := #[
    "latest", "address", "build",
    "--payment-verification-key-file", s!"{keyDir}/payment.vkey",
    "--stake-verification-key-file", s!"{keyDir}/stake.vkey",
    "--testnet-magic", magic ] }
  if addrOut.exitCode != 0 then
    IO.eprintln s!"[spo] Failed to build address: {addrOut.stderr}"; return
  let paymentAddr := addrOut.stdout.trim
  IO.println s!"[spo] Payment address: {paymentAddr}"

  -- Step 2: Query UTxO
  IO.println "[spo] Step 2: Querying UTxO..."
  let utxoOut ← IO.Process.output { cmd := "cardano-cli", args := #[
    "latest", "query", "utxo",
    "--socket-path", socketPath,
    "--address", paymentAddr,
    "--testnet-magic", magic ] }
  if utxoOut.exitCode != 0 then
    IO.eprintln s!"[spo] Failed to query UTxO: {utxoOut.stderr}"; return
  IO.println utxoOut.stdout
  -- Parse first UTxO (TxHash#TxIx from line 3+)
  let utxoLines := utxoOut.stdout.trim.splitOn "\n" |>.drop 2 |>.filter (· != "")
  if utxoLines.isEmpty then
    IO.eprintln "[spo] No UTxO found. Fund your payment address first."; return
  let firstLine := utxoLines.head!
  let parts := firstLine.splitOn " " |>.filter (· != "")
  if parts.length < 2 then
    IO.eprintln s!"[spo] Could not parse UTxO line: {firstLine}"; return
  let txIn := s!"{parts[0]!}#{parts[1]!}"
  IO.println s!"[spo] Using UTxO: {txIn}"

  -- Step 3: Stake key registration certificate (omitted for pool parameter updates)
  let includeStakeCert ← if update then do
    IO.println "[spo] Step 3: Pool update — skipping stake key registration certificate"
    pure false
  else do
    IO.println "[spo] Step 3: Creating stake key registration certificate..."
    let ok3 ← runCmd "cardano-cli" #[
      "latest", "stake-address", "registration-certificate",
      "--stake-verification-key-file", s!"{keyDir}/stake.vkey",
      "--key-reg-deposit-amt", "2000000",
      "--out-file", s!"{keyDir}/stake.cert" ]
    if !ok3 then return
    pure true

  -- Step 4: Pool registration certificate
  IO.println "[spo] Step 4: Creating pool registration certificate..."
  -- Compute metadata hash if provided
  let metadataHashHex : Option String ← match metadataFile with
    | some path =>
      try
        let content ← IO.FS.readFile path
        let hash ← blake2b_256 content.toUTF8
        pure (some (encodeHex hash))
      catch _ => pure none
    | none => pure none
  if relayHost.isEmpty then
    IO.eprintln "[spo] Error: --relay HOST:PORT is required"; return
  let poolCertArgs := #[
    "latest", "stake-pool", "registration-certificate",
    "--cold-verification-key-file", s!"{keyDir}/pool.vkey",
    "--vrf-verification-key-file", s!"{keyDir}/vrf.vkey",
    "--pool-pledge", toString pledgeLovelace,
    "--pool-cost", toString costLovelace,
    "--pool-margin", toString margin,
    "--pool-reward-account-verification-key-file", s!"{keyDir}/stake.vkey",
    "--pool-owner-stake-verification-key-file", s!"{keyDir}/stake.vkey",
    "--single-host-pool-relay", relayHost,
    "--pool-relay-port", toString relayPort ]
  let poolCertArgs := match metadataUrl, metadataHashHex with
    | some url, some hash => poolCertArgs ++ #["--metadata-url", url, "--metadata-hash", hash]
    | some url, none => poolCertArgs ++ #["--metadata-url", url]
    | _, _ => poolCertArgs
  let poolCertArgs := poolCertArgs ++ #["--testnet-magic", magic, "--out-file", s!"{keyDir}/pool.cert"]
  let ok4 ← runCmd "cardano-cli" poolCertArgs
  if !ok4 then return

  -- Step 5: Build transaction
  IO.println "[spo] Step 5: Building transaction..."
  let buildArgs := #[
    "latest", "transaction", "build",
    "--socket-path", socketPath,
    "--testnet-magic", magic,
    "--tx-in", txIn,
    "--change-address", paymentAddr ]
    ++ (if includeStakeCert then #["--certificate-file", s!"{keyDir}/stake.cert"] else #[])
    ++ #["--certificate-file", s!"{keyDir}/pool.cert",
         "--witness-override", "3",
         "--out-file", s!"{keyDir}/reg.txbody"]
  let ok5 ← runCmd "cardano-cli" buildArgs
  if !ok5 then return

  -- Step 6: Sign
  IO.println "[spo] Step 6: Signing transaction..."
  let ok6 ← runCmd "cardano-cli" #[
    "latest", "transaction", "sign",
    "--tx-body-file", s!"{keyDir}/reg.txbody",
    "--signing-key-file", s!"{keyDir}/payment.skey",
    "--signing-key-file", s!"{keyDir}/stake.skey",
    "--signing-key-file", s!"{keyDir}/pool.skey",
    "--testnet-magic", magic,
    "--out-file", s!"{keyDir}/reg.tx" ]
  if !ok6 then return

  -- Step 7: Submit
  IO.println "[spo] Step 7: Submitting transaction..."
  let ok7 ← runCmd "cardano-cli" #[
    "latest", "transaction", "submit",
    "--socket-path", socketPath,
    "--testnet-magic", magic,
    "--tx-file", s!"{keyDir}/reg.tx" ]
  if !ok7 then return

  IO.println ""
  IO.println "[spo] Pool registration submitted successfully!"
  IO.println s!"[spo] Payment address: {paymentAddr}"
  IO.println "[spo] Wait 2 epoch boundaries (~2 days on preview) for stake to activate."

end Dion.CLI.SPO
