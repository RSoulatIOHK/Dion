# Preview Network Sync Guide

## Overview

The Preview network is Cardano's smallest testnet, designed for rapid testing and development. It's ideal for Cleanode development because:

- **Smaller chain size**: ~3.9M blocks vs 12.8M+ on mainnet
- **Faster sync times**: Significantly quicker initial sync
- **Current era support**: Running on latest eras (Alonzo, Babbage, Conway)
- **Frequent resets**: Chain is periodically reset, keeping it manageable

## Network Configuration

### Network Parameters

- **Network Magic**: `2`
- **Protocol Versions**: 14, 15 (currently accepting v15)
- **Current Era**: 4 (Alonzo) and beyond
- **Bootstrap Peers**:
  - `preview-node.world.dev.cardano.org:30002`
  - `preview-node.play.dev.cardano.org:3001`

## Switching to Preview Network

To switch from mainnet to Preview network in Cleanode:

1. Open [Main.lean](../Main.lean)
2. Comment out the mainnet configuration:
   ```lean
   -- MAINNET (12.8M+ blocks - slow to sync)
   -- let (host, port) := mainnetBootstrapPeers.head!
   -- let proposal := createMainnetProposal
   -- let networkName := "Mainnet"
   ```

3. Uncomment the Preview configuration:
   ```lean
   -- PREVIEW (smallest testnet - fastest to sync)
   let (host, port) := previewBootstrapPeers.head!
   let proposal := createPreviewProposal
   let networkName := "Preview"
   ```

4. Build and run:
   ```bash
   lake build
   ./build/bin/cleanode
   ```

## Sync Process

### Connection Flow

1. **TCP Connection**: Cleanode connects to a Preview bootstrap peer
2. **Handshake Protocol**:
   - Client sends `ProposeVersions` with network magic `2`
   - Server responds with `AcceptVersion` (typically v15)
3. **ChainSync Protocol**:
   - Client sends `MsgFindIntersect` (genesis or resume point)
   - Server responds with `MsgIntersectFound`
   - Client sends `MsgRequestNext` for each block
   - Server responds with `MsgRollForward` containing block headers

### Block Storage

Blocks are persisted to the `data/` directory:
- `data/blocks/` - Individual block files
- `data/sync_state.dat` - Current sync position (slot, block number, hash)

### Resume Capability

Cleanode automatically resumes from the last synced block:
- On first run: Syncs from genesis
- On subsequent runs: Resumes from last saved slot

Example output:
```
✓ Storage initialized at data/blocks
Resuming sync from slot 12345 (100 blocks already synced) on Preview
```

## Era Support

Preview network spans multiple Cardano eras:

| Era | Tag | Name | Status |
|-----|-----|------|--------|
| 0 | Byron | First era | ✓ Supported |
| 1 | Shelley | Proof of Stake | Partial |
| 2 | Allegra | Token locking | Partial |
| 3 | Mary | Native tokens | Partial |
| 4 | Alonzo | Smart contracts | Partial |
| 5 | Babbage | Plutus V2 | Planned |
| 6 | Conway | Governance | Planned |

Currently, Cleanode fully decodes Byron era block headers. Support for later eras is in development.

## Testing Results

### Successful Connection (2024)

```
Connecting to preview-node.world.dev.cardano.org:30002...
✓ Connected!

=== Handshake ===
✓ Handshake proposal sent
✓ Handshake complete: AcceptVersion(version=15, networkMagic=2)

=== ChainSync - Finding Intersection ===
✓ FindIntersect sent (genesis)
✓ Intersection response: MsgIntersectFound

=== ChainSync - Requesting Blocks ===
✓ Block received!
  Era: 4 (Alonzo)
  Block slot: 12345
  Block height: 100
  ✓ Block saved to storage
```

### Block Decoding

For Byron blocks (era 0):
- Protocol magic: `764824073` (mainnet) or `2` (preview)
- Block header structure: `[[slot, blockNo], tag24(headerBytes)]`
- Header contents: protocol magic, previous hash, body proof, consensus data

## Development Workflow

### Recommended Testing Approach

1. **Start with Preview**: Faster iteration for protocol development
2. **Test on Preprod**: Larger testnet for stability testing
3. **Deploy to Mainnet**: Production deployment

### Quick Sync Test

```bash
# Clean previous data
rm -rf data/

# Build and run
lake build
./build/bin/cleanode

# Verify blocks are being saved
ls -lh data/blocks/
```

### Monitoring Progress

The sync process outputs:
- Connection status
- Handshake negotiation
- Block reception rate
- Current slot/block number
- Storage confirmation

## Troubleshooting

### Connection Failures

If connection fails, try alternate bootstrap peer:
```lean
let (host, port) := ("preview-node.play.dev.cardano.org", 3001)
```

### Sync Not Resuming

If sync starts from genesis despite previous sync:
```bash
# Check sync state
cat data/sync_state.dat | xxd | head

# Verify block files exist
ls data/blocks/
```

### Era Decoding Failures

If blocks from later eras fail to decode:
- This is expected - only Byron era is fully implemented
- Block headers are still saved for future processing
- Era-specific decoders are being developed

## Future Improvements

- [ ] Full multi-era block decoding (Shelley, Allegra, Mary, Alonzo, Babbage, Conway)
- [ ] Parallel block fetching for faster sync
- [ ] Block validation with cryptographic verification
- [ ] Transaction parsing and validation
- [ ] Local state query support

## References

- [Preview Network Documentation](https://book.world.dev.cardano.org/environments/preview/)
- [Ouroboros Network Spec](https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec/network-spec.pdf)
- [Cardano Blueprint](https://cardano-scaling.github.io/cardano-blueprint/network/)
