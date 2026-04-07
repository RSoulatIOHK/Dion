# Cardano Network Configuration Files

This directory contains configuration files for Dion to connect to different Cardano networks. These files are vendored from the official [cardano-configurations](https://github.com/input-output-hk/cardano-configurations) repository.

## Directory Structure

```
config/
├── mainnet/       # Mainnet configuration
├── preprod/       # Preprod testnet configuration
└── preview/       # Preview testnet configuration
```

Each network directory contains:

- `config.json` - Main node configuration
- `byron-genesis.json` - Byron era genesis parameters
- `shelley-genesis.json` - Shelley era genesis parameters
- `alonzo-genesis.json` - Alonzo era genesis parameters (Plutus)
- `conway-genesis.json` - Conway era genesis parameters (Governance)

## Network Details

### Mainnet

- **Network Magic**: 764824073
- **Purpose**: Production network
- **Chain Size**: ~12.8M+ blocks
- **Consensus**: Praos (Proof of Stake)

### Preprod

- **Network Magic**: 1
- **Purpose**: Pre-production testing environment
- **Chain Size**: Smaller than mainnet, periodically reset
- **Consensus**: Praos

### Preview

- **Network Magic**: 2
- **Purpose**: Development and rapid testing
- **Chain Size**: ~3.9M blocks (smallest testnet)
- **Consensus**: Praos
- **Note**: Best choice for development due to faster sync

## Usage with Dion

These config files can be loaded using the `loadNetworkConfig` function in `Dion.Config`:

```lean
import Dion.Config

def main : IO Unit := do
  match ← loadNetworkConfig "config/preview/config.json" with
  | .error e => IO.println s!"Failed to load config: {e}"
  | .ok config =>
      IO.println s!"Loaded config for {config.protocol}"
      -- Use config...
```

## Updating Configuration Files

To update to the latest configurations from the cardano-configurations repository:

```bash
# Update mainnet
curl -sL https://raw.githubusercontent.com/input-output-hk/cardano-configurations/master/network/mainnet/cardano-node/config.json -o config/mainnet/config.json
curl -sL https://raw.githubusercontent.com/input-output-hk/cardano-configurations/master/network/mainnet/cardano-node/byron-genesis.json -o config/mainnet/byron-genesis.json
curl -sL https://raw.githubusercontent.com/input-output-hk/cardano-configurations/master/network/mainnet/cardano-node/shelley-genesis.json -o config/mainnet/shelley-genesis.json
curl -sL https://raw.githubusercontent.com/input-output-hk/cardano-configurations/master/network/mainnet/cardano-node/alonzo-genesis.json -o config/mainnet/alonzo-genesis.json
curl -sL https://raw.githubusercontent.com/input-output-hk/cardano-configurations/master/network/mainnet/cardano-node/conway-genesis.json -o config/mainnet/conway-genesis.json

# Update preview
curl -sL https://raw.githubusercontent.com/input-output-hk/cardano-configurations/master/network/preview/cardano-node/config.json -o config/preview/config.json
curl -sL https://raw.githubusercontent.com/input-output-hk/cardano-configurations/master/network/preview/cardano-node/byron-genesis.json -o config/preview/byron-genesis.json
curl -sL https://raw.githubusercontent.com/input-output-hk/cardano-configurations/master/network/preview/cardano-node/shelley-genesis.json -o config/preview/shelley-genesis.json
curl -sL https://raw.githubusercontent.com/input-output-hk/cardano-configurations/master/network/preview/cardano-node/alonzo-genesis.json -o config/preview/alonzo-genesis.json
curl -sL https://raw.githubusercontent.com/input-output-hk/cardano-configurations/master/network/preview/cardano-node/conway-genesis.json -o config/preview/conway-genesis.json

# Update preprod
curl -sL https://raw.githubusercontent.com/input-output-hk/cardano-configurations/master/network/preprod/cardano-node/config.json -o config/preprod/config.json
curl -sL https://raw.githubusercontent.com/input-output-hk/cardano-configurations/master/network/preprod/cardano-node/byron-genesis.json -o config/preprod/byron-genesis.json
curl -sL https://raw.githubusercontent.com/input-output-hk/cardano-configurations/master/network/preprod/cardano-node/shelley-genesis.json -o config/preprod/shelley-genesis.json
curl -sL https://raw.githubusercontent.com/input-output-hk/cardano-configurations/master/network/preprod/cardano-node/alonzo-genesis.json -o config/preprod/alonzo-genesis.json
curl -sL https://raw.githubusercontent.com/input-output-hk/cardano-configurations/master/network/preprod/cardano-node/conway-genesis.json -o config/preprod/conway-genesis.json
```

## Source

These configuration files are maintained by the Cardano community and IOHK/Input Output Global:

- **Repository**: https://github.com/input-output-hk/cardano-configurations
- **License**: Apache 2.0
- **Last Updated**: See individual file timestamps

## Notes

- Genesis files rarely change as they define the initial chain state
- `config.json` may be updated for protocol upgrades or tracing improvements
- The cardano-configurations repository is updated daily from The Cardano Book
- Always use the official repository as the source of truth for production deployments
