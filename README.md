# Dion

A formally verified Cardano node implementation in Lean 4, with proofs of correctness from the [Cardano Blueprint](https://cardano-scaling.github.io/cardano-blueprint/) specifications.

## Overview

Dion aims to be a fully functional Cardano node with formal verification of critical components, proving adherence to the Cardano protocol specifications. The implementation is structured in phases, starting from basic chain observation to full block production capabilities.

## Project Structure

```
Dion/
├── Dion/          # Main Lean 4 source code
│   ├── Network/       # Network layer (Ouroboros protocols)
│   ├── Ledger/        # Ledger rules and state
│   ├── Crypto/        # Cryptographic primitives
│   ├── Consensus/     # Consensus algorithms (Praos)
│   ├── Node/          # Node orchestration
│   └── Config/        # Configuration management
├── scripts/           # Utility scripts for project management
├── PROJECT_BREAKDOWN.md  # Detailed project plan
└── README.md          # This file
```

## Development Phases

### Phase 1: Foundation & Network Layer (Sprints 1-6)
- Project infrastructure and build system
- Cryptographic primitives (Blake2b, Ed25519, VRF, KES)
- CBOR encoding/decoding with correctness proofs
- Network multiplexer and mini-protocols
- Chain sync protocol (read-only)

### Phase 2: Chain Observer & Block Processing (Sprints 7-10)
- Chain observer application
- Block fetch protocol
- Transaction parsing and validation
- UTxO model with invariant proofs
- Ledger state management

### Phase 3: Relay Node Functionality (Sprints 11-12)
- Transaction submission and propagation
- Mempool management
- P2P networking and peer discovery

### Phase 4: Block Production (Sprints 13-15)
- Ouroboros Praos consensus
- VRF-based leader election
- KES key management
- Block forging
- Stake pool operations

### Phase 5: Advanced Features (Sprints 16-17)
- Mithril integration for fast bootstrap
- Performance optimization
- Monitoring and metrics
- Production readiness

## Building

Dion uses the Lake build system for Lean 4.

```bash
# Build the project
lake build

# Run the executable
lake exe dion
```

## Project Management

All project planning, issue tracking, and sprint organization is managed through GitHub:

- **Issues**: 242 issues across 5 PI Objectives, 15 Epics, 38 Stories, and 184 Tasks
- **Project Board**: [Dion Project](https://github.com/users/RSoulatIOHK/projects/14)
- **Sprints**: 17 two-week sprints (~8 months)

See [PROJECT_BREAKDOWN.md](PROJECT_BREAKDOWN.md) for the complete project plan.

### Utility Scripts

The `scripts/` directory contains utility scripts for managing the project:

- **Issue generation**: Create all GitHub issues from PROJECT_BREAKDOWN.md
- **Sprint assignment**: Automatically assign issues to sprints
- **Project board management**: Add issues to project board

See [scripts/README.md](scripts/README.md) for detailed documentation.

## References

- [Cardano Blueprint](https://cardano-scaling.github.io/cardano-blueprint/) - Protocol specifications
- [Ouroboros Network Spec](https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec/network-spec.pdf) - Network layer specification
- [Cardano Node](https://github.com/IntersectMBO/cardano-node) - Reference implementation
- [Cardano Ledger](https://github.com/IntersectMBO/cardano-ledger) - Ledger specifications

## Key Features

- **Formal Verification**: Critical components proven correct against specifications
- **Type Safety**: Leverages Lean 4's dependent type system
- **Modular Architecture**: Clear separation between network, ledger, and consensus layers
- **Specification-Driven**: Implementation follows Cardano Blueprint specifications
- **Incremental Development**: Phased approach from observer to producer

## Contributing

The project follows a sprint-based development model. Check the [project board](https://github.com/users/RSoulatIOHK/projects/14) for current priorities and available tasks.

## License

**Proprietary License - All Rights Reserved**

Copyright (c) 2024-2025 Romain Soulat

This software is proprietary and confidential. You may view the source code for educational, research, or reference purposes only. Any use, modification, or distribution requires explicit written permission from the author.

See [LICENSE](LICENSE) file for complete terms and conditions.

For licensing inquiries or permission requests, please contact the author.
