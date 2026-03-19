# Preprod Testnet Era History

Source: Koios API (https://preprod.koios.rest/api/v1/epoch_info) queried 2026-03-16.

## System Start
- **Date**: 2022-06-01T00:00:00Z
- **Unix**: 1654041600
- **Network Magic**: 1

## Byron Parameters
- epochLength: 21600 slots
- slotDuration: 20000 ms (20s)
- k (security param): 2160

## Shelley+ Parameters
- epochLength: 432000 slots
- slotLength: 1s
- securityParam: 2160
- activeSlotsCoeff: 0.05

## Era Transitions

| Era     | Start Epoch | End Epoch | Start Slot | End Slot    | Start Time (s) | End Time (s) |
|---------|-------------|-----------|------------|-------------|-----------------|--------------|
| Byron   | 0           | 4         | 0          | 86,400      | 0               | 1,728,000    |
| Shelley | 4           | 5         | 86,400     | 518,400     | 1,728,000       | 2,160,000    |
| Allegra | 5           | 6         | 518,400    | 950,400     | 2,160,000       | 2,592,000    |
| Mary    | 6           | 7         | 950,400    | 1,382,400   | 2,592,000       | 3,024,000    |
| Alonzo  | 7           | 12        | 1,382,400  | 3,542,400   | 3,024,000       | 5,184,000    |
| Babbage | 12          | 163       | 3,542,400  | 68,774,400  | 5,184,000       | 70,416,000   |
| Conway  | 163         | ongoing   | 68,774,400 | -           | 70,416,000      | -            |

### Slot/Time Calculations

Byron end:
- 4 epochs x 21600 slots = 86,400 slots
- 86,400 slots x 20s = 1,728,000 seconds

Post-Byron (epoch N, N >= 4):
- slot = 86,400 + (N - 4) x 432,000
- time = 1,728,000 + (N - 4) x 432,000

### Verification (2026-03-16)
- Current epoch: 276, slot in epoch: 392,099
- Absolute slot: 86,400 + (276 - 4) x 432,000 + 392,099 = 117,982,499 (confirmed)
- Block: 4,514,667

## Shelley Genesis (key params)

```json
{
    "systemStart": "2022-06-01T00:00:00Z",
    "networkMagic": 1,
    "networkId": "Testnet",
    "activeSlotsCoeff": 0.05,
    "securityParam": 2160,
    "epochLength": 432000,
    "slotLength": 1,
    "maxLovelaceSupply": 45000000000000000,
    "slotsPerKESPeriod": 129600,
    "maxKESEvolutions": 62,
    "updateQuorum": 5,
    "protocolParams": {
        "minFeeA": 44,
        "minFeeB": 155381,
        "maxBlockBodySize": 65536,
        "maxTxSize": 16384,
        "maxBlockHeaderSize": 1100,
        "keyDeposit": 2000000,
        "poolDeposit": 500000000,
        "minPoolCost": 340000000,
        "eMax": 18,
        "nOpt": 150,
        "a0": 0.3,
        "rho": 0.003,
        "tau": 0.20,
        "decentralisationParam": 1,
        "minUTxOValue": 1000000,
        "protocolVersion": { "major": 2, "minor": 0 }
    }
}
```

## Conway Genesis (governance params)

```json
{
    "poolVotingThresholds": {
        "committeeNormal": 0.51,
        "committeeNoConfidence": 0.51,
        "hardForkInitiation": 0.51,
        "motionNoConfidence": 0.51,
        "ppSecurityGroup": 0.51
    },
    "dRepVotingThresholds": {
        "motionNoConfidence": 0.67,
        "committeeNormal": 0.67,
        "committeeNoConfidence": 0.6,
        "updateToConstitution": 0.75,
        "hardForkInitiation": 0.6,
        "ppNetworkGroup": 0.67,
        "ppEconomicGroup": 0.67,
        "ppTechnicalGroup": 0.67,
        "ppGovGroup": 0.75,
        "treasuryWithdrawal": 0.67
    },
    "committeeMinSize": 7,
    "committeeMaxTermLength": 146,
    "govActionLifetime": 6,
    "govActionDeposit": 100000000000,
    "dRepDeposit": 500000000,
    "dRepActivity": 20,
    "minFeeRefScriptCostPerByte": 15
}
```

## Alonzo Genesis (execution params)

```json
{
    "lovelacePerUTxOWord": 34482,
    "executionPrices": {
        "prSteps": { "numerator": 721, "denominator": 10000000 },
        "prMem": { "numerator": 577, "denominator": 10000 }
    },
    "maxTxExUnits": { "exUnitsMem": 10000000, "exUnitsSteps": 10000000000 },
    "maxBlockExUnits": { "exUnitsMem": 50000000, "exUnitsSteps": 40000000000 },
    "maxValueSize": 5000,
    "collateralPercentage": 150,
    "maxCollateralInputs": 3
}
```
