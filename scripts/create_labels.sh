#!/bin/bash
# Create GitHub labels for Cleanode project

echo "Creating GitHub labels..."

# Hierarchy labels
gh label create "pi-objective" --description "Program Increment level goal" --color "0E8A16" || true
gh label create "epic" --description "Large body of work" --color "1D76DB" || true
gh label create "story" --description "User-facing functionality" --color "5319E7" || true
gh label create "task" --description "Technical implementation work" --color "FBCA04" || true

# Phase labels
gh label create "phase-1" --description "Chain Observer" --color "D93F0B" || true
gh label create "phase-2" --description "Block Processing" --color "D93F0B" || true
gh label create "phase-3" --description "Relay Node" --color "D93F0B" || true
gh label create "phase-4" --description "Block Production" --color "D93F0B" || true
gh label create "phase-5" --description "Advanced Features" --color "D93F0B" || true

# Component labels
gh label create "crypto" --description "Cryptographic primitives" --color "C5DEF5" || true
gh label create "network" --description "Network layer" --color "C5DEF5" || true
gh label create "ledger" --description "Ledger layer" --color "C5DEF5" || true
gh label create "consensus" --description "Consensus layer" --color "C5DEF5" || true

# Special labels
gh label create "formal-verification" --description "Involves formal proofs" --color "006B75" || true
gh label create "documentation" --description "Documentation needed" --color "0075CA" || true

echo "✅ Labels created!"
