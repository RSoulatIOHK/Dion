import Dion.Network.TxSubmission2State
import Dion.Network.PeerSharingState
import Dion.Network.Mempool

/-!
# Phase 3 Proof Scaffolds

Formal properties for Phase 3 components: TxSubmission2 protocol,
PeerSharing protocol, and Mempool invariants.

Most proofs use `sorry` as they involve IO or complex list properties.
Trivially provable structural theorems are fully proved.
-/

namespace Dion.Proofs.Phase3Proofs

open Dion.Network.Mempool

-- ============================
-- = TxSubmission2 Properties =
-- ============================

/-- The protocol starts in StInit -/
theorem txsubmission_starts_init (sock : Dion.Network.Socket.Socket) :
    (Dion.Network.TxSubmission2State.TypedTxSubmission2.new sock).state =
      Dion.Network.TxSubmission2State.TxSubmission2State.StInit := by
  rfl

/-- Agency is always well-defined: every state is either client, server, or terminal -/
theorem txsubmission_agency_complete (s : Dion.Network.TxSubmission2State.TxSubmission2State) :
    Dion.Network.TxSubmission2State.isClientAgency s = true ∨
    Dion.Network.TxSubmission2State.isServerAgency s = true ∨
    Dion.Network.TxSubmission2State.isTerminal s = true := by
  cases s <;> simp [Dion.Network.TxSubmission2State.isClientAgency,
    Dion.Network.TxSubmission2State.isServerAgency,
    Dion.Network.TxSubmission2State.isTerminal]

/-- Client and server agency are mutually exclusive -/
theorem txsubmission_agency_exclusive (s : Dion.Network.TxSubmission2State.TxSubmission2State) :
    ¬(Dion.Network.TxSubmission2State.isClientAgency s = true ∧
      Dion.Network.TxSubmission2State.isServerAgency s = true) := by
  cases s <;> simp [Dion.Network.TxSubmission2State.isClientAgency,
    Dion.Network.TxSubmission2State.isServerAgency]

-- ============================
-- = PeerSharing Properties   =
-- ============================

/-- The protocol starts in StIdle -/
theorem peersharing_starts_idle (sock : Dion.Network.Socket.Socket) :
    (Dion.Network.PeerSharingState.TypedPeerSharing.new sock).state =
      Dion.Network.PeerSharingState.PeerSharingState.StIdle := by
  rfl

/-- Agency is always well-defined -/
theorem peersharing_agency_complete (s : Dion.Network.PeerSharingState.PeerSharingState) :
    Dion.Network.PeerSharingState.isClientAgency s = true ∨
    Dion.Network.PeerSharingState.isServerAgency s = true ∨
    Dion.Network.PeerSharingState.isTerminal s = true := by
  cases s <;> simp [Dion.Network.PeerSharingState.isClientAgency,
    Dion.Network.PeerSharingState.isServerAgency,
    Dion.Network.PeerSharingState.isTerminal]

-- ============================
-- = Mempool Properties       =
-- ============================

/-- Empty mempool has no entries -/
theorem mempool_empty_is_empty :
    (Mempool.empty config).entries = [] := by
  rfl

/-- Empty mempool has zero bytes -/
theorem mempool_empty_zero_bytes :
    (Mempool.empty config).totalBytes = 0 := by
  rfl

/-- Removing confirmed transactions never increases mempool size -/
theorem mempool_remove_monotone (pool : Mempool) (hashes : List ByteArray) :
    (pool.removeConfirmed hashes).entries.length ≤ pool.entries.length := by
  sorry

/-- No duplicate transactions in a well-formed mempool -/
theorem mempool_no_duplicates (pool : Mempool) :
    (pool.entries.map (·.txHash)).Nodup := by
  sorry

/-- Mempool size never exceeds config maxSize after prune -/
theorem mempool_prune_bounded (pool : Mempool) (now : Nat) :
    (pool.prune now).entries.length ≤ pool.config.maxSize := by
  sorry

/-- getTxIds returns at most `count` entries -/
theorem mempool_getTxIds_bounded (pool : Mempool) (count : Nat) :
    (pool.getTxIds count).length ≤ count := by
  sorry

end Dion.Proofs.Phase3Proofs
