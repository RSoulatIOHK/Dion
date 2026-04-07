import Dion.TUI.State

/-!
# Prometheus Metrics

Collects node metrics from TUIState and renders them in Prometheus text
exposition format (text/plain; version=0.0.4).

## Exposed Metrics
- dion_blocks_synced_total
- dion_tip_slot
- dion_tip_block_number
- dion_rollbacks_total
- dion_peers_connected
- dion_mempool_tx_count
- dion_mempool_bytes
- dion_consensus_vrf_valid_total
- dion_consensus_vrf_invalid_total
- dion_consensus_kes_valid_total
- dion_consensus_kes_invalid_total
- dion_consensus_opcert_valid_total
- dion_consensus_opcert_invalid_total
- dion_consensus_headers_validated_total
- dion_epoch_current
- dion_uptime_seconds
-/

namespace Dion.Monitoring.Metrics

open Dion.TUI.State

/-- Render a single Prometheus metric line -/
private def metricLine (name : String) (help : String) (mtype : String) (value : Nat) : String :=
  s!"# HELP {name} {help}\n# TYPE {name} {mtype}\n{name} {value}\n"

/-- Render a gauge metric -/
private def gauge (name help : String) (value : Nat) : String :=
  metricLine name help "gauge" value

/-- Render a counter metric -/
private def counter (name help : String) (value : Nat) : String :=
  metricLine name help "counter" value

/-- Render all metrics from TUIState in Prometheus text format -/
def renderPrometheus (state : TUIState) (nowMs : Nat) : String :=
  let uptimeSec := (nowMs - state.startedAt) / 1000
  let peersConnected := state.peers.filter (fun p => p.status == "syncing" || p.status == "connected") |>.length

  String.join [
    counter "dion_blocks_synced_total" "Total number of blocks synced" state.blocksReceived,
    gauge "dion_tip_slot" "Current tip slot number" state.tipSlot,
    gauge "dion_tip_block_number" "Current tip block number" state.tipBlockNo,
    counter "dion_rollbacks_total" "Total number of chain rollbacks" state.rollbacks,
    gauge "dion_peers_connected" "Number of currently connected peers" peersConnected,
    gauge "dion_mempool_tx_count" "Number of transactions in the mempool" state.mempoolTxCount,
    gauge "dion_mempool_bytes" "Size of the mempool in bytes" state.mempoolBytes,
    counter "dion_consensus_headers_validated_total" "Total block headers validated" state.consensus.validatedHeaders,
    counter "dion_consensus_vrf_valid_total" "VRF proofs that verified correctly" state.consensus.vrfValid,
    counter "dion_consensus_vrf_invalid_total" "VRF proofs that failed verification" state.consensus.vrfInvalid,
    counter "dion_consensus_kes_valid_total" "KES signatures that verified correctly" state.consensus.kesValid,
    counter "dion_consensus_kes_invalid_total" "KES signatures that failed verification" state.consensus.kesInvalid,
    counter "dion_consensus_opcert_valid_total" "Operational certificates that verified" state.consensus.opCertValid,
    counter "dion_consensus_opcert_invalid_total" "Operational certificates that failed" state.consensus.opCertInvalid,
    gauge "dion_epoch_current" "Current epoch number" state.consensus.currentEpoch,
    gauge "dion_uptime_seconds" "Node uptime in seconds" uptimeSec
  ]

end Dion.Monitoring.Metrics
