import Cleanode.TUI.State

/-!
# Prometheus Metrics

Collects node metrics from TUIState and renders them in Prometheus text
exposition format (text/plain; version=0.0.4).

## Exposed Metrics
- cleanode_blocks_synced_total
- cleanode_tip_slot
- cleanode_tip_block_number
- cleanode_rollbacks_total
- cleanode_peers_connected
- cleanode_mempool_tx_count
- cleanode_mempool_bytes
- cleanode_consensus_vrf_valid_total
- cleanode_consensus_vrf_invalid_total
- cleanode_consensus_kes_valid_total
- cleanode_consensus_kes_invalid_total
- cleanode_consensus_opcert_valid_total
- cleanode_consensus_opcert_invalid_total
- cleanode_consensus_headers_validated_total
- cleanode_epoch_current
- cleanode_uptime_seconds
-/

namespace Cleanode.Monitoring.Metrics

open Cleanode.TUI.State

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
    counter "cleanode_blocks_synced_total" "Total number of blocks synced" state.blocksReceived,
    gauge "cleanode_tip_slot" "Current tip slot number" state.tipSlot,
    gauge "cleanode_tip_block_number" "Current tip block number" state.tipBlockNo,
    counter "cleanode_rollbacks_total" "Total number of chain rollbacks" state.rollbacks,
    gauge "cleanode_peers_connected" "Number of currently connected peers" peersConnected,
    gauge "cleanode_mempool_tx_count" "Number of transactions in the mempool" state.mempoolTxCount,
    gauge "cleanode_mempool_bytes" "Size of the mempool in bytes" state.mempoolBytes,
    counter "cleanode_consensus_headers_validated_total" "Total block headers validated" state.consensus.validatedHeaders,
    counter "cleanode_consensus_vrf_valid_total" "VRF proofs that verified correctly" state.consensus.vrfValid,
    counter "cleanode_consensus_vrf_invalid_total" "VRF proofs that failed verification" state.consensus.vrfInvalid,
    counter "cleanode_consensus_kes_valid_total" "KES signatures that verified correctly" state.consensus.kesValid,
    counter "cleanode_consensus_kes_invalid_total" "KES signatures that failed verification" state.consensus.kesInvalid,
    counter "cleanode_consensus_opcert_valid_total" "Operational certificates that verified" state.consensus.opCertValid,
    counter "cleanode_consensus_opcert_invalid_total" "Operational certificates that failed" state.consensus.opCertInvalid,
    gauge "cleanode_epoch_current" "Current epoch number" state.consensus.currentEpoch,
    gauge "cleanode_uptime_seconds" "Node uptime in seconds" uptimeSec
  ]

end Cleanode.Monitoring.Metrics
