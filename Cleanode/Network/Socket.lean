/-!
# TCP Socket Interface

This module provides a low-level TCP socket interface for Cleanode,
implemented via FFI to C for actual socket operations.

## Overview

Since Lean 4 doesn't have native TCP socket support, we use FFI to C's
socket API. This module provides:
- TCP connection establishment
- Send/receive operations
- Error handling
- Socket lifecycle management

## References
- POSIX socket API
- Ouroboros Network Spec (for protocol requirements)
-/

namespace Cleanode.Network.Socket

/-- Opaque socket handle type (external object wrapping file descriptor) -/
opaque SocketPointed : NonemptyType
def Socket : Type := SocketPointed.type
instance : Nonempty Socket := SocketPointed.property

/-- Socket error type -/
inductive SocketError where
  | ConnectionFailed (reason : String)
  | SendFailed (reason : String)
  | ReceiveFailed (reason : String)
  | Timeout
  | Closed
  deriving Repr, BEq

instance : ToString SocketError where
  toString
    | .ConnectionFailed r => s!"Connection failed: {r}"
    | .SendFailed r => s!"Send failed: {r}"
    | .ReceiveFailed r => s!"Receive failed: {r}"
    | .Timeout => "Socket timeout"
    | .Closed => "Socket closed"

/-- Connect to a remote host -/
@[extern "cleanode_socket_connect"]
opaque socket_connect (host : @& String) (port : @& UInt16) : IO (Except SocketError Socket)

/-- Send bytes over socket -/
@[extern "cleanode_socket_send"]
opaque socket_send (sock : @& Socket) (data : @& ByteArray) : IO (Except SocketError Unit)

/-- Receive up to maxBytes from socket (single recv, may return fewer) -/
@[extern "cleanode_socket_receive"]
opaque socket_receive (sock : @& Socket) (maxBytes : @& UInt32) : IO (Except SocketError ByteArray)

/-- Receive exactly numBytes from socket (loops until all received) -/
@[extern "cleanode_socket_receive_exact"]
opaque socket_receive_exact (sock : @& Socket) (numBytes : @& UInt32) : IO (Except SocketError ByteArray)

/-- Close socket -/
@[extern "cleanode_socket_close"]
opaque socket_close (sock : @& Socket) : IO Unit

/-- Resolve hostname to all IP addresses via DNS -/
@[extern "cleanode_dns_resolve"]
opaque dns_resolve (host : @& String) : IO (Array String)

/-- High-level connection helper -/
def connect (host : String) (port : UInt16) : IO (Except SocketError Socket) := do
  IO.println s!"Connecting to {host}:{port}..."
  socket_connect host port

/-- Send data with error handling -/
def send (sock : Socket) (data : ByteArray) : IO (Except SocketError Unit) := do
  socket_send sock data

/-- Receive data with error handling -/
def receive (sock : Socket) (maxBytes : UInt32 := 4096) : IO (Except SocketError ByteArray) := do
  socket_receive sock maxBytes

/-- Close connection -/
def close (sock : Socket) : IO Unit := do
  socket_close sock

end Cleanode.Network.Socket
