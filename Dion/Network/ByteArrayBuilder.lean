/-!
# ByteArray Builder

Accumulates ByteArray chunks and concatenates them in a single pass.
This avoids the O(n²) cost of repeated `ByteArray.append` in foldl loops.

Usage:
  let b := Builder.empty
  let b := b.append chunk1
  let b := b.append chunk2
  b.toByteArray  -- single O(n) concatenation
-/

namespace Dion.Network.ByteArrayBuilder

/-- A builder that collects ByteArray chunks for efficient concatenation. -/
structure Builder where
  chunks : List ByteArray  -- Chunks in reverse order (newest first)
  totalSize : Nat

/-- Empty builder -/
def Builder.empty : Builder := { chunks := [], totalSize := 0 }

/-- Append a chunk -/
@[inline] def Builder.append (b : Builder) (bs : ByteArray) : Builder :=
  { chunks := bs :: b.chunks, totalSize := b.totalSize + bs.size }

/-- Concatenate all chunks into a single ByteArray -/
def Builder.toByteArray (b : Builder) : ByteArray :=
  let result := ByteArray.emptyWithCapacity b.totalSize
  -- Chunks are in reverse order, so reverse before copying
  b.chunks.reverse.foldl (fun acc chunk => acc ++ chunk) result

/-- Convenience: fold a list through an encoder into a builder -/
def Builder.foldAppend {α : Type} (items : List α) (encode : α → ByteArray) : Builder :=
  items.foldl (fun b item => b.append (encode item)) Builder.empty

/-- Convenience: fold and produce final ByteArray -/
def foldEncode {α : Type} (items : List α) (encode : α → ByteArray) : ByteArray :=
  (Builder.foldAppend items encode).toByteArray

end Dion.Network.ByteArrayBuilder
