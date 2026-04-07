/-!
# Zstandard Decompression FFI

Wraps libzstd via C FFI for decompressing Mithril snapshots.
-/

namespace Dion.Mithril.Zstd

/-- Decompress a zstd-compressed file to an output file (streaming, handles large files) -/
@[extern "dion_zstd_decompress_file"]
opaque decompressFile (srcPath : @& String) (dstPath : @& String) : IO (Except String Unit)

/-- Decompress a zstd-compressed ByteArray in memory -/
@[extern "dion_zstd_decompress"]
opaque decompress (data : @& ByteArray) : IO (Except String ByteArray)

end Dion.Mithril.Zstd
