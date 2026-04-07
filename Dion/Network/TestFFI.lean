namespace Dion.Network.TestFFI

@[extern "dion_test_simple"]
opaque test_simple (x : UInt32) : IO UInt32

end Dion.Network.TestFFI
