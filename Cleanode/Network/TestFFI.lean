namespace Cleanode.Network.TestFFI

@[extern "cleanode_test_simple"]
opaque test_simple (x : UInt32) : IO UInt32

end Cleanode.Network.TestFFI
