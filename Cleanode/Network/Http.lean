/-!
# HTTP Client FFI

Wraps libcurl via C FFI to provide HTTPS GET and file download capabilities.
Used by the Mithril integration for snapshot discovery and download.
-/

namespace Dion.Network.Http

/-- HTTP GET returning raw response bytes -/
@[extern "dion_http_get"]
opaque httpGet (url : @& String) : IO (Except String ByteArray)

/-- HTTP GET with Accept: application/json, returning response as String -/
@[extern "dion_http_get_json"]
opaque httpGetJson (url : @& String) : IO (Except String String)

/-- Download a URL to a local file path -/
@[extern "dion_http_download"]
opaque httpDownload (url : @& String) (path : @& String) : IO (Except String Unit)

end Dion.Network.Http
