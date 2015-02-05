Servant Route
=============

This package is a server implementation for [Servant][0] APIs using a more
traditional routing semantics than the existing back-tracking implementation.

[0]: https://github.com/haskell-servant/

This difference in semantics is quite simple -- Servant's existing `HasServer`
mechanism conflates two different concerns: routing a request to the
appropriate handler and extracting information from the request to invoke the
handler. The former should use only information which identifies an HTTP
resource while the latter can use any and all request properties. This
conflation makes it difficult to implement Servant APIs which produce sensible
error responses: a missing `Header`, for example, forces back-tracking and the
client sees an error from some subsequent handler instead of one that will
describes the actual problem with their request.

To resolve this problem we propose two phases: routing and dispatching. The
routing process traverses an API type and inspects the request to determine
whether or not the corresponding handler should be called while dispatching
extracts information from the request and calls the handler. With this
separation, routing is free to ignore some API type components which do not
change the HTTP resource denoted by the request.
