# The problem

The standard way of generating documentation with Servant is `servant-docs`. However it doesn't always make it easy to write high quality examples.

Consider an endpoint for lowercasing text: `Capture Text :> Get Text`.

Using an example typeclass to generate examples isn't satisfying here. It could output lowercase text on both sides (in which case the example isn't very illustrative) or uppercase text as the output (in which case it's wrong).

# The solution

Provide explicit examples of input and output for each endpoint in the Haskell code, and use typeclass machinery to generate the final docs from them.

This mirrors what Servant already does for the server, where you provide a handler for each endpoint and Servant generates an application from them.

# In this repo

[./src/Goal.hs](./src/Goal.hs): The desired interface. Not working yet.

[./src/TinyServant.hs](./src/TinyServant.hs): One file implementation of part of Servant, used instead of the real thing while I experiment solutions.
