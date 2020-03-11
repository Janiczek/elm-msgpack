# `Janiczek/elm-msgpack`

WIP :warning:

- [x] encoders / decoders implemented
- [ ] unit tests around interesting examples (particularly 64bit integers)
- [ ] look at other implementations' test suites
- [ ] roundtrip fuzz tests (value -> encode -> decode -> is the same)
- [ ] documentation
- [ ] publish to packages.elm-lang.org
- [ ] advertise on msgpack.org
- [ ] RPC: figure out what it's about and whether it makes sense / is possible to support it in Elm
- [ ] think about adding map functions to allow encoding records / custom types etc.
- [ ] think about wrapping elm/bytes Decoder / Encoder types to our own opaque type to "wall this garden" and make it "MsgPack-only" decoders/encoders, with Bytes being only the implementation detail
