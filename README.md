# Biter

This was written in an afternoon of painfully debugging `webassembly <-> JS <-> Haskell`
FFI layers.

The point of the tool is to allow rapid debugging and experimentation with
bit encodings of:

- [ ] ASCII values
- [ ] UTF-8 values
- [x] Signed and unsigned integers of arbitrary bit width
- [ ] IEEE floating and double precision.


Currently, there's a single haskell module `Bytes` which can be used from
the haskell interpreter `ghci`. I'm as of yet not sure what the corrent design
for a CLI is so I'm holding off on that.


##### Trivia

The name comes from the hobbit, where the orcs refer to the ancient sword
`Orcrist` as `Biter`.
