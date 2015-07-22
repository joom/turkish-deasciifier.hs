# turkish-deasciifier.hs [![Build Status](https://secure.travis-ci.org/joom/turkish-deasciifier.hs.svg)](http://travis-ci.org/joom/turkish-deasciifier.hs)
Haskell port of [Deniz Yuret's Turkish deasciifier](https://github.com/emres/turkish-mode). Translated from the the [Java](https://github.com/ahmetalpbalkan/turkish-deasciifier-java) and [JavaScript](https://github.com/f/deasciifier) ports.

## Installation

```
cabal install turkish-deasciifier
```

## Usage

As a library:

```haskell
import Language.TurkishDeasciifier

main :: IO ()
main = putStrLn (deasciify "Pijamali hasta yagiz sofore cabucak guvendi.")

-- prints "Pijamalı hasta yağız şoföre çabucak güvendi."
```

As an executable:

```
$ echo "Pijamali hasta yagiz sofore cabucak guvendi." | turkish-deasciifier

Pijamalı hasta yağız şoföre çabucak güvendi.
```

## License

MIT
