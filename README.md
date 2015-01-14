# Hocker

(Yet another) docker configuration manager provider tool thingy.

Hocker was primarily developed to acquaint myself to Haskell and only
secondary to provide value. Most of the time it would be wiser to
use e.g. [fig](https://github.com/docker/fig).


## Installation

### Requirements

 - Haskell (Platform or cabal + ghc)

On OS X, you may `brew cask install haskell-platform` for the carefree package,
or try <https://www.haskell.org/platform/>. Otherwise, install
[cabal](https://www.haskell.org/cabal/) and [ghc](https://www.haskell.org/ghc/)

### Installation

1. Clone the source

    ```sh
    git clone git@github.com:knutwalker/hocker.git
    cd hocker
    ```

2. Build from source

    ```sh
    make install
    ```

    This will install a `hocker` binary to `$HOME/bin/hocker`.

    If you wish to change the path, use something like `make install PREFIX="usr/local"`.

3. enjoy

### Usage

TODO... (try `hocker` for some help)
