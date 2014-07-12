#!/usr/bin/env bash

mkdir spectrogram
cd spectrogram

git clone https://github.com/tobbebex/GPipe.git
git clone https://github.com/plredmond/spectrogram3d_hs.git

cabal sandbox init --sandbox pkg
cabal sandbox add-source GPipe
cabal sandbox add-source spectrogram3d_hs

pushd spectrogram3d_hs
  cabal sandbox init --sandbox ../pkg
  cabal install
popd
