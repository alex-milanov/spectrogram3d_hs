# TODO

* ~~change functions which produce either NaN or \_|\_ to produce Maybe~~
* ~~normalize audio volume before fft (use a learned amplitude max)~~
  * this is implemented but disabled; current system normalizes silence to loud white noise; todo: find out how to normalize everything *but* silence
* change display state to contain vectors instead of primitivestreams (then freqmax can be applied to whole display list each frame)
* ~~add color to the lines~~
  * use this method instead http://vis4.net/blog/posts/avoid-equidistant-hsv-colors/
* revamp state to use small, composable `Control.Wire`s
* delete modules in `Lib/` in favor of adding dependency on `gltut-framework`, which has up-to-date versions of those modules
* pull learning parameters out of code and include in DisplayConf (and future AudioConf)
* ~~windowing function on the input to FFT ... reduces popping at ends of wave chunk~~
  * hamming is implemented; might try a more advanced one
