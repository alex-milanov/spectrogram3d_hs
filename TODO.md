# TODO

* wrap NaN-producing and \_|\_-producing functions with Maybe-producing alternatives
* normalize audio volume before fft (use a learned amplitude max)
* change display state to contain vectors instead of primitivestreams (then freqmax can be applied to whole display list each frame)
* add color to the lines http://vis4.net/blog/posts/avoid-equidistant-hsv-colors/
* revamp state to use small, composable `Control.Wire`s
* windowing function on the input to FFT ... reduces popping at ends of wave chunk
* depend on gltut-framework instead of duplicating those modules in Lib/
