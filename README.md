# spectrogram3d_hs

A live 3D spectrogram written in Haskell. It takes sound from JACK, and draws a mesh with GPipe. You can use it visualize audio from VLC, iTunes, or even microphone input (whistling is particularly interesting to watch).

There are lots of dependencies. I'll write up a guide through the cabal hell soon. See alse [TODO.md](https://github.com/plredmond/spectogram3d_hs/blob/master/TODO.md).

## FAQts

* Structure
  1. The main spawns a thread which hooks into JACK and accumulates audio samples. Every `offset::Int` samples it generates an FFT using `stride::Int` samples as input. In the first few FFTs the stride has been initialized with zeros.
  2. The main then enters a GLUT mainloop which reads an `IORef` in its display function and accumulates FFTs. FFTs are projected into 3D using the [programmable graphics pipeline](http://www.arcsynthesis.org/gltut/).
* The FFT is a pure (no mutation) pure-Haskell (not bindings to another language) implementation from the [`arb-fft`](http://hackage.haskell.org/package/arb-fft) package written by Ian Ross [[blag]](http://www.skybluetrades.net/haskell-fft-index.html). Haskell is fast.
* The OpenGL vertex and fragment shaders are written in Haskell and compiled down to GLSL in [GPipe [1]](https://github.com/tobbebex/GPipe) [[2]](http://hackage.haskell.org/package/GPipe) written by Tobias Bexelius.
* All graphics are expressed in a pure and functional pipeline using GPipe. I still don't know any OpenGL API functions, for better or worse.

## Usage

Start it without arguments to use the default stride and offset (stride of FFT and offset between start of FFTs).

```
$ ./dist/build/spectrogram/spectrogram
Using defaults..
Stride: 512
Offset: 512
Planning..
done.

...
```

Start it with two numbers (no more, no less!) to specify stride and offset. Here we generate a higher-resolution FFT more frequently than the defaults.

```
$ ./dist/build/spectrogram/spectrogram 1024 256
Reading two ints.. ["1024","256"]
Stride: 1024
Offset: 256
Planning..
done.

...
```

The JACK server must already be running or the first thing you'll see in the output after planning will be the audio thread quitting.

The screen should display 3D axes in RGB. Once you route audio at the JACK *receive ports* of "spectrogram", the FFTs will start appearing onscreen.

![routing audio](https://raw.github.com/plredmond/spectogram3d_hs/master/examples/jackpilot.png)

The JACK *send ports* of "spectrogram" contain no information.

## Examples

Before writing this Haskell spectrogram, I wrote [one in python](https://github.com/plredmond/spectrogrampy). For comparison, here are two screenshots of the animated spectrogram produced by this program when run on the same two sounds that appear in the [python program's readme](https://github.com/plredmond/spectrogrampy/blob/master/README.md).

### Sine at 1250hz and 650hz sampled at 10kHz for 1 second

[mel.wav](https://raw.github.com/plredmond/spectrogrampy/master/examples/mel.wav)  
![mel spectrogram3d_hs](https://raw.github.com/plredmond/spectogram3d_hs/master/examples/mel.png)

The round blips are where VLC clicked while restarting the 1-second audio file. I suppose the rest are harmonics?

### Bird call (source: Wikipedia)

[Parus major 15mars2011 [wikipedia]](http://en.wikipedia.org/wiki/File:Parus_major_15mars2011.ogg)  
![bird spectrogram3d_hs](https://raw.github.com/plredmond/spectogram3d_hs/master/examples/bird.png)

This is the tail-end of the last call made by this bird. The animated version is better.

Enjoy!

-- [PLR](http://f06mote.com)

---

[deps..]
