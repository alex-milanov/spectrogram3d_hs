# spectrogram3d_hs

A realtime 3D spectrogram written in Haskell. Takes sound from JACK, and draws a mesh with GPipe.

There are lots of dependencies. I'll write up a guide through the cabal hell soon. See alse [TODO.md](https://github.com/plredmond/spectogram3d_hs/blob/master/TODO.md).

*Please excuse the misspelling in this repo's title/url.*

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

Before writing this Haskell spectrogram, I wrote [one in python](https://github.com/plredmond/spectrogrampy). Here are two screenshots of the animated spectrogram produced by this program when run on the same two sounds that appear the [python program's readme](https://github.com/plredmond/spectrogrampy/blob/master/README.md), for comparison.

#### Sine at 1250hz and 650hz sampled at 10kHz for 1 second

[mel.wav](https://raw.github.com/plredmond/spectrogrampy/master/examples/mel.wav)  
![mel spectrogram3d_hs](https://raw.github.com/plredmond/spectogram3d_hs/master/examples/mel.png)

The round blips are where VLC clicked while restarting the 1-second audio file. I suppose the rest are harmonics?

#### Bird call (source: Wikipedia)

[Parus major 15mars2011 [wikipedia]](http://en.wikipedia.org/wiki/File:Parus_major_15mars2011.ogg)  
![bird spectrogram3d_hs](https://raw.github.com/plredmond/spectogram3d_hs/master/examples/bird.png)

This is the tail-end of the last call made by this bird. The animated version is better.

Enjoy!

-- [PLR](http://f06mote.com)

---

[deps..]
