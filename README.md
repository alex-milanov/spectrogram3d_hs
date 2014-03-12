# spectogram3d_hs

A realtime 3D spectrogram written in Haskell. Takes sound from JACK, and draws a mesh with GPipe.

* There are lots of dependencies. I'll write up a guide through cabal hell soon.
* I'll clean up the code with `Control.Wire` soon hopefully too.

## usage

Start it without arguments to use the default stride and offset (stride of FFT and offset between start of FFTs).

```
$ ./dist/build/waveform/waveform
Using defaults..
Stride: 512
Offset: 512
Planning..
done.

...
```

Start it with two numbers to specify stride and offset. Here we generate a higher-resolution FFT more frequently than the defaults.

```
$ ./dist/build/waveform/waveform 1024 256
Reading two ints.. ["1024","256"]
Stride: 1024
Offset: 256
Planning..
done.

...
```

The JACK server must already be running or the first thing you'll see in the output after planning will be the audio thread quitting.

```
audio monitor thread finished
```

The screen should display 3D axes in RGB. Once you route audio at the JACK *receive ports* of "waveform", the FFTs will start appearing onscreen. The JACK *send ports* of "waveform" contain no information. I should probably rename the executable to something else.

Enjoy!

-- [PLR](http://f06mote.com)

---

[deps..]
