package com.igormaznitsa.jprol.easygui.guilibs;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.LineEvent;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.UnsupportedAudioFileException;

public final class SoundClip implements AutoCloseable {

  private final AtomicBoolean playing = new AtomicBoolean();
  private final Clip clip;
  private final Semaphore lock = new Semaphore(1);
  private final AtomicBoolean closed = new AtomicBoolean();
  private final AudioInputStream audioStream;

  public SoundClip(final byte[] resourceData) {
    try {
      this.clip = AudioSystem.getClip();
      this.audioStream = AudioSystem.getAudioInputStream(new ByteArrayInputStream(resourceData));
      this.clip.addLineListener(event -> {
        if (event.getType() == LineEvent.Type.STOP) {
          playing.set(false);
        }
      });
      this.clip.open(audioStream);
    } catch (UnsupportedAudioFileException | IOException | LineUnavailableException ex) {
      throw new RuntimeException("Error during load and init sound clip", ex);
    }
  }

  @Override
  public void close() {
    if (this.closed.get()) {
      return;
    }
    this.lock.acquireUninterruptibly();
    try {
      if (this.closed.compareAndSet(false, true)) {
        try {
          try {
            if (this.playing.compareAndSet(true, false)
                && this.clip.isActive()) {
              this.clip.stop();
              this.clip.flush();
            }
          } finally {
            this.clip.close();
          }
        } finally {
          try {
            this.audioStream.close();
          } catch (Exception ex) {
            // ignore
          }
        }
      }
    } finally {
      this.lock.release();
    }
  }

  public SoundClip play() {
    if (this.closed.get()) {
      throw new IllegalStateException("Closed");
    }
    this.lock.acquireUninterruptibly();
    try {
      if (!this.closed.get()) {
        if (this.playing.compareAndSet(true, false)
            && this.clip.isActive()) {
          this.clip.stop();
          this.clip.flush();
        }
        if (this.playing.compareAndSet(false, true)) {
          this.clip.setFramePosition(0);
          this.clip.start();
        }
      }
      return this;
    } finally {
      this.lock.release();
    }
  }

  public SoundClip play(final int loops) {
    if (this.closed.get()) {
      throw new IllegalStateException("Closed");
    }
    this.lock.acquireUninterruptibly();
    try {
      if (!this.closed.get()) {
        if (this.playing.compareAndSet(true, false)
            && this.clip.isActive()) {
          this.clip.stop();
          this.clip.flush();
        }
        if (this.playing.compareAndSet(false, true)) {
          this.clip.setFramePosition(0);
          this.clip.setLoopPoints(0, -1);
          this.clip.loop(loops);
        }
      }
      return this;
    } finally {
      this.lock.release();
    }
  }

  public SoundClip stop() {
    if (this.closed.get()) {
      throw new IllegalStateException("Closed");
    }
    this.lock.acquireUninterruptibly();
    try {
      if (!this.closed.get()) {
        if (this.playing.compareAndSet(true, false)
            && this.clip.isActive()) {
          this.clip.stop();
          this.clip.setFramePosition(0);
        }
      }
      return this;
    } finally {
      this.lock.release();
    }
  }

  public boolean isPlaying() {
    if (this.closed.get()) {
      return false;
    }
    return this.playing.get();
  }
}
