package com.igormaznitsa.jprol.libs;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.LineEvent;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.UnsupportedAudioFileException;

public final class SoundClip implements AutoCloseable {

  private final AtomicBoolean playing = new AtomicBoolean();
  private final Clip clip;
  private final Lock lock = new ReentrantLock();

  public SoundClip(final byte[] resourceData) {
    try {
      this.clip = AudioSystem.getClip();
      final AudioInputStream audioStream =
          AudioSystem.getAudioInputStream(new ByteArrayInputStream(resourceData));
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
    this.lock.lock();
    try {
      this.stop();
      this.clip.close();
    } finally {
      this.lock.unlock();
    }
  }

  public SoundClip play() {
    this.lock.lock();
    try {
      this.stop();
      if (this.playing.compareAndSet(false, true)) {
        this.clip.setFramePosition(0);
        this.clip.start();
      }
      return this;
    } finally {
      this.lock.unlock();
    }
  }

  public SoundClip play(final int loops) {
    this.lock.lock();
    try {
      this.stop();
      if (this.playing.compareAndSet(false, true)) {
        this.clip.setFramePosition(0);
        this.clip.setLoopPoints(0, -1);
        this.clip.loop(loops);
      }
      return this;
    } finally {
      this.lock.unlock();
    }
  }

  public SoundClip stop() {
    this.lock.lock();
    try {
      if (this.playing.compareAndSet(true, false)
          && this.clip.isActive()) {
        this.clip.stop();
      }
      return this;
    } finally {
      this.lock.unlock();
    }
  }

  public boolean isPlaying() {
    return this.playing.get();
  }
}
