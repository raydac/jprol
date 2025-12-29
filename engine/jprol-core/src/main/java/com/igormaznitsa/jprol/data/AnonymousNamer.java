package com.igormaznitsa.jprol.data;

import java.util.concurrent.atomic.AtomicLong;

final class AnonymousNamer {

  private static final AtomicLong COUNTER = new AtomicLong(1L);

  AnonymousNamer() {

  }

  String makeName() {
    final long value = COUNTER.getAndIncrement();
    return String.valueOf(value);
  }
}
