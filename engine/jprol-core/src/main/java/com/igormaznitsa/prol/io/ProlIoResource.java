package com.igormaznitsa.prol.io;

import java.io.Closeable;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import static com.igormaznitsa.prol.utils.Utils.assertNotNull;

public abstract class ProlIoResource<T extends Closeable> {

  protected final String id;
  protected final T resource;
  protected final List<ProlResourceCloseListener> closeListeners = new CopyOnWriteArrayList<>();

  protected ProlIoResource(String id, T resource) {
    this.id = assertNotNull(id, "Resource Id can't be null");
    this.resource = assertNotNull(resource, "Resource must not be null");
  }

  public void addProlResourceCloseListener(final ProlResourceCloseListener l) {
    this.closeListeners.add(l);
  }

  public void removeProlResourceCloseListener(final ProlResourceCloseListener l) {
    this.closeListeners.remove(l);
  }

  public String getId() {
    return this.id;
  }

  public void close() throws IOException {
    try {
      this.resource.close();
    } finally {
      this.closeListeners.forEach(l -> {
        l.onProlResourceClosed(this);
      });
    }
  }

  public interface ProlResourceCloseListener {
    void onProlResourceClosed(ProlIoResource<? extends Closeable> closedResource);
  }
}
