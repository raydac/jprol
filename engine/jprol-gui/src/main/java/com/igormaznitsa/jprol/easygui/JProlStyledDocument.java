package com.igormaznitsa.jprol.easygui;

import static java.util.Objects.requireNonNull;

import java.util.List;
import javax.swing.event.DocumentEvent;
import javax.swing.event.UndoableEditEvent;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.SimpleAttributeSet;

public class JProlStyledDocument extends DefaultStyledDocument {
  private boolean disableAutoNotification;

  public JProlStyledDocument() {
    super();
  }

  public void insertBunch(final List<StyledText> list) {
    final boolean old = this.disableAutoNotification;
    this.disableAutoNotification = false;
    int start = this.getLength();
    int length = 0;
    try {
      for (final StyledText item : list) {
        if (item == StyledText.CLEAR) {
          start = 0;
          length = 0;
          this.remove(0, this.getLength());
        } else {
          final String text = item.getText();
          this.insertString(this.getLength(), text, item.getAttributeSet());
          length += text.length();
        }
      }
    } catch (BadLocationException ex) {
      //
    } finally {
      this.setDisableAutoNotification(old);
      this.fireListeners(start, length, DocumentEvent.EventType.INSERT);
    }
  }

  public boolean isDisableAutoNotification() {
    return this.disableAutoNotification;
  }

  public void setDisableAutoNotification(boolean value) {
    this.disableAutoNotification = value;
  }

  @Override
  protected void fireInsertUpdate(final DocumentEvent e) {
    if (!this.disableAutoNotification) {
      super.fireInsertUpdate(e);
    }
  }

  @Override
  protected void fireChangedUpdate(DocumentEvent e) {
    if (!this.disableAutoNotification) {
      super.fireChangedUpdate(e);
    }
  }

  @Override
  protected void fireRemoveUpdate(DocumentEvent e) {
    if (!this.disableAutoNotification) {
      super.fireRemoveUpdate(e);
    }
  }

  @Override
  protected void fireUndoableEditUpdate(UndoableEditEvent e) {
    if (!this.disableAutoNotification) {
      super.fireUndoableEditUpdate(e);
    }
  }

  public void fireListeners(final int off, final int length, final
  DocumentEvent.EventType eventType) {
    final DefaultDocumentEvent documentEvent = new DefaultDocumentEvent(off, length, eventType);
    documentEvent.end();
    if (eventType == DocumentEvent.EventType.CHANGE) {
      this.fireChangedUpdate(documentEvent);
    } else if (eventType == DocumentEvent.EventType.INSERT) {
      this.fireInsertUpdate(documentEvent);
    } else if (eventType == DocumentEvent.EventType.REMOVE) {
      this.fireRemoveUpdate(documentEvent);
    }
  }

  public static final class StyledText {
    public static final StyledText CLEAR = new StyledText(new SimpleAttributeSet(), "");

    private final AttributeSet attributeSet;
    private final String text;

    public StyledText(final AttributeSet attributeSet, final String text) {
      this.attributeSet = requireNonNull(attributeSet);
      this.text = requireNonNull(text);
    }

    public StyledText copyWith(final String text) {
      return new StyledText(this.attributeSet, text);
    }

    public AttributeSet getAttributeSet() {
      return this.attributeSet;
    }

    public String getText() {
      return this.text;
    }
  }
}
