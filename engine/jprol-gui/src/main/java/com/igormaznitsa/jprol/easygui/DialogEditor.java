/*
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.igormaznitsa.jprol.easygui;

import java.awt.Color;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.IOException;
import java.io.PipedReader;
import java.io.PipedWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.prefs.Preferences;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleConstants.CharacterConstants;

/**
 * The class implements the Dialog editor for the IDE because it is a very
 * specialized auxiliary class, it is not described very precisely
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class DialogEditor extends AbstractProlEditor
    implements KeyListener, FocusListener, Runnable, EditorPane.EventReplacer {

  private static final long serialVersionUID = 5005224218702033782L;
  private final NonClosableReader inputReader;
  private final NonClosableWriter inputWriter;
  private final NonClosableWriter outputWriter;
  private final NonClosableReader outputReader;
  private final Thread dialogThread;
  private final SimpleAttributeSet consoleAttribute;
  private final SimpleAttributeSet userAttribute;
  private volatile boolean isWorking;
  private volatile boolean cancelCurrentRead;

  public DialogEditor() throws IOException {
    super("Dialog", true, false);

    ((EditorPane) this.editor).setEventReplacer(this);

    isWorking = true;

    removePropertyLink(PROPERTY_ED_FOREGROUND);
    addPropertyLink(new PropertyLink(this, "Output color", "EdOutputColor"));
    addPropertyLink(new PropertyLink(this, "Input color", "EdInputColor"));

    consoleAttribute = new SimpleAttributeSet();
    consoleAttribute.addAttribute(CharacterConstants.Foreground, Color.ORANGE);
    consoleAttribute.addAttribute(CharacterConstants.Bold, Boolean.TRUE);

    userAttribute = new SimpleAttributeSet();
    userAttribute.addAttribute(CharacterConstants.Foreground, Color.CYAN);
    userAttribute.addAttribute(CharacterConstants.Bold, Boolean.FALSE);

    setContentType("text/plain");
    editor.addKeyListener(this);
    editor.addFocusListener(this);

    outputWriter = new NonClosableWriter(false);
    outputReader = new NonClosableReader(outputWriter);

    inputWriter = new NonClosableWriter(true);
    inputReader = new NonClosableReader(inputWriter);

    setEnabled(false);

    dialogThread = new Thread(this, "prolDialogThread");
    dialogThread.setDaemon(true);
    dialogThread.start();

    setEdBackground(Color.BLUE.darker().darker().darker().darker());
    editor.setForeground(Color.WHITE);
    editor.setCaretColor(Color.YELLOW);
    editor.setFont(DEFAULT_FONT);

    ((EditorPane) this.editor).setCharacterAttributes(userAttribute, false);
  }

  @Override
  public void setEdBackground(Color val) {
    final JTextPane textPane = (JTextPane) this.editor;
    final SimpleAttributeSet background = new SimpleAttributeSet();
    StyleConstants.setBackground(background, val);
    textPane.getStyledDocument().setParagraphAttributes(0,
        textPane.getDocument().getLength(), background, false);
    consoleAttribute.addAttribute(CharacterConstants.Background, val);
    userAttribute.addAttribute(CharacterConstants.Background, val);
    super.setEdBackground(val);
    editor.revalidate();
    editor.repaint();
  }

  @Override
  public void loadPreferences(Preferences prefs) {
    final Color bgColor = extractColor(prefs, "dialogbackcolor", Color.BLUE.darker());
    final Color inColor = extractColor(prefs, "dialoginputcolor", Color.GREEN);
    final Color outColor = extractColor(prefs, "dialogoutputcolor", Color.ORANGE);
    final Color caretColor = extractColor(prefs, "dialogcaretcolor", Color.GREEN);

    if (bgColor != null) {
      setEdBackground(bgColor);
    }
    if (inColor != null) {
      setEdInputColor(inColor);
    }
    if (outColor != null) {
      setEdOutputColor(outColor);
    }
    if (caretColor != null) {
      setEdCaretColor(caretColor);
    }
    setEdWordWrap(prefs.getBoolean("dialogwordwrap", true));
    setEdFont(loadFontFromPrefs(prefs, "dialogoutputfont", DEFAULT_FONT));
  }

  @Override
  public void savePreferences(Preferences prefs) {
    prefs.putInt("dialogbackcolor", getEdBackground().getRGB());
    prefs.putInt("dialoginputcolor", getEdInputColor().getRGB());
    prefs.putInt("dialogoutputcolor", getEdOutputColor().getRGB());
    prefs.putInt("dialogcaretcolor", getEdCaretColor().getRGB());
    prefs.putBoolean("dialogwordwrap", getEdWordWrap());
    saveFontToPrefs(prefs, "dialogoutputfont", ((EditorPane) editor).getBaseFont());
  }

  public void initBeforeSession() {
    cancelCurrentRead = false;
    inputWriter.clearBuffer();
    outputWriter.clearBuffer();
  }

  public void cancelRead() {
    cancelCurrentRead = true;
    inputWriter.clearBuffer();
    outputWriter.clearBuffer();
  }

  public Color getEdOutputColor() {
    return StyleConstants.getForeground(consoleAttribute);
  }

  public void setEdOutputColor(Color color) {
    StyleConstants.setForeground(consoleAttribute, color);
  }

  public Color getEdInputColor() {
    return StyleConstants.getForeground(userAttribute);
  }

  public void setEdInputColor(Color color) {
    StyleConstants.setForeground(userAttribute, color);
  }

  @Override
  @SuppressWarnings("empty-statement")
  public void clearText() {
    UiUtils.assertSwingThread();
    super.clearText();
    // clear input cache
    inputWriter.clearBuffer();
    outputWriter.clearBuffer();
  }

  public Reader getInputReader() {
    return inputReader;
  }

  public Writer getOutputWriter() {
    return outputWriter;
  }

  public void addText(final String text) {
    try {
      final Runnable code = () -> {
        try {
          final Document document = editor.getDocument();

          final int extraLen = document.getLength() - 20000;
          if (extraLen > 0) {
            document.remove(0, extraLen);
          }

          document.insertString(document.getLength(), text, consoleAttribute);
          int textLength = document.getLength();
          ((EditorPane) editor).setCharacterAttributes(userAttribute, false);
          editor.setCaretPosition(textLength);
        } catch (BadLocationException ex) {
          ex.printStackTrace();
        }
      };

      if (SwingUtilities.isEventDispatchThread()) {
        code.run();
      } else {
        SwingUtilities.invokeLater(code);
      }
    } catch (Throwable thr) {

      throw new Error("Error ", thr);
    }
  }

  @Override
  public void keyTyped(KeyEvent e) {
    if (e == null) {
      return;
    }
    synchronized (editor.getDocument()) {
      int textLength = editor.getDocument().getLength();
      editor.setCaretPosition(textLength);

      try {
        inputWriter.append(e.getKeyChar());
      } catch (Exception ex) {
        // ignore
      }
    }
  }

  @Override
  public void keyPressed(KeyEvent e) {
    ((EditorPane) this.editor).setCharacterAttributes(userAttribute, false);
  }

  @Override
  public void keyReleased(KeyEvent e) {
  }

  @Override
  public void focusGained(FocusEvent e) {
    editor.setCaretPosition(editor.getDocument().getLength());

  }

  @Override
  public void focusLost(FocusEvent e) {
  }

  public synchronized void close() {
    if (dialogThread != null) {
      isWorking = false;
      cancelRead();
      dialogThread.interrupt();
    }
  }

  @Override
  public void run() {
    try {

      final Thread thisThread = Thread.currentThread();
      final StringBuilder builder = new StringBuilder(256);

      while (!thisThread.isInterrupted() && isWorking) {
        if (outputReader.ready()) {
          synchronized (this) {
            while (outputReader.ready()) {
              final int ch = outputReader.read();
              if (ch < 0) {
                break;
              }
              builder.append((char) ch);
            }
            addText(builder.toString());
            builder.setLength(0);
          }
        } else {
          try {
            Thread.sleep(200);
          } catch (InterruptedException ex) {
            Thread.currentThread().interrupt();
          }
        }
      }
    } catch (Exception ex) {
      ex.printStackTrace();
    }
  }

  @Override
  public boolean doesSupportTextPaste() {
    return true;
  }

  @Override
  public synchronized void pasteText() {
    try {
      final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
      final Transferable contents = clipboard.getContents(null);
      if (contents != null && contents.isDataFlavorSupported(DataFlavor.stringFlavor)) {
        final String str = (String) contents.getTransferData(DataFlavor.stringFlavor);
        if (str != null && !str.isEmpty()) {
          inputWriter.write(str.toCharArray(), 0, str.length());
        }
      }
    } catch (Exception ex) {
      ex.printStackTrace();
    }
  }

  @Override
  public void pasteEvent() {
    pasteText();
  }

  public final static class NonClosableWriter extends PipedWriter {

    private final List<Character> buffer;
    private final boolean waitEnter;
    private final AtomicInteger foundNextLineCounter = new AtomicInteger();

    public NonClosableWriter(final boolean waitEnter) {
      this.waitEnter = waitEnter;
      buffer = new ArrayList<>();
    }

    @Override
    public void write(final int c) {
      synchronized (buffer) {
        buffer.add((char) c);
        if (c == '\n') {
          if (this.waitEnter) {
            boolean bufferChanged;
            do {
              bufferChanged = false;
              for (int i = 0; i < this.buffer.size() && !bufferChanged; i++) {
                switch (this.buffer.get(i)) {
                  case KeyEvent.VK_DELETE: {
                    buffer.remove(i);
                    if (i < buffer.size()) {
                      buffer.remove(i);
                    }
                    bufferChanged = true;
                  }
                  break;
                  case KeyEvent.VK_BACK_SPACE: {
                    buffer.remove(i);
                    if (i > 0) {
                      buffer.remove(i - 1);
                    }
                    bufferChanged = true;
                  }
                  break;
                }
              }
            } while (bufferChanged && !Thread.currentThread().isInterrupted());
          }
          this.foundNextLineCounter.incrementAndGet();
        }
      }
    }

    @Override
    public void write(final char[] cbuf, final int off, final int len) throws IOException {
      int start = off;
      synchronized (buffer) {
        for (int li = 0; li < len; li++) {
          this.write(cbuf[start++]);
        }
      }
    }

    public boolean isEmpty() {
      synchronized (buffer) {
        return buffer.isEmpty();
      }
    }

    private int readChar() {
      int result = -1;
      if (!Thread.currentThread().isInterrupted()) {
        synchronized (this.buffer) {
          if (this.waitEnter && this.foundNextLineCounter.get() == 0) {
            return -1;
          } else {
            if (!this.buffer.isEmpty()) {
              result = buffer.remove(0);
              if (result == '\n') {
                this.foundNextLineCounter.decrementAndGet();
              }
            }
          }
        }
      }
      return result;
    }

    private int findFirstNonWhitespaceChar() {
      int result = -1;
      if (!Thread.currentThread().isInterrupted()) {
        synchronized (this.buffer) {
          if (!this.buffer.isEmpty()) {
            result = buffer.remove(0);
            if (result == '\n') {
              this.foundNextLineCounter.decrementAndGet();
            }
            if (Character.isISOControl(result) || Character.isWhitespace(result)) {
              result = -1;
            }
          }
        }
      }
      return result;
    }

    @Override
    public void flush() {
    }

    @Override
    public void close() {
    }

    public void clearBuffer() {
      synchronized (this.buffer) {
        this.foundNextLineCounter.set(0);
        this.buffer.clear();
      }
    }
  }

  public class NonClosableReader extends PipedReader {

    protected final NonClosableWriter src;

    protected NonClosableReader(final NonClosableWriter src) throws IOException {
      super(src);
      this.src = src;
    }

    @Override
    public void close() {
    }

    public synchronized int findFirstNonWhitespaceChar() throws IOException {
      if (src == null) {
        throw new IOException("There is not any connected source.");
      }

      final Thread thisThread = Thread.currentThread();

      while (isWorking) {
        final int chr = src.findFirstNonWhitespaceChar();

        if (thisThread.isInterrupted()) {
          return -1;
        }

        if (cancelCurrentRead) {
          cancelCurrentRead = false;
          return -1;
        }

        if (chr >= 0) {
          return chr;
        } else {
          try {
            Thread.sleep(30);
          } catch (InterruptedException ex) {
            Thread.currentThread().interrupt();
            return -1;
          }
        }
      }
      return -1;
    }


    @Override
    public synchronized int read() throws IOException {
      if (src == null) {
        throw new IOException("There is not any connected source.");
      }

      final Thread thisThread = Thread.currentThread();

      while (isWorking) {
        final int chr = src.readChar();

        if (thisThread.isInterrupted()) {
          return -1;
        }

        if (cancelCurrentRead) {
          cancelCurrentRead = false;
          return -1;
        }

        if (chr >= 0) {
          return chr;
        } else {
          try {
            Thread.sleep(30);
          } catch (InterruptedException ex) {
            Thread.currentThread().interrupt();
            return -1;
          }
        }
      }
      return -1;
    }

    @Override
    public synchronized int read(char[] cbuf, int off, int len) throws IOException {
      if (src == null) {
        throw new IOException("There is not defined any source.");
      }
      int readLen = 0;

      final Thread thisThread = Thread.currentThread();

      while (editor.isEditable() && len > 0 && isWorking) {
        int chr = src.readChar();

        if (thisThread.isInterrupted()) {
          return -1;
        }

        if (cancelCurrentRead) {
          cancelCurrentRead = false;
          return -1;
        }

        if (chr < 0) {
          try {
            Thread.sleep(10);
            continue;
          } catch (InterruptedException ex) {
            return -1;
          }
        }
        cbuf[off++] = (char) chr;
        len--;
        readLen++;
      }
      return readLen;
    }

    @Override
    public synchronized boolean ready() throws IOException {
      if (src == null) {
        throw new IOException("There is not defined any source.");
      }
      return !src.isEmpty();
    }
  }
}
