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
package com.igormaznitsa.prol.easygui;

import java.awt.Color;
import java.awt.Font;
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
import java.util.LinkedList;
import java.util.prefs.Preferences;
import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleConstants.CharacterConstants;

/**
 * The class implements the Dialog editor for the IDE because it is a very
 * specialized auxiliary class, it is not described very precisely
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class DialogEditor extends AbstractProlEditor implements KeyListener, FocusListener, Runnable, EditorPane.EventReplacer {
  private static final long serialVersionUID = 5005224218702033782L;

  public class NonClossableReader extends PipedReader {

    protected final NonClossableWriter src;

    protected NonClossableReader(final NonClossableWriter src) throws IOException {
      super(src);
      this.src = src;
    }

    @Override
    public void close() {
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
        }
        else {
          try {
            Thread.sleep(10);
          }
          catch (InterruptedException ex) {
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
          }
          catch (InterruptedException ex) {
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

  public final static class NonClossableWriter extends PipedWriter {

    protected final LinkedList<Character> buffer;

    public NonClossableWriter() {
      buffer = new LinkedList<Character>();
    }

    @Override
    public void write(final int c) throws IOException {
      synchronized (buffer) {
        buffer.add((char) c);
      }
    }

    @Override
    public void write(final char[] cbuf, final int off, final int len) throws IOException {
      int start = off;
      final Thread thisThread = Thread.currentThread();
      synchronized (buffer) {
        for (int li = 0; li < len; li++) {
          if (thisThread.isInterrupted()) {
            return;
          }
          buffer.add(cbuf[start++]);
        }
      }
    }

    public boolean isEmpty() {
      synchronized (buffer) {
        return buffer.isEmpty();
      }
    }

    protected int readChar() {
      synchronized (buffer) {
        if (buffer.isEmpty()) {
          return -1;
        }

        final Thread thisThread = Thread.currentThread();

        char first = buffer.removeFirst();
        if (thisThread.isInterrupted()) {
          return -1;
        }
        return first;
      }
    }

    @Override
    public void flush() {
    }

    @Override
    public void close() {
    }

    public void clearBuffer() {
      synchronized (buffer) {
        buffer.clear();
      }
    }
  }
  private final NonClossableReader inputReader;
  private final NonClossableWriter inputWriter;
  private final NonClossableWriter outsideWriter;
  private final NonClossableReader outsideReader;
  private final Thread dialogThread;
  private volatile boolean isWorking;
  private volatile boolean cancelCurrentRead;
  private final SimpleAttributeSet consoleAttribute;
  private final SimpleAttributeSet userAttribute;

  @Override
  public void loadPreferences(Preferences prefs) {
    setEdBackground(new Color(prefs.getInt("dialogbackcolor", 0x7908AA  )));
    setEdInputColor(new Color(prefs.getInt("dialoginputcolor", 0x34D800)));
    setEdOutputColor(new Color(prefs.getInt("dialogoutputcolor", 0xF3FD00)));
    setEdCaretColor(new Color(prefs.getInt("dialogcaretcolor", 0xF30021)));
    setEdWordWrap(prefs.getBoolean("dialogwordwrap", true));
    setEdFont(loadFontFromPrefs(prefs, "dialogoutputfont"));
  }

  @Override
  public void savePreferences(Preferences prefs) {
    prefs.putInt("dialogbackcolor", getEdBackground().getRGB());
    prefs.putInt("dialoginputcolor", getEdInputColor().getRGB());
    prefs.putInt("dialogoutputcolor", getEdOutputColor().getRGB());
    prefs.putInt("dialogcaretcolor", getEdCaretColor().getRGB());
    prefs.putBoolean("dialogwordwrap", getEdWordWrap());
    saveFontToPrefs(prefs, "dialogoutputfont", editor.getFont());
  }

  public synchronized void initBeforeSession() {
    cancelCurrentRead = false;
    inputWriter.clearBuffer();
    outsideWriter.clearBuffer();
  }

  public synchronized void cancelRead() {
    cancelCurrentRead = true;
    inputWriter.clearBuffer();
    outsideWriter.clearBuffer();
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
  public synchronized void clearText() {
    super.clearText();
    // clear input cache
    inputWriter.clearBuffer();
    outsideWriter.clearBuffer();
  }

  public Reader getInputReader() {
    return inputReader;
  }

  public Writer getOutputWriter() {
    return outsideWriter;
  }

  public synchronized void addText(final String text) {
    try {
      SwingUtilities.invokeAndWait(new Runnable() {

        @Override
        public void run() {
          try {
            synchronized (editor.getDocument()) {
              editor.getDocument().insertString(editor.getDocument().getLength(), text, consoleAttribute);
              int textLength = editor.getDocument().getLength();
              editor.setCharacterAttributes(userAttribute, false);
              editor.setCaretPosition(textLength);
            }
          }
          catch (BadLocationException ex) {
            ex.printStackTrace();
          }
        }
      });
    }
    catch (Throwable thr) {
      if (thr instanceof InterruptedException) {
        Thread.currentThread().interrupt();
      }

      throw new Error("Error ", thr);
    }
  }

  public DialogEditor() throws IOException {
    super("Dialog");

    editor.setEventReplacer(this);

    isWorking = true;

    removePropertyFromList("EdForeground");
    addPropertyToList(new PropertyLink(this, "Output color", "EdOutputColor"));
    addPropertyToList(new PropertyLink(this, "Input color", "EdInputColor"));

    consoleAttribute = new SimpleAttributeSet();
    consoleAttribute.addAttribute(CharacterConstants.Foreground, Color.ORANGE);
    consoleAttribute.addAttribute(CharacterConstants.Bold, Boolean.TRUE);

    userAttribute = new SimpleAttributeSet();
    userAttribute.addAttribute(CharacterConstants.Foreground, Color.CYAN);
    userAttribute.addAttribute(CharacterConstants.Bold, Boolean.FALSE);

    editor.setContentType("text/plain");
    editor.addKeyListener(this);
    editor.addFocusListener(this);

    outsideWriter = new NonClossableWriter();
    outsideReader = new NonClossableReader(outsideWriter);

    inputWriter = new NonClossableWriter();
    inputReader = new NonClossableReader(inputWriter);

    setEnabled(false);

    dialogThread = new Thread(this, "prolDialogThread");
    dialogThread.setDaemon(true);
    dialogThread.start();

    editor.setBackground(Color.BLUE.darker().darker().darker().darker());
    editor.setForeground(Color.WHITE);
    editor.setCaretColor(Color.YELLOW);
    editor.setFont(new Font("Arial", Font.BOLD, 14));

    editor.setCharacterAttributes(userAttribute, false);
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
      }
      catch (Exception ex) {
      }
    }
  }

  @Override
  public void keyPressed(KeyEvent e) {
    editor.setCharacterAttributes(userAttribute, false);
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
        if (outsideReader.ready()) {
          synchronized (this) {
            while (outsideReader.ready()) {
              final int ch = outsideReader.read();
              if (ch < 0) {
                break;
              }
              builder.append((char) ch);
            }
            addText(builder.toString());
            builder.setLength(0);
          }
        }
        else {
          try{
            Thread.sleep(200);
          }catch(InterruptedException ex){
            
          }
        }
      }
    }
    catch (Exception ex) {
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
    }
    catch (Exception ex) {
      ex.printStackTrace();
    }
  }

  @Override
  public void pasteEvent() {
    pasteText();
  }
}
