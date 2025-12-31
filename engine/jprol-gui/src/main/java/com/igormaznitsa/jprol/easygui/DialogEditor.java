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

import static com.igormaznitsa.jprol.easygui.DialogEditor.InputSource.SYSTEM;
import static com.igormaznitsa.jprol.easygui.DialogEditor.InputSource.USER;
import static com.igormaznitsa.jprol.easygui.JProlStyledDocument.StyledText.CLEAR;
import static com.igormaznitsa.jprol.easygui.UiUtils.assertSwingThread;

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
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;
import java.util.prefs.Preferences;
import javax.swing.JTextPane;
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
    implements KeyListener, FocusListener, ScalableEditorPane.EventReplacer {
  private final SimpleAttributeSet consoleAttribute;
  private final SimpleAttributeSet userAttribute;

  private final BlockingQueue<Integer> inputQueue = new ArrayBlockingQueue<>(16384);
  private final Semaphore readSemaphore = new Semaphore(1);
  private final Reader inputReader = new Reader() {
    @Override
    public int read(final char[] cbuf, final int off, final int len) throws IOException {
      try {
        readSemaphore.acquire();
        int readCounter = 0;
        int index = off;
        while (readCounter < len) {
          final Integer next = inputQueue.poll(100, TimeUnit.MILLISECONDS);
          if (next == null) {
            break;
          }
          final int nextInt = next;
          if (nextInt < 0) {
            break;
          }
          cbuf[index++] = (char) nextInt;
          readCounter++;
        }
        return readCounter;
      } catch (InterruptedException ex) {
        Thread.currentThread().interrupt();
        throw new IOException("Interrupted thread");
      } finally {
        readSemaphore.release();
      }
    }

    @Override
    public void close() throws IOException {

    }
  };

  @Override
  public void savePreferences(Preferences prefs) {
    prefs.putInt("dialogbackcolor", getEdBackground().getRGB());
    prefs.putInt("dialoginputcolor", getEdInputColor().getRGB());
    prefs.putInt("dialogoutputcolor", getEdOutputColor().getRGB());
    prefs.putInt("dialogcaretcolor", getEdCaretColor().getRGB());
    prefs.putBoolean("dialogwordwrap", getEdWordWrap());
    saveFontToPrefs(prefs, "dialogoutputfont", ((ScalableEditorPane) editor).getBaseFont());
  }
  private final Queue<JProlStyledDocument.StyledText> writeQueue = new ConcurrentLinkedQueue<>();

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
  private final Writer outputWriter = new Writer() {
    @Override
    public void write(final char[] buffer, final int off, final int len) throws IOException {
      addText(new String(buffer, off, len), SYSTEM);
    }

    @Override
    public void flush() throws IOException {
      // ignore
    }

    @Override
    public void close() throws IOException {
      // ignore
    }
  };

  public DialogEditor() throws IOException {
    super("Dialog", true, false);

    ((ScalableEditorPane) this.editor).setEventReplacer(this);

    removePropertyLink(PROPERTY_ED_FOREGROUND);
    addPropertyLink(new PropertyLink(this, "Output color", "EdOutputColor"));
    addPropertyLink(new PropertyLink(this, "Input color", "EdInputColor"));

    this.consoleAttribute = new SimpleAttributeSet();
    this.consoleAttribute.addAttribute(CharacterConstants.Foreground, Color.ORANGE);
    this.consoleAttribute.addAttribute(CharacterConstants.Bold, Boolean.TRUE);

    this.userAttribute = new SimpleAttributeSet();
    this.userAttribute.addAttribute(CharacterConstants.Foreground, Color.CYAN);
    this.userAttribute.addAttribute(CharacterConstants.Bold, Boolean.FALSE);

    this.setContentType("text/plain");
    this.editor.addKeyListener(this);
    this.editor.addFocusListener(this);

    this.setEnabled(false);

    this.setEdBackground(Color.BLUE.darker().darker().darker().darker());
    this.editor.setForeground(Color.WHITE);
    this.editor.setCaretColor(Color.YELLOW);
    this.editor.setFont(DEFAULT_FONT);

    ((ScalableEditorPane) this.editor).setCharacterAttributes(this.userAttribute, false);
  }

  @Override
  public void setEdBackground(final Color color) {
    final JTextPane textPane = (JTextPane) this.editor;

    final SimpleAttributeSet background = new SimpleAttributeSet();
    StyleConstants.setBackground(background, color);
    textPane.getStyledDocument()
        .setParagraphAttributes(0, textPane.getDocument().getLength(), background, false);

    this.consoleAttribute.addAttribute(CharacterConstants.Background, color);
    this.userAttribute.addAttribute(CharacterConstants.Background, color);

    super.setEdBackground(color);
    this.editor.revalidate();
    this.editor.repaint();
  }

  @Override
  public void loadPreferences(Preferences prefs) {
    final Color bgColor = extractColor(prefs, "dialogbackcolor", Color.BLUE.darker());
    final Color inColor = extractColor(prefs, "dialoginputcolor", Color.GREEN);
    final Color outColor = extractColor(prefs, "dialogoutputcolor", Color.ORANGE);
    final Color caretColor = extractColor(prefs, "dialogcaretcolor", Color.GREEN);

    if (bgColor != null) {
      this.setEdBackground(bgColor);
    }
    if (inColor != null) {
      this.setEdInputColor(inColor);
    }
    if (outColor != null) {
      this.setEdOutputColor(outColor);
    }
    if (caretColor != null) {
      this.setEdCaretColor(caretColor);
    }
    this.setEdWordWrap(prefs.getBoolean("dialogwordwrap", true));
    this.setEdFont(loadFontFromPrefs(prefs, "dialogoutputfont", DEFAULT_FONT));
  }

  public void onStartNewSession() {
    this.writeQueue.clear();
    this.writeQueue.add(CLEAR);
  }

  @Override
  @SuppressWarnings("empty-statement")
  public void clearText() {
    assertSwingThread();
    super.clearText();
  }

  public Reader getInput() {
    return this.inputReader;
  }

  public Writer getOutput() {
    return this.outputWriter;
  }

  public String blockingRead(final Predicate<Character> charPredicate, final boolean tillEol) {
    try {
      this.readSemaphore.acquire();
      final StringBuilder builder = new StringBuilder();
      while (!Thread.currentThread().isInterrupted()) {
        final Integer code = this.inputQueue.poll(100, TimeUnit.MILLISECONDS);
        if (code == null) {
          continue;
        }
        final char ch = (char) code.intValue();
        if (tillEol && ch == '\n') {
          break;
        }

        if (charPredicate.test(ch)) {
          builder.append(ch);
          if (!tillEol) {
            break;
          }
        }
      }
      return builder.toString();
    } catch (InterruptedException ex) {
      return null;
    } finally {
      this.readSemaphore.release();
    }
  }

  public void interruptBlockedOperations() {
    this.readSemaphore.release();
  }

  public void onTimer() {
    assertSwingThread();

    final JProlStyledDocument document = (JProlStyledDocument) this.editor.getDocument();
    List<JProlStyledDocument.StyledText> found = null;
    try {
      int counter = 0;
      while (counter < 100 && !Thread.currentThread().isInterrupted()) {
        final JProlStyledDocument.StyledText next = this.writeQueue.poll();
        if (next == null) {
          break;
        }
        counter--;

        if (found == null) {
          found = new ArrayList<>();
        }
        found.add(next);
      }
    } finally {
      if (found != null && !found.isEmpty()) {
        document.insertBunch(found);
        editor.setCaretPosition(document.getLength());
      }
    }
  }

  private void addText(final String text, final InputSource inputSource) {
    if (text != null) {
      switch (inputSource) {
        case SYSTEM:
          this.writeQueue.add(
              new JProlStyledDocument.StyledText(this.consoleAttribute, text));
          break;
        case USER:
          this.writeQueue.add(
              new JProlStyledDocument.StyledText(this.userAttribute, text));
          break;
      }
    }
  }

  public void addSystemText(final String text) {
    this.addText(text, SYSTEM);
  }

  @Override
  public void keyTyped(KeyEvent e) {
    if (!e.isConsumed()) {
      e.consume();
      final char keyChar = e.getKeyChar();
      this.inputQueue.add((int) keyChar);
      this.addText(String.valueOf(keyChar), USER);
    }
  }

  @Override
  public void keyPressed(KeyEvent e) {

  }

  @Override
  public void focusGained(FocusEvent e) {
    this.editor.setCaretPosition(editor.getDocument().getLength());
  }

  public void close() {
    this.writeQueue.clear();
    this.interruptBlockedOperations();
  }

  @Override
  public void keyReleased(KeyEvent e) {
  }

  @Override
  public void pasteText() {
    try {
      final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
      final Transferable contents = clipboard.getContents(null);
      if (contents != null && contents.isDataFlavorSupported(DataFlavor.stringFlavor)) {
        final String str = (String) contents.getTransferData(DataFlavor.stringFlavor);
        if (str != null && !str.isEmpty()) {
          this.addText(str, USER);
        }
      }
    } catch (Exception ex) {
      ex.printStackTrace();
    }
  }

  @Override
  public void focusLost(FocusEvent e) {
  }

  private enum OutputType {
    CLEAR, ADDED, TYPED
  }

  @Override
  public boolean doesSupportTextPaste() {
    return true;
  }

  public enum InputSource {
    SYSTEM,
    USER
  }

  @Override
  public void pasteEvent() {
    pasteText();
  }

}
