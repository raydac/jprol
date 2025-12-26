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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.prefs.Preferences;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

/**
 * The class implements the trace pane for the IDE because it is a very
 * specialized auxiliary class, it is not described very precisely
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class TraceDialog extends AbstractProlEditor implements ActionListener {
  private static final SimpleAttributeSet ATTRSET_OTHER = new SimpleAttributeSet();
  private static final SimpleAttributeSet ATTRSET_CALL = new SimpleAttributeSet();
  private static final SimpleAttributeSet ATTRSET_REDO = new SimpleAttributeSet();
  private static final SimpleAttributeSet ATTRSET_FAIL = new SimpleAttributeSet();
  private static final SimpleAttributeSet ATTRSET_EXIT = new SimpleAttributeSet();
  private final Queue<TextMessage> messageQueue = new ConcurrentLinkedQueue<>();

  public TraceDialog() {
    super("Trace", false, false);

    this.editor.setEditable(false);

    removePropertyLink("EdForeground");
    removePropertyLink("EdCaretColor");

    addPropertyLink(new PropertyLink(this, "Call color", "EdCallColor"));
    addPropertyLink(new PropertyLink(this, "Redo color", "EdRedoColor"));
    addPropertyLink(new PropertyLink(this, "Fail color", "EdFailColor"));
    addPropertyLink(new PropertyLink(this, "Exit color", "EdExitColor"));
    addPropertyLink(new PropertyLink(this, "Other color", "EdOtherColor"));

    setEnabled(false);
    setContentType("text/rtf");

    editor.setBackground(Color.BLUE.darker().darker().darker().darker());
    editor.setForeground(Color.WHITE);
  }

  public Color getEdOtherColor() {
    return StyleConstants.getForeground(ATTRSET_OTHER);
  }

  public void setEdOtherColor(final Color color) {
    clearText();
    StyleConstants.setForeground(ATTRSET_OTHER, color);
  }

  public Color getEdCallColor() {
    return StyleConstants.getForeground(ATTRSET_CALL);
  }

  public void setEdCallColor(final Color color) {
    clearText();
    StyleConstants.setForeground(ATTRSET_CALL, color);
  }

  public Color getEdRedoColor() {
    return StyleConstants.getForeground(ATTRSET_REDO);
  }

  public void setEdRedoColor(final Color color) {
    clearText();
    StyleConstants.setForeground(ATTRSET_REDO, color);
  }

  public Color getEdExitColor() {
    return StyleConstants.getForeground(ATTRSET_EXIT);
  }

  public void setEdExitColor(final Color color) {
    clearText();
    StyleConstants.setForeground(ATTRSET_EXIT, color);
  }

  public Color getEdFailColor() {
    return StyleConstants.getForeground(ATTRSET_FAIL);
  }

  public void setEdFailColor(final Color color) {
    clearText();
    StyleConstants.setForeground(ATTRSET_FAIL, color);
  }

  public void addCallText(String text) {
    addText("CALL: " + text, ATTRSET_CALL);
  }

  public void addRedoText(String text) {
    addText("REDO: " + text, ATTRSET_REDO);
  }

  public void addFailText(String text) {
    addText("FAIL: " + text, ATTRSET_FAIL);
  }

  public void addExitText(String text) {
    addText("EXIT: " + text, ATTRSET_EXIT);
  }

  public void addText(String text) {
    addText(text, ATTRSET_OTHER);
  }

  @Override
  public void clearText() {
    this.messageQueue.clear();
    this.messageQueue.add(TextMessage.CLEAR_ALL);
  }

  public void onTimer() {
    final Document document = this.editor.getDocument();
    int loaded = 0;
    while (loaded < 100 && !Thread.currentThread().isInterrupted()) {
      final TextMessage next = messageQueue.poll();
      if (next == null) {
        break;
      }
      loaded++;

      if (next == TextMessage.CLEAR_ALL) {
        try {
          document.remove(0, document.getLength());
        } catch (BadLocationException ex) {
          // ignore
        }
      } else {
        try {
          document.insertString(document.getLength(), next.text + '\n', next.type);
        } catch (BadLocationException ex) {
          // ignore
        }
      }
    }
    if (loaded > 0) {
      this.editor.setCaretPosition(document.getLength());
    }
  }

  public void addText(final String text, final AttributeSet type) {
    if (text != null && type != null) {
      this.messageQueue.add(new TextMessage(text, type));
    }
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    if (e.getActionCommand().equals("CLEAR")) {
      clearText();
    }
  }

  @Override
  public void loadPreferences(Preferences prefs) {
    final Color bgColor = extractColor(prefs, "tracebackcolor", Color.LIGHT_GRAY);
    final Color callColor = extractColor(prefs, "tracecallcolor", Color.BLACK);
    final Color redoColor = extractColor(prefs, "traceredocolor", Color.DARK_GRAY);
    final Color exitColor = extractColor(prefs, "traceexitcolor", Color.BLUE);
    final Color failColor = extractColor(prefs, "tracefailcolor", Color.RED);
    final Color otherColor = extractColor(prefs, "traceothercolor", Color.YELLOW);

    if (bgColor != null) {
      setEdBackground(bgColor);
    }
    if (callColor != null) {
      setEdCallColor(callColor);
    }
    if (redoColor != null) {
      setEdRedoColor(redoColor);
    }
    if (exitColor != null) {
      setEdExitColor(exitColor);
    }
    if (failColor != null) {
      setEdFailColor(failColor);
    }
    if (otherColor != null) {
      setEdOtherColor(otherColor);
    }

    setEdWordWrap(prefs.getBoolean("tracewordwrap", false));

    setEdFont(loadFontFromPrefs(prefs, "tracefont", this.editor.getFont()));
  }

  @Override
  public void savePreferences(Preferences prefs) {
    prefs.putInt("tracebackcolor", getEdBackground().getRGB());
    prefs.putInt("tracecallcolor", getEdCallColor().getRGB());
    prefs.putInt("traceredocolor", getEdRedoColor().getRGB());
    prefs.putInt("traceexitcolor", getEdExitColor().getRGB());
    prefs.putInt("tracefailcolor", getEdFailColor().getRGB());
    prefs.putInt("traceothercolor", getEdOtherColor().getRGB());
    prefs.putBoolean("tracewordwrap", getEdWordWrap());
    saveFontToPrefs(prefs, "tracefont", editor.getFont());
  }

  private static final class TextMessage {
    private static final TextMessage CLEAR_ALL = new TextMessage(null, null);
    final String text;
    final AttributeSet type;

    TextMessage(final String text, final AttributeSet type) {
      this.text = text;
      this.type = type;
    }
  }
}
