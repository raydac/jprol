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

import static com.igormaznitsa.jprol.easygui.JProlStyledDocument.StyledText.CLEAR;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.prefs.Preferences;
import javax.swing.text.AttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

/**
 * The class implements the trace pane for the IDE because it is a very
 * specialized auxiliary class, it is not described very precisely
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class TraceDialog extends AbstractProlEditor implements ActionListener {
  private static final SimpleAttributeSet ATTRIBUTE_OTHER = new SimpleAttributeSet();
  private static final SimpleAttributeSet ATTRIBUTE_CALL = new SimpleAttributeSet();
  private static final SimpleAttributeSet ATTRIBUTE_REDO = new SimpleAttributeSet();
  private static final SimpleAttributeSet ATTRIBUTE_FAIL = new SimpleAttributeSet();
  private static final SimpleAttributeSet ATTRIBUTE_EXIT = new SimpleAttributeSet();

  private final Queue<JProlStyledDocument.StyledText> messageQueue = new ConcurrentLinkedQueue<>();

  public TraceDialog() {
    super("Trace", false, false);

    this.editor.setEditable(false);

    this.removePropertyLink("EdForeground");
    this.removePropertyLink("EdCaretColor");

    this.addPropertyLink(new PropertyLink(this, "Call color", "EdCallColor"));
    this.addPropertyLink(new PropertyLink(this, "Redo color", "EdRedoColor"));
    this.addPropertyLink(new PropertyLink(this, "Fail color", "EdFailColor"));
    this.addPropertyLink(new PropertyLink(this, "Exit color", "EdExitColor"));
    this.addPropertyLink(new PropertyLink(this, "Other color", "EdOtherColor"));

    this.setEnabled(false);
    this.setContentType("text/plain");

    this.editor.setBackground(Color.BLUE.darker().darker().darker().darker());
    this.editor.setForeground(Color.WHITE);
  }

  public Color getEdOtherColor() {
    return StyleConstants.getForeground(ATTRIBUTE_OTHER);
  }

  public void setEdOtherColor(final Color color) {
    StyleConstants.setForeground(ATTRIBUTE_OTHER, color);
    this.clearText();
  }

  public Color getEdCallColor() {
    return StyleConstants.getForeground(ATTRIBUTE_CALL);
  }

  public void setEdCallColor(final Color color) {
    StyleConstants.setForeground(ATTRIBUTE_CALL, color);
    this.clearText();
  }

  public Color getEdRedoColor() {
    return StyleConstants.getForeground(ATTRIBUTE_REDO);
  }

  public void setEdRedoColor(final Color color) {
    StyleConstants.setForeground(ATTRIBUTE_REDO, color);
    this.clearText();
  }

  public Color getEdExitColor() {
    return StyleConstants.getForeground(ATTRIBUTE_EXIT);
  }

  public void setEdExitColor(final Color color) {
    StyleConstants.setForeground(ATTRIBUTE_EXIT, color);
    this.clearText();
  }

  public Color getEdFailColor() {
    return StyleConstants.getForeground(ATTRIBUTE_FAIL);
  }

  public void setEdFailColor(final Color color) {
    StyleConstants.setForeground(ATTRIBUTE_FAIL, color);
    this.clearText();
  }

  public void addCallText(String text) {
    this.addText("CALL: " + text, ATTRIBUTE_CALL);
  }

  public void addRedoText(String text) {
    this.addText("REDO: " + text, ATTRIBUTE_REDO);
  }

  public void addFailText(String text) {
    this.addText("FAIL: " + text, ATTRIBUTE_FAIL);
  }

  public void addExitText(String text) {
    this.addText("EXIT: " + text, ATTRIBUTE_EXIT);
  }

  public void addText(String text) {
    this.addText(text, ATTRIBUTE_OTHER);
  }

  @Override
  public void clearText() {
    this.messageQueue.clear();
    this.messageQueue.add(CLEAR);
  }

  public void onTimer() {
    final JProlStyledDocument document = (JProlStyledDocument) this.editor.getDocument();
    int loaded = 0;

    List<JProlStyledDocument.StyledText> foundTexts = null;

    while (loaded < 100 && !Thread.currentThread().isInterrupted()) {
      final JProlStyledDocument.StyledText next = this.messageQueue.poll();
      if (next == null) {
        break;
      }
      loaded++;

      if (foundTexts == null) {
        foundTexts = new ArrayList<>();
      }

      if (next == CLEAR) {
        foundTexts.add(next);
      } else {
        foundTexts.add(next.copyWith(next.getText() + '\n'));
      }
    }

    if (foundTexts != null) {
      document.insertBunch(foundTexts);
      this.editor.setCaretPosition(document.getLength());
    }
  }

  public void addText(final String text, final AttributeSet type) {
    if (text != null && type != null) {
      this.messageQueue.add(new JProlStyledDocument.StyledText(type, text));
    }
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    if (e.getActionCommand().equals("CLEAR")) {
      this.clearText();
    }
  }

  @Override
  public void loadPreferences(final Preferences prefs) {
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

    this.setEdWordWrap(prefs.getBoolean("tracewordwrap", false));

    this.setEdFont(loadFontFromPrefs(prefs, "tracefont", this.editor.getFont()));
  }

  @Override
  public void savePreferences(final Preferences prefs) {
    prefs.putInt("tracebackcolor", getEdBackground().getRGB());
    prefs.putInt("tracecallcolor", getEdCallColor().getRGB());
    prefs.putInt("traceredocolor", getEdRedoColor().getRGB());
    prefs.putInt("traceexitcolor", getEdExitColor().getRGB());
    prefs.putInt("tracefailcolor", getEdFailColor().getRGB());
    prefs.putInt("traceothercolor", getEdOtherColor().getRGB());
    prefs.putBoolean("tracewordwrap", getEdWordWrap());
    saveFontToPrefs(prefs, "tracefont", editor.getFont());
  }

}
