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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.prefs.Preferences;
import javax.swing.SwingUtilities;
import javax.swing.event.HyperlinkListener;
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
  private static final long serialVersionUID = -907317020170501786L;

  private static final SimpleAttributeSet ATTRSET_OTHER = new SimpleAttributeSet();
  private static final SimpleAttributeSet ATTRSET_CALL = new SimpleAttributeSet();
  private static final SimpleAttributeSet ATTRSET_REDO = new SimpleAttributeSet();
  private static final SimpleAttributeSet ATTRSET_FAIL = new SimpleAttributeSet();
  private static final SimpleAttributeSet ATTRSET_EXIT = new SimpleAttributeSet();

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

  public TraceDialog() {
    super("Trace");

    removePropertyFromList("EdForeground");
    removePropertyFromList("EdCaretColor");

    addPropertyToList(new PropertyLink(this, "Call color", "EdCallColor"));
    addPropertyToList(new PropertyLink(this, "Redo color", "EdRedoColor"));
    addPropertyToList(new PropertyLink(this, "Fail color", "EdFailColor"));
    addPropertyToList(new PropertyLink(this, "Exit color", "EdExitColor"));
    addPropertyToList(new PropertyLink(this, "Other color", "EdOtherColor"));

    setEnabled(false);
    editor.setContentType("text/rtf");

    editor.setBackground(Color.BLUE.darker().darker().darker().darker());
    editor.setForeground(Color.WHITE);

  }

  public void removeHypelinkListener(HyperlinkListener listener) {
    editor.removeHyperlinkListener(listener);
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
  public synchronized void clearText() {
    super.clearText();
  }

  public void addText(final String text, final AttributeSet type) {
    final Thread thr = Thread.currentThread();

    try {
      SwingUtilities.invokeAndWait(new Runnable() {

        @Override
        public void run() {
          if (thr.isInterrupted()) {
            return;
          }
          final Document doc = editor.getDocument();
          if (doc != null) {
            try {
              doc.insertString(doc.getEndPosition().getOffset(), text + '\n', type);
              editor.setCaretPosition(doc.getLength());
            }
            catch (BadLocationException ex) {
              ex.printStackTrace();
            }
          }
        }
      });
    }
    catch (Exception ex) {
      ex.printStackTrace();

      if (ex instanceof InterruptedException) {
        Thread.currentThread().interrupt();
      }

      return;
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
    setEdBackground(new Color(prefs.getInt("tracebackcolor", 0x7C005A)));
    setEdCallColor(new Color(prefs.getInt("tracecallcolor", 0xBF9430)));
    setEdRedoColor(new Color(prefs.getInt("traceredocolor", 0xA67300)));
    setEdExitColor(new Color(prefs.getInt("traceexitcolor", 0x00B060)));
    setEdFailColor(new Color(prefs.getInt("tracefailcolor", 0xFF4500)));
    setEdOtherColor(new Color(prefs.getInt("traceothercolor", 0xA62D00)));

    setEdWordWrap(prefs.getBoolean("tracewordwrap", false));

    setEdFont(loadFontFromPrefs(prefs, "tracefont"));
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
}
