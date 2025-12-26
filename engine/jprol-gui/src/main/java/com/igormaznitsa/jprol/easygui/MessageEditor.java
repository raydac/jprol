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
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.prefs.Preferences;
import javax.swing.JEditorPane;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

/**
 * The class implements the message pane for the IDE because it is a very
 * specialized auxiliary class, it is not described very precisely
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class MessageEditor extends AbstractProlEditor {

  public static final int TYPE_INFO = 0;
  public static final int TYPE_WARNING = 1;
  public static final int TYPE_ERROR = 2;
  private static final SimpleAttributeSet ATTRSET_INFO = new SimpleAttributeSet();
  private static final SimpleAttributeSet ATTRSET_WARNING = new SimpleAttributeSet();
  private static final SimpleAttributeSet ATTRSET_ERROR = new SimpleAttributeSet();
  private final StringBuilder internalBuffer = new StringBuilder();
  private final Queue<TextRecord> recordQueue = new ConcurrentLinkedQueue<>();

  public MessageEditor() {
    super("Messages", false, false);
    this.editor.setEditable(false);
    this.editor.putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, true);

    removePropertyLink("EdForeground");
    removePropertyLink("EdCaretColor");
    removePropertyLink("EdWordWrap");

    addPropertyLink(new PropertyLink(this, "Info color", "EdInfoColor"));
    addPropertyLink(new PropertyLink(this, "Warning color", "EdWarningColor"));
    addPropertyLink(new PropertyLink(this, "Error color", "EdErrorColor"));

    setContentType("text/html");

    this.internalBuffer.setLength(0);

    editor.setBackground(Color.BLUE.darker().darker().darker().darker());
    editor.setForeground(Color.WHITE);

    editor.setFont(DEFAULT_FONT);
  }

  private static String colorToHtml(Color color) {
    final String str = Integer.toHexString(color.getRGB() & 0xFFFFFF).toUpperCase();
    if (str.length() < 6) {
      return "#" + "00000".substring(str.length() - 1) + str;
    }
    return "#" + str;
  }

  public Color getEdInfoColor() {
    return StyleConstants.getForeground(ATTRSET_INFO);
  }

  public void setEdInfoColor(final Color color) {
    this.clearText();
    StyleConstants.setForeground(ATTRSET_INFO, color);
  }

  public Color getEdWarningColor() {
    return StyleConstants.getForeground(ATTRSET_WARNING);
  }

  public void setEdWarningColor(final Color color) {
    this.clearText();
    StyleConstants.setForeground(ATTRSET_WARNING, color);
  }

  public Color getEdErrorColor() {
    return StyleConstants.getForeground(ATTRSET_ERROR);
  }

  public void setEdErrorColor(final Color color) {
    this.clearText();
    StyleConstants.setForeground(ATTRSET_ERROR, color);
  }

  public void addInfoText(String text) {
    addText(text, TYPE_INFO, null, null);
  }

  public void addWarningText(String text) {
    addText(text, TYPE_WARNING, null, null);
  }

  public void addErrorText(String text) {
    addText(text, TYPE_ERROR, null, null);
  }

  @Override
  public void clearText() {
    this.recordQueue.add(TextRecord.CLEAR_ALL);
  }

  public void onTimer() {
    this.processQueue();
  }

  private void processQueue() {
    final StringBuilder builder = new StringBuilder();
    int counter = 0;
    while (counter < 100 && !Thread.currentThread().isInterrupted()) {
      final TextRecord record = recordQueue.poll();
      if (record == null) {
        break;
      }
      counter++;
      if (record == TextRecord.CLEAR_ALL) {
        this.internalBuffer.setLength(0);
        builder.setLength(0);
      } else {
        builder.append(record.toText());
      }
    }
    if (counter > 0) {
      this.internalBuffer.append(builder);
    }
    this.updateEditorText(this.internalBuffer.toString());
  }

  private void updateEditorText(final String text) {
    this.editor.setText(
        "<html><body bgcolor=\"" + colorToHtml(this.editor.getBackground()) + "\">" + text +
            "</body><html>");
  }

  public void addText(String text, int type, String linkRef, String linkText) {
    this.recordQueue.add(new TextRecord(text, type, linkRef, linkText));
  }

  @Override
  public void loadPreferences(Preferences prefs) {
    final Color bgColor = extractColor(prefs, "messagesbackcolor", Color.BLACK);
    final Color errColor = extractColor(prefs, "messageserrorcolor", Color.RED);
    final Color infColor = extractColor(prefs, "messagesinfocolor", Color.GREEN);
    final Color wrnColor = extractColor(prefs, "messageswarningcolor", Color.YELLOW);

    if (bgColor != null) {
      setEdBackground(bgColor);
    }
    if (errColor != null) {
      setEdErrorColor(errColor);
    }
    if (infColor != null) {
      setEdInfoColor(infColor);
    }
    if (wrnColor != null) {
      setEdWarningColor(wrnColor);
    }
    setEdFont(loadFontFromPrefs(prefs, "messagesfont", this.editor.getFont()));
  }

  @Override
  public void savePreferences(Preferences prefs) {
    prefs.putInt("messagesbackcolor", getEdBackground().getRGB());
    prefs.putInt("messageserrorcolor", getEdErrorColor().getRGB());
    prefs.putInt("messagesinfocolor", getEdInfoColor().getRGB());
    prefs.putInt("messageswarningcolor", getEdWarningColor().getRGB());
    saveFontToPrefs(prefs, "messagesfont", editor.getFont());
  }

  private static final class TextRecord {
    static final TextRecord CLEAR_ALL = new TextRecord("", -1, "", "");

    final String text;
    final int type;
    final String linkRef;
    final String linkText;

    TextRecord(final String text, final int type, final String linkRef, final String linkText) {
      this.text = text;
      this.type = type;
      this.linkRef = linkRef;
      this.linkText = linkText;
    }

    private String toText() {
      final String text = escapeHTML(this.text);

      String color;

      boolean bold;
      boolean italic = false;

      switch (this.type) {
        case TYPE_INFO: {
          color = colorToHtml(StyleConstants.getForeground(ATTRSET_INFO));
          bold = true;
        }
        break;
        case TYPE_ERROR: {
          color = colorToHtml(StyleConstants.getForeground(ATTRSET_ERROR));
          bold = true;
        }
        break;
        case TYPE_WARNING: {
          color = colorToHtml(StyleConstants.getForeground(ATTRSET_WARNING));
          bold = true;
          italic = true;
        }
        break;
        default:
          throw new IllegalArgumentException("Unsupported message type");
      }

      final StringBuilder buffer = new StringBuilder();

      if (italic) {
        buffer.append("<i>");
      }
      if (bold) {
        buffer.append("<b>");
      }

      buffer.append("<span style=\"color:").append(color).append("\">").append(text);
      if (this.linkRef != null) {
        buffer.append("&nbsp;<a href=\"").append(this.linkRef).append("\">");
        buffer.append(this.linkText == null ? escapeHTML(this.linkRef) : escapeHTML(this.linkText));
        buffer.append("</a>");
      }
      buffer.append("</span><br>");
      if (italic) {
        buffer.append("</i>");
      }
      if (bold) {
        buffer.append("</b>");
      }

      return buffer.toString();
    }
  }
}
