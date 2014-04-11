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
import java.awt.PopupMenu;
import java.util.prefs.Preferences;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

/**
 * The class implements the message pane for the IDE because it is a very
 * specialized auxiliary class, it is not described very precisely
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class MessageEditor extends AbstractProlEditor {
  private static final long serialVersionUID = -9129430685050371959L;

  public static final int TYPE_INFO = 0;
  public static final int TYPE_WARNING = 1;
  public static final int TYPE_ERROR = 2;
  private String insideBuffer;
  private static final SimpleAttributeSet ATTRSET_INFO = new SimpleAttributeSet();
  private static final SimpleAttributeSet ATTRSET_WARNING = new SimpleAttributeSet();
  private static final SimpleAttributeSet ATTRSET_ERROR = new SimpleAttributeSet();

  public Color getEdInfoColor() {
    return StyleConstants.getForeground(ATTRSET_INFO);
  }

  public void setEdInfoColor(final Color color) {
    clearText();
    StyleConstants.setForeground(ATTRSET_INFO, color);
  }

  public Color getEdWarningColor() {
    return StyleConstants.getForeground(ATTRSET_WARNING);
  }

  public void setEdWarningColor(final Color color) {
    clearText();
    StyleConstants.setForeground(ATTRSET_WARNING, color);
  }

  public Color getEdErrorColor() {
    return StyleConstants.getForeground(ATTRSET_ERROR);
  }

  public void setEdErrorColor(final Color color) {
    clearText();
    StyleConstants.setForeground(ATTRSET_ERROR, color);
  }

  public MessageEditor() {
    super("Messages");

    removePropertyFromList("EdForeground");
    removePropertyFromList("EdCaretColor");
    removePropertyFromList("EdWordWrap");

    addPropertyToList(new PropertyLink(this, "Info color", "EdInfoColor"));
    addPropertyToList(new PropertyLink(this, "Warning color", "EdWarningColor"));
    addPropertyToList(new PropertyLink(this, "Error color", "EdErrorColor"));

    setEnabled(false);
    editor.setContentType("text/html");

    insideBuffer = "";

    final PopupMenu popup = new PopupMenu();

    editor.setBackground(Color.BLUE.darker().darker().darker().darker());
    editor.setForeground(Color.WHITE);
  }

  public void addHyperlinkListener(HyperlinkListener listener) {
    editor.addHyperlinkListener(listener);
  }

  public void removeHypelinkListener(HyperlinkListener listener) {
    editor.removeHyperlinkListener(listener);
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
  public synchronized void clearText() {
    super.clearText();
    insideBuffer = "";
  }

  private static String colorToHtml(Color color) {
    final String str = Integer.toHexString(color.getRGB() & 0xFFFFFF).toUpperCase();
    if (str.length() < 6) {
      return "#" + "00000".substring(str.length() - 1) + str;
    }
    return "#" + str;
  }

  public synchronized void addText(String text, int type, String linkRef, String linkText) {

    text = escapeHTML(text);

    String color = null;

    boolean bold = false;
    boolean italic = false;

    switch (type) {
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

    final StringBuilder bldr = new StringBuilder();

    if (italic) {
      bldr.append("<i>");
    }
    if (bold) {
      bldr.append("<b>");
    }

    bldr.append("<span style=\"color:").append(color).append("\">").append(text);
    if (linkRef != null) {
      bldr.append("&nbsp;<a href=\"" + linkRef + "\">");
      bldr.append(linkText == null ? escapeHTML(linkRef) : escapeHTML(linkText));
      bldr.append("</a>");
    }
    bldr.append("</span><br>");
    if (italic) {
      bldr.append("</i>");
    }
    if (bold) {
      bldr.append("</b>");
    }

    insideBuffer += bldr.toString();
    editor.setText("<html><body>" + insideBuffer + "</body><html>");
  }

  @Override
  public void loadPreferences(Preferences prefs) {
    setEdBackground(new Color(prefs.getInt("messagesbackcolor", 0x3E13AF)));
    setEdErrorColor(new Color(prefs.getInt("messageserrorcolor", 0xC10087)));
    setEdInfoColor(new Color(prefs.getInt("messagesinfocolor", 0xB1F100)));
    setEdWarningColor(new Color(prefs.getInt("messageswarningcolor", 0xFFD700
    
    )));
    setEdFont(loadFontFromPrefs(prefs, "messagesfont"));
  }

  @Override
  public void savePreferences(Preferences prefs) {
    prefs.putInt("messagesbackcolor", getEdBackground().getRGB());
    prefs.putInt("messageserrorcolor", getEdErrorColor().getRGB());
    prefs.putInt("messagesinfocolor", getEdInfoColor().getRGB());
    prefs.putInt("messageswarningcolor", getEdWarningColor().getRGB());
    saveFontToPrefs(prefs, "messagesfont", editor.getFont());
  }
}
