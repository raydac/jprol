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

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import java.awt.*;
import java.awt.datatransfer.Clipboard;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.prefs.Preferences;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.JTextComponent;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rtextarea.RTextScrollPane;

/**
 * The class extends a Scroll pane and is used as the ancestor for all text
 * windows in the IDE because it is a very specialized auxiliary class, it is
 * not described very precisely
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public abstract class AbstractProlEditor extends JPanel implements TreeModel {

  private static final long serialVersionUID = -3683279049571011888L;
  protected final ArrayList<PropertyLink> editableProperties;
  protected final JTextComponent editor;
  protected final JScrollPane scrollPane;
  protected final JPopupMenu POPUP_MENU = new JPopupMenu();
  protected final JMenuItem POPUP_CLEARTEXT = new JMenuItem("Clear text", UIUtils.loadIcon("page_delete"));
  protected final JMenuItem POPUP_COPY = new JMenuItem("Copy", UIUtils.loadIcon("page_copy"));
  protected final JMenuItem POPUP_CUT = new JMenuItem("Cut", UIUtils.loadIcon("cut"));
  protected final JMenuItem POPUP_PASTE = new JMenuItem("Paste", UIUtils.loadIcon("page_paste"));
  private final String nameID;
  private boolean wordWrap;

  public AbstractProlEditor(final String title, final boolean scalable, final boolean lineNumeration) {
    this(title, new EditorPane(scalable), lineNumeration);
  }

  public AbstractProlEditor(final String title, final JTextComponent editor, final boolean lineNumeration) {
    super();
    this.editableProperties = new ArrayList<>();
    this.editor = editor;

    if (this.editor instanceof RSyntaxTextArea) {
      this.scrollPane = new RTextScrollPane(this.editor, lineNumeration);
    } else {
      this.scrollPane = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    }

    this.setEdWordWrap(false);

    setBorder(new TitledBorder(title));
    this.nameID = title;
    this.editor.setEditable(true);

    setLayout(new BorderLayout(0, 0));
    add(this.scrollPane, BorderLayout.CENTER);

    // add properties
    addPropertyToList(new PropertyLink(this, "Font", "EdFont"));
    addPropertyToList(new PropertyLink(this, "Background color", "EdBackground"));
    addPropertyToList(new PropertyLink(this, "Foreground color", "EdForeground"));
    addPropertyToList(new PropertyLink(this, "Caret color", "EdCaretColor"));
    addPropertyToList(new PropertyLink(this, "Word wrap", "EdWordWrap"));

    this.POPUP_CLEARTEXT.addActionListener((ActionEvent e) -> {
      clearText();
    });

    this.POPUP_COPY.addActionListener((ActionEvent e) -> {
      copyText();
    });

    this.POPUP_PASTE.addActionListener((ActionEvent e) -> {
      pasteText();
    });

    this.POPUP_CUT.addActionListener((ActionEvent e) -> {
      cutText();
    });

    // add popup
    if (doesSupportTextCut()) {
      this.POPUP_MENU.add(this.POPUP_CUT);
    }
    this.POPUP_MENU.add(this.POPUP_COPY);
    if (doesSupportTextPaste()) {
      POPUP_MENU.add(this.POPUP_PASTE);
    }
    this.POPUP_MENU.add(this.POPUP_CLEARTEXT);

    this.editor.setComponentPopupMenu(this.POPUP_MENU);
  }

  public void addHyperlinkListener(final HyperlinkListener listener) {
    if (this.editor instanceof JEditorPane) {
      ((JEditorPane) this.editor).addHyperlinkListener(listener);
    }
  }

  public void removeHypelinkListener(final HyperlinkListener listener) {
    if (this.editor instanceof JEditorPane) {
      ((JEditorPane) this.editor).removeHyperlinkListener(listener);
    }
  }

  public void setContentType(final String mime) {
    if (this.editor instanceof JEditorPane) {
      ((JEditorPane) this.editor).setContentType(mime);
    } else if (this.editor instanceof RSyntaxTextArea) {
      ((RSyntaxTextArea) this.editor).setSyntaxEditingStyle(mime);
    }
  }

  protected static Color extractColor(final Preferences prefs, final String name) {
    final int value = prefs.getInt(name, 0xFFFFFFFF);
    return value == 0xFFFFFFFF ? null : new Color(value & 0xFFFFFF);
  }

  protected static Color extractColor(final Preferences prefs, final String name, final Color dflt) {
    final int value = prefs.getInt(name, 0xFFFFFFFF);
    return value == 0xFFFFFFFF ? dflt : new Color(value & 0xFFFFFF);
  }

  public static String escapeHTML(String s) {
    final StringBuilder sb = new StringBuilder();
    int n = s.length();
    for (int i = 0; i < n; i++) {
      char c = s.charAt(i);
      switch (c) {
        case '<':
          sb.append("&lt;");
          break;
        case '>':
          sb.append("&gt;");
          break;
        case '&':
          sb.append("&amp;");
          break;
        case '"':
          sb.append("&quot;");
          break;
        case 'à':
          sb.append("&agrave;");
          break;
        case 'À':
          sb.append("&Agrave;");
          break;
        case 'â':
          sb.append("&acirc;");
          break;
        case 'Â':
          sb.append("&Acirc;");
          break;
        case 'ä':
          sb.append("&auml;");
          break;
        case 'Ä':
          sb.append("&Auml;");
          break;
        case 'å':
          sb.append("&aring;");
          break;
        case 'Å':
          sb.append("&Aring;");
          break;
        case 'æ':
          sb.append("&aelig;");
          break;
        case 'Æ':
          sb.append("&AElig;");
          break;
        case 'ç':
          sb.append("&ccedil;");
          break;
        case 'Ç':
          sb.append("&Ccedil;");
          break;
        case 'é':
          sb.append("&eacute;");
          break;
        case 'É':
          sb.append("&Eacute;");
          break;
        case 'è':
          sb.append("&egrave;");
          break;
        case 'È':
          sb.append("&Egrave;");
          break;
        case 'ê':
          sb.append("&ecirc;");
          break;
        case 'Ê':
          sb.append("&Ecirc;");
          break;
        case 'ë':
          sb.append("&euml;");
          break;
        case 'Ë':
          sb.append("&Euml;");
          break;
        case 'ï':
          sb.append("&iuml;");
          break;
        case 'Ï':
          sb.append("&Iuml;");
          break;
        case 'ô':
          sb.append("&ocirc;");
          break;
        case 'Ô':
          sb.append("&Ocirc;");
          break;
        case 'ö':
          sb.append("&ouml;");
          break;
        case 'Ö':
          sb.append("&Ouml;");
          break;
        case 'ø':
          sb.append("&oslash;");
          break;
        case 'Ø':
          sb.append("&Oslash;");
          break;
        case 'ß':
          sb.append("&szlig;");
          break;
        case 'ù':
          sb.append("&ugrave;");
          break;
        case 'Ù':
          sb.append("&Ugrave;");
          break;
        case 'û':
          sb.append("&ucirc;");
          break;
        case 'Û':
          sb.append("&Ucirc;");
          break;
        case 'ü':
          sb.append("&uuml;");
          break;
        case 'Ü':
          sb.append("&Uuml;");
          break;
        case '®':
          sb.append("&reg;");
          break;
        case '©':
          sb.append("&copy;");
          break;
        case '€':
          sb.append("&euro;");
          break;
        // be carefull with this one (non-breaking whitee space)
        //case ' ':
        //    sb.append("&nbsp;");
        //    break;

        default:
          sb.append(c);
          break;
      }
    }
    return sb.toString();
  }

  public String getNameID() {
    return this.nameID;
  }

  public Font getEdFont() {
    return editor.getFont();
  }

  public void setEdFont(final Font val) {
    editor.setFont(val);
  }

  public void setPopupMenu(final JPopupMenu menu) {
    this.editor.setComponentPopupMenu(menu);
  }

  public Color getEdBackground() {
    return editor.getBackground();
  }

  public void setEdBackground(final Color val) {
    editor.setBackground(val);
    scrollPane.getViewport().setBackground(val);
  }

  public boolean getEdWordWrap() {
    return this.wordWrap;
  }

  public void setEdWordWrap(final Boolean value) {
    final Component c = this.scrollPane.getViewport().getView();

    if (c instanceof RSyntaxTextArea) {
      ((RSyntaxTextArea) c).setLineWrap(value);
    } else {
      if (c != null) {
        this.scrollPane.getViewport().remove(c);
      }

      final Container editorParent = this.editor.getParent();
      if (editorParent != null) {
        editorParent.remove(this.editor);
      }

      this.wordWrap = value;

      if (value) {
        this.scrollPane.getViewport().setView(this.editor);
      } else {
        final JPanel wrapper = new JPanel(new BorderLayout(0, 0));
        wrapper.add(this.editor, BorderLayout.CENTER);
        this.scrollPane.getViewport().setView(wrapper);
      }
    }
    this.scrollPane.revalidate();
    this.scrollPane.repaint();
  }

  public Color getEdForeground() {
    return editor.getForeground();
  }

  public void setEdForeground(final Color val) {
    editor.setForeground(val);
  }

  public Color getEdCaretColor() {
    return editor.getCaretColor();
  }

  public void setEdCaretColor(final Color val) {
    editor.setCaretColor(val);
  }

  protected synchronized final void removePropertyFromList(final String propertyName) {
    for (int li = 0; li < editableProperties.size(); li++) {
      final PropertyLink link = editableProperties.get(li);
      if (link.property.equals(propertyName)) {
        editableProperties.remove(li);
        return;
      }
    }
  }

  protected final synchronized void addPropertyToList(final PropertyLink link) {
    this.editableProperties.add(link);
  }

  @Override
  public String toString() {
    return this.nameID;
  }

  public synchronized void setTitle(String title) {
    ((TitledBorder) getBorder()).setTitle(title);
  }

  @Override
  public void setEnabled(final boolean flag) {
    //super.setEnabled(flag);
    editor.setEditable(flag);
    if (!flag) {
      editor.transferFocus();
    }
  }

  @Override
  public void requestFocus() {
    if (editor.isEditable()) {
      editor.requestFocus();
    }
  }

  public synchronized JTextComponent getEditor() {
    return editor;
  }

  public synchronized void clearText() {
    editor.setText("");
  }

  public synchronized void copyText() {
    Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
    TransferHandler transferHandler = editor.getTransferHandler();
    transferHandler.exportToClipboard(editor, clipboard, TransferHandler.COPY);
  }

  public synchronized void pasteText() {
    Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
    TransferHandler transferHandler = editor.getTransferHandler();
    transferHandler.importData(editor, clipboard.getContents(null));
  }

  public synchronized void cutText() {
    copyText();
    editor.replaceSelection("");
  }

  @Override
  public void addTreeModelListener(final TreeModelListener l) {
  }

  @Override
  public Object getChild(final Object parent, final int index) {
    if (this.equals(parent)) {
      return editableProperties.get(index);
    }
    return null;
  }

  @Override
  public int getChildCount(final Object parent) {
    if (this.equals(parent)) {
      return editableProperties.size();
    }
    return -1;
  }

  @Override
  public int getIndexOfChild(final Object parent, final Object child) {
    if (this.equals(parent)) {
      return editableProperties.indexOf(child);
    }
    return -1;
  }

  @Override
  public Object getRoot() {
    return this;
  }

  @Override
  public boolean isLeaf(final Object node) {
    return node instanceof PropertyLink;
  }

  @Override
  public void removeTreeModelListener(TreeModelListener l) {
    // just ignore
  }

  @Override
  public void valueForPathChanged(final TreePath path, final Object newValue) {
    // just ignore
  }

  public abstract void loadPreferences(Preferences prefs);

  public abstract void savePreferences(Preferences prefs);

  public void saveFontToPrefs(final Preferences prefs, final String key, final Font font) {
    String style = "PLAIN";
    switch (font.getStyle()) {
      case Font.BOLD: {
        style = "BOLD";
      }
      break;
      case Font.ITALIC: {
        style = "ITALIC";
      }
      break;
      case Font.BOLD | Font.ITALIC: {
        style = "BOLDITALIC";
      }
      break;
    }
    prefs.put(key, font.getFamily() + " " + style + " " + font.getSize());
  }

  public Font loadFontFromPrefs(Preferences prefs, String key, final Font defaultFont) {
    return prefs.get(key, null) == null ? defaultFont : Font.decode(prefs.get(key, null));
  }

  public boolean doesSupportTextPaste() {
    return false;
  }

  public boolean doesSupportTextCut() {
    return false;
  }

  public final static class PropertyLink {

    private final Object ownerObject;
    private final Class<?> ownerClass;
    private final String name;
    private final String property;

    public PropertyLink(final Object object, final String name, final String property) {
      this.name = name;
      this.ownerObject = object;
      this.ownerClass = object.getClass();
      this.property = property;
    }

    public Object getOwner() {
      return ownerObject;
    }

    @SuppressWarnings("unchecked")
    public Object getProperty() {
      try {
        Method meth = ownerClass.getMethod("get" + property, (Class[]) null);
        return meth.invoke(ownerObject);
      } catch (Throwable thr) {
        throw new RuntimeException("Can't read property", thr);
      }
    }

    @SuppressWarnings("unchecked")
    public void setProperty(final Object obj) {
      try {
        ownerClass.getMethod("set" + property, new Class<?>[]{obj.getClass()}).invoke(ownerObject, obj);
      } catch (Throwable thr) {
        throw new RuntimeException("Can't set property", thr);
      }
    }

    @Override
    public String toString() {
      String str = this.name;
      final Object val = getProperty();
      if (val instanceof Boolean) {
        str += " (" + val.toString() + ")";
      }

      return str;
    }
  }
}
