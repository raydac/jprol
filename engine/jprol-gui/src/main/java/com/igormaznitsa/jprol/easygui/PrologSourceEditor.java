package com.igormaznitsa.jprol.easygui;

import com.igormaznitsa.jprol.easygui.tokenizer.JProlTokenMaker;
import java.awt.Color;
import java.awt.Font;
import java.io.IOException;
import java.util.logging.Logger;
import java.util.prefs.Preferences;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.event.DocumentListener;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.undo.UndoManager;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.Theme;
import org.fife.ui.rtextarea.RUndoManager;

/**
 * The class implements the source editor pane of the IDE.
 */
public class PrologSourceEditor extends AbstractProlEditor {

  public static final String SOURCE_WORDWRAP = "sourcewordwrap";
  public static final String SOURCE_FOREGROUND_COLOR = "sourceforegroundcolor";
  public static final String SOURCE_CARET_COLOR = "sourcecaretcolor";
  public static final String SOURCE_BACK_COLOR = "sourceedbackcolor";
  public static final String SOURCE_FONT = "sourcefont";
  /**
   * Inside logger, the logger id = "PROL_NOTE_PAD"
   */
  protected static final Logger LOGGER = Logger.getLogger(PrologSourceEditor.class.getName());
  private static final long serialVersionUID = -2223529439306867844L;
  protected RUndoManager undoManager;

  public PrologSourceEditor() {
    super("Editor", new ScalableRsyntaxTextArea(), true);
    replacePropertyLink(PROPERTY_ED_FONT, new PropertyLink(this, "Font", "EdAndBaseFont"));

    final ScalableRsyntaxTextArea theEditor = (ScalableRsyntaxTextArea) this.editor;
    theEditor.setSyntaxEditingStyle(JProlTokenMaker.MIME);
    theEditor.setTabsEmulated(true);

    this.applyScheme(theEditor);

    theEditor.getInputMap().put(KeyStroke.getKeyStroke("control Z"), "none");
    theEditor.getInputMap().put(KeyStroke.getKeyStroke("control Y"), "none");

    theEditor.setAntiAliasingEnabled(true);
    theEditor.setBracketMatchingEnabled(true);
    theEditor.setCodeFoldingEnabled(true);

    removePropertyLink("EdWordWrap");

    theEditor.setFont(DEFAULT_FONT);
    theEditor.setBaseFont(DEFAULT_FONT);

    editor.setVisible(true);

    setEnabled(true);

    this.undoManager = new RUndoManager(theEditor);
  }

  private void applyScheme(final RSyntaxTextArea editor) {
    final String resource;
    if (UiUtils.isDarkTheme()) {
      resource = "/themes/jprol_dark.xml";
    } else {
      resource = "/themes/jprol_light.xml";
    }
    try {
      Theme theme = Theme.load(getClass().getResourceAsStream(resource));
      theme.apply(editor);
    } catch (IOException ioe) {
      // do nothing
    }
  }

  public Font getEdAndBaseFont() {
    return ((ScalableRsyntaxTextArea) this.editor).getBaseFont();
  }

  public void setEdAndBaseFont(final Font font) {
    final ScalableRsyntaxTextArea scalableRsyntaxTextArea = (ScalableRsyntaxTextArea) this.editor;
    scalableRsyntaxTextArea.setBaseFont(font);
    scalableRsyntaxTextArea.setFont(font);
  }

  public String getText() {
    return editor.getText();
  }

  public UndoManager getUndoManager() {
    return undoManager;
  }

  public void addUndoableEditListener(final UndoableEditListener listener) {
    editor.getDocument().addUndoableEditListener(listener);
  }

  public void addDocumentListener(final DocumentListener listener) {
    editor.getDocument().addDocumentListener(listener);
  }

  public int getCaretPosition() {
    return this.editor.getCaretPosition();
  }

  public void setCaretPosition(final int pos) {
    this.editor.setCaretPosition(pos);
    this.editor.getCaret().setVisible(true);
  }

  public void setCaretPosition(final int line, final int pos) {
    try {
      final Element rootelement = editor.getDocument().getDefaultRootElement();

      final Element element = rootelement.getElement(line - 1);
      final int offset = element.getStartOffset() + pos - 1;

      editor.setCaretPosition(offset);
      editor.requestFocus();
    } catch (Exception ex) {
      LOGGER.throwing(this.getClass().getCanonicalName(), "setCaretPosition()", ex);
    }
  }

  @Override
  public void loadPreferences(final Preferences preferences) {
    final Color backColor = extractColor(preferences, SOURCE_BACK_COLOR);
    final Color caretColor = extractColor(preferences, SOURCE_CARET_COLOR);
    final Color fgColor = extractColor(preferences, SOURCE_FOREGROUND_COLOR);

    if (backColor != null) {
      setEdBackground(backColor);
    }
    if (caretColor != null) {
      setEdCaretColor(caretColor);
    }
    if (fgColor != null) {
      setEdForeground(fgColor);
    }
    this.setEdWordWrap(preferences.getBoolean(SOURCE_WORDWRAP, true));
    final Font sourceEditorFont =
        loadFontFromPrefs(preferences, SOURCE_FONT, DEFAULT_FONT);
    ((ScalableRsyntaxTextArea) this.getEditor()).setBaseFont(sourceEditorFont);
    this.setEdFont(sourceEditorFont);
  }

  @Override
  public void savePreferences(final Preferences preferences) {
    preferences.putInt(SOURCE_BACK_COLOR, getEdBackground().getRGB());
    preferences.putInt(SOURCE_CARET_COLOR, getEdCaretColor().getRGB());
    preferences.putInt(SOURCE_FOREGROUND_COLOR, getEdForeground().getRGB());
    preferences.putBoolean(SOURCE_WORDWRAP, getEdWordWrap());
    saveFontToPrefs(preferences, SOURCE_FONT, ((ScalableRsyntaxTextArea) editor).getBaseFont());
  }

  public boolean uncommentSelectedLines() {
    if (editor.getDocument().getLength() == 0) {
      return false;
    }

    int selectionStart = editor.getSelectionStart();
    int selectionEnd = editor.getSelectionEnd();

    if (selectionStart < 0 || selectionEnd < 0) {
      selectionStart = editor.getCaretPosition();
      selectionEnd = selectionStart;
    }

    final Element root = editor.getDocument().getDefaultRootElement();
    final int startElement = root.getElementIndex(selectionStart);
    final int endElement = root.getElementIndex(selectionEnd);

    boolean result = false;

    for (int i = startElement; i <= endElement; i++) {
      final Element elem = root.getElement(i);
      try {
        final String elementtext = elem.getDocument()
            .getText(elem.getStartOffset(), elem.getEndOffset() - elem.getStartOffset());
        if (elementtext.trim().startsWith("%")) {
          final int indexofcomment = elementtext.indexOf('%');
          if (indexofcomment >= 0) {
            elem.getDocument().remove(elem.getStartOffset() + indexofcomment, 1);
            result = true;
          }
        }
      } catch (BadLocationException ex) {
        LOGGER.throwing(this.getClass().getCanonicalName(), "uncommentSelectedLines()", ex);
      }
    }
    editor.revalidate();
    return result;
  }

  public int getLine() {
    if (this.editor instanceof RSyntaxTextArea) {
      final RSyntaxTextArea rSyntaxTextArea = (RSyntaxTextArea) this.editor;
      return rSyntaxTextArea.getCaretLineNumber();
    } else if (this.editor instanceof JTextArea) {
      try {
        final JTextArea textArea = (JTextArea) this.editor;
        int caretPos = textArea.getCaretPosition();
        return textArea.getLineOfOffset(caretPos);
      } catch (Exception ex) {
        return -1;
      }
    } else {
      return -1;
    }
  }

  public int getPos() {
    if (this.editor instanceof RSyntaxTextArea) {
      final RSyntaxTextArea rSyntaxTextArea = (RSyntaxTextArea) this.editor;
      return rSyntaxTextArea.getCaretOffsetFromLineStart();
    } else if (this.editor instanceof JTextArea) {
      try {
        final JTextArea textArea = (JTextArea) this.editor;
        int caretPos = textArea.getCaretPosition();
        int lineNum = textArea.getLineOfOffset(caretPos);
        return caretPos - textArea.getLineStartOffset(lineNum);
      } catch (Exception ex) {
        return -1;
      }
    } else {
      return -1;
    }
  }

  public boolean commentSelectedLines() {
    if (editor.getDocument().getLength() == 0) {
      return false;
    }

    int selectionStart = editor.getSelectionStart();
    int selectionEnd = editor.getSelectionEnd();

    if (selectionStart < 0 || selectionEnd < 0) {
      selectionStart = editor.getCaretPosition();
      selectionEnd = selectionStart;
    }

    final Element root = editor.getDocument().getDefaultRootElement();
    final int startElement = root.getElementIndex(selectionStart);
    final int endElement = root.getElementIndex(selectionEnd);

    boolean result = false;

    for (int i = startElement; i <= endElement; i++) {
      final Element elem = root.getElement(i);
      try {
        final String elementtext = elem.getDocument()
            .getText(elem.getStartOffset(), elem.getEndOffset() - elem.getStartOffset());
        if (!elementtext.trim().startsWith("%")) {
          elem.getDocument().insertString(elem.getStartOffset(), "%", null);
          result = true;
        }
      } catch (BadLocationException ex) {
        LOGGER.throwing(this.getClass().getCanonicalName(), "commentSelectedLines()", ex);
      }
    }
    editor.revalidate();
    return result;
  }

  @Override
  public boolean doesSupportTextCut() {
    return true;
  }

  @Override
  public boolean doesSupportTextPaste() {
    return true;
  }
}
