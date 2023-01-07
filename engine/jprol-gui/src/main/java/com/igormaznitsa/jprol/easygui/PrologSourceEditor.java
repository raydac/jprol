package com.igormaznitsa.jprol.easygui;

import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.Token;
import org.fife.ui.rtextarea.RUndoManager;

import javax.swing.*;
import javax.swing.event.DocumentListener;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.undo.UndoManager;
import java.awt.*;
import java.util.logging.Logger;
import java.util.prefs.Preferences;

/**
 * The class implements the source editor pane of the IDE.
 */
public class PrologSourceEditor extends AbstractProlEditor {

  /**
   * Inside logger, the logger id = "PROL_NOTE_PAD"
   */
  protected static final Logger LOG = Logger.getLogger(PrologSourceEditor.class.getName());
  private static final long serialVersionUID = -2223529439306867844L;
  protected RUndoManager undoManager;

  public PrologSourceEditor() {
    super("Editor", new ScalableRsyntaxTextArea(), true);

    final RSyntaxTextArea theEditor = (RSyntaxTextArea) this.editor;
    theEditor.setTabsEmulated(true);
    theEditor.setSyntaxEditingStyle("text/jprol");
    theEditor.getSyntaxScheme().getStyle(Token.VARIABLE).foreground = Color.RED.darker();
    theEditor.getSyntaxScheme().getStyle(Token.VARIABLE).font = theEditor.getFont().deriveFont(Font.BOLD);

    theEditor.getInputMap().put(KeyStroke.getKeyStroke("control Z"), "none");
    theEditor.getInputMap().put(KeyStroke.getKeyStroke("control Y"), "none");

    theEditor.setAntiAliasingEnabled(true);
    theEditor.setBracketMatchingEnabled(true);
    theEditor.setCodeFoldingEnabled(true);

    removePropertyFromList("EdWordWrap");

    editor.setForeground(Color.BLACK);
    editor.setBackground(Color.WHITE);
    editor.setCaretColor(Color.BLACK);
    editor.setFont(new Font("Courier", Font.BOLD, 18));

    editor.setVisible(true);

    setEnabled(true);

    this.undoManager = new RUndoManager(theEditor);
  }

  public synchronized String getText() {
    return editor.getText();
  }

  public synchronized UndoManager getUndoManager() {
    return undoManager;
  }

  public synchronized void addUndoableEditListener(final UndoableEditListener listener) {
    editor.getDocument().addUndoableEditListener(listener);
  }

  public synchronized void addDocumentListener(final DocumentListener listener) {
    editor.getDocument().addDocumentListener(listener);
  }

  public synchronized int getCaretPosition() {
    return this.editor.getCaretPosition();
  }

  public synchronized void setCaretPosition(final int pos) {
    this.editor.setCaretPosition(pos);
    this.editor.getCaret().setVisible(true);
  }

  public synchronized void setCaretPosition(final int line, final int pos) {
    try {
      final Element rootelement = editor.getDocument().getDefaultRootElement();

      final Element element = rootelement.getElement(line - 1);
      final int offset = element.getStartOffset() + pos - 1;

      editor.setCaretPosition(offset);
      editor.requestFocus();
    } catch (Exception ex) {
      LOG.throwing(this.getClass().getCanonicalName(), "setCaretPosition()", ex);
    }
  }

  @Override
  public void loadPreferences(final Preferences prefs) {
    final Color backColor = extractColor(prefs, "sourceedbackcolor");
    final Color caretColor = extractColor(prefs, "sourcecaretcolor");
    final Color fgColor = extractColor(prefs, "sourceforegroundcolor");

    if (backColor != null) {
      setEdBackground(backColor);
    }
    if (caretColor != null) {
      setEdCaretColor(caretColor);
    }
    if (fgColor != null) {
      setEdForeground(fgColor);
    }
    setEdWordWrap(prefs.getBoolean("sourcewordwrap", true));
    setEdFont(loadFontFromPrefs(prefs, "sourcefont", this.editor.getFont()));
  }

  @Override
  public void savePreferences(final Preferences prefs) {
    prefs.putInt("sourceedbackcolor", getEdBackground().getRGB());
    prefs.putInt("sourcecaretcolor", getEdCaretColor().getRGB());
    prefs.putInt("sourceforegroundcolor", getEdForeground().getRGB());
    prefs.putBoolean("sourcewordwrap", getEdWordWrap());
    saveFontToPrefs(prefs, "sourcefont", ((ScalableRsyntaxTextArea) editor).getBaseFont());
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
        final String elementtext = elem.getDocument().getText(elem.getStartOffset(), elem.getEndOffset() - elem.getStartOffset());
        if (elementtext.trim().startsWith("%")) {
          final int indexofcomment = elementtext.indexOf('%');
          if (indexofcomment >= 0) {
            elem.getDocument().remove(elem.getStartOffset() + indexofcomment, 1);
            result = true;
          }
        }
      } catch (BadLocationException ex) {
        LOG.throwing(this.getClass().getCanonicalName(), "uncommentSelectedLines()", ex);
      }
    }
    editor.revalidate();
    return result;
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
        final String elementtext = elem.getDocument().getText(elem.getStartOffset(), elem.getEndOffset() - elem.getStartOffset());
        if (!elementtext.trim().startsWith("%")) {
          elem.getDocument().insertString(elem.getStartOffset(), "%", null);
          result = true;
        }
      } catch (BadLocationException ex) {
        LOG.throwing(this.getClass().getCanonicalName(), "commentSelectedLines()", ex);
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
