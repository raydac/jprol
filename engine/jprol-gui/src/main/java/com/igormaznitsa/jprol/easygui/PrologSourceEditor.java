package com.igormaznitsa.jprol.easygui;

import static com.igormaznitsa.jprol.easygui.UiUtils.ellipseRight;

import com.igormaznitsa.jprol.annotations.JProlConsultText;
import com.igormaznitsa.jprol.annotations.JProlOperator;
import com.igormaznitsa.jprol.annotations.JProlOperators;
import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.easygui.tokenizer.JProlTokenMaker;
import java.awt.Color;
import java.awt.Font;
import java.io.IOException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.logging.Logger;
import java.util.prefs.Preferences;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.event.DocumentListener;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.undo.UndoManager;
import org.fife.ui.autocomplete.AutoCompletion;
import org.fife.ui.autocomplete.Completion;
import org.fife.ui.autocomplete.CompletionProvider;
import org.fife.ui.autocomplete.DefaultCompletionProvider;
import org.fife.ui.autocomplete.ShorthandCompletion;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.Theme;
import org.fife.ui.rtextarea.RUndoManager;

/**
 * The class implements the source editor pane of the IDE.
 */
public class PrologSourceEditor extends AbstractProlEditor {

  public static final String PROPERTY_SOURCE_WORDWRAP = "sourcewordwrap";
  public static final String PROPERTY_SOURCE_FOREGROUND_COLOR = "sourceforegroundcolor";
  public static final String PROPERTY_SOURCE_CARET_COLOR = "sourcecaretcolor";
  public static final String PROPERTY_SOURCE_BACK_COLOR = "sourceedbackcolor";
  public static final String PROPERTY_SOURCE_FONT = "sourcefont";
  public static final Font DEFAULT_SORCE_FONT =
      LocalFont.LOCAL_JET_BRAINS_MONO.getFont().deriveFont(Font.PLAIN, 18);
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

    theEditor.setFont(DEFAULT_SORCE_FONT);
    theEditor.setBaseFont(DEFAULT_SORCE_FONT);

    final AutoCompletion autoCompletion =
        new AutoCompletion(this.makeCompletionProvider());
    autoCompletion.setAutoActivationEnabled(false);
    autoCompletion.setParameterAssistanceEnabled(false);
    autoCompletion.setShowDescWindow(true);
    autoCompletion.setAutoCompleteSingleChoices(false);
    autoCompletion.install(this.editor);

    editor.setVisible(true);

    setEnabled(true);

    this.undoManager = new RUndoManager(theEditor);
  }

  private static List<Completion> makeShorthandCompletions(
      final CompletionProvider completionProvider) {
    final List<JProlOperator> operatorList = new ArrayList<>();
    final List<JProlPredicate> predicateList = new ArrayList<>();

    for (final String className : MainFrame.PROL_LIBRARIES) {
      final Class<?> libClass;
      try {
        libClass = Class.forName(className);
      } catch (Exception ex) {
        throw new RuntimeException(ex);
      }

      final JProlOperator operator = libClass.getAnnotation(JProlOperator.class);
      final JProlOperators operators = libClass.getAnnotation(JProlOperators.class);
      if (operator != null) {
        operatorList.add(operator);
      }
      if (operators != null) {
        Collections.addAll(operatorList, operators.value());
      }

      final JProlConsultText consultText = libClass.getAnnotation(JProlConsultText.class);
      if (consultText != null) {
        predicateList.addAll(Arrays.asList(consultText.declaredPredicates()));
      }

      for (final Method method : libClass.getMethods()) {
        final JProlPredicate predicate = method.getAnnotation(JProlPredicate.class);
        if (predicate != null) {
          predicateList.add(predicate);
        }
      }
    }

    final Function<JProlOperator, String> opReplacement = op -> {
      String left = "";
      String right = "";
      switch (op.type()) {
        case XFX: {
          left = "X ";
          right = " Y";
        }
        break;
        case YFX: {
          left = "Y ";
          right = " X";
        }
        break;
        case XFY: {
          left = "X";
          right = " Y";
        }
        break;
        case FX: {
          right = " X";
        }
        break;
        case FY: {
          right = " Y";
        }
        break;
        case YF: {
          left = "Y ";
        }
        break;
        case XF: {
          left = "X ";
        }
        break;
      }
      return String.format("%s%s%s", left, op.name(), right);
    };

    final Function<String, String> replacementPred = (signature) -> {
      final int lastIndex = signature.lastIndexOf('/');
      if (lastIndex < 0) {
        throw new IllegalArgumentException("Wrong signature format " + signature);
      }
      final String name = signature.substring(0, lastIndex);
      final int arity;
      try {
        arity = Integer.parseInt(signature.substring(lastIndex + 1));
      } catch (Exception e) {
        throw new IllegalArgumentException(signature);
      }
      final StringBuilder result = new StringBuilder(name);
      if (arity > 0) {
        result.append('(');
        for (int i = 0; i < arity; i++) {
          if (i > 0) {
            result.append(". ");
          }
          result.append('_');
        }
        result.append(')');
      }
      return result.toString();
    };

    final List<Completion> result = new ArrayList<>();
    operatorList.forEach(op -> {
      result.add(new ShorthandCompletion(completionProvider, op.name() + '/' + op.type().getArity(),
          opReplacement.apply(op)));
    });
    predicateList.forEach(p -> {
      result.add(new ShorthandCompletion(completionProvider, p.signature(),
          replacementPred.apply(p.signature()), "", p.reference()));
      for (final String s : p.synonyms()) {
        result.add(new ShorthandCompletion(completionProvider, s, replacementPred.apply(s),
            ellipseRight(p.reference(), 32, "..."),
            p.reference()));
      }
    });
    return result;
  }

  private CompletionProvider makeCompletionProvider() {
    final DefaultCompletionProvider result = new DefaultCompletionProvider();
    result.addCompletions(makeShorthandCompletions(result));
    return result;
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
    final Color backColor = extractColor(preferences, PROPERTY_SOURCE_BACK_COLOR);
    final Color caretColor = extractColor(preferences, PROPERTY_SOURCE_CARET_COLOR);
    final Color fgColor = extractColor(preferences, PROPERTY_SOURCE_FOREGROUND_COLOR);

    if (backColor != null) {
      this.setEdBackground(backColor);
    }
    if (caretColor != null) {
      this.setEdCaretColor(caretColor);
    }
    if (fgColor != null) {
      this.setEdForeground(fgColor);
    }
    this.setEdWordWrap(preferences.getBoolean(PROPERTY_SOURCE_WORDWRAP, true));
    this.setEdAndBaseFont(loadFontFromPrefs(preferences, PROPERTY_SOURCE_FONT, DEFAULT_SORCE_FONT));
  }

  @Override
  public void savePreferences(final Preferences preferences) {
    preferences.putInt(PROPERTY_SOURCE_BACK_COLOR, getEdBackground().getRGB());
    preferences.putInt(PROPERTY_SOURCE_CARET_COLOR, getEdCaretColor().getRGB());
    preferences.putInt(PROPERTY_SOURCE_FOREGROUND_COLOR, getEdForeground().getRGB());
    preferences.putBoolean(PROPERTY_SOURCE_WORDWRAP, getEdWordWrap());
    saveFontToPrefs(preferences, PROPERTY_SOURCE_FONT,
        ((ScalableRsyntaxTextArea) editor).getBaseFont());
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
        final String elementText = elem.getDocument()
            .getText(elem.getStartOffset(), elem.getEndOffset() - elem.getStartOffset());
        if (!elementText.trim().startsWith("%")) {
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
