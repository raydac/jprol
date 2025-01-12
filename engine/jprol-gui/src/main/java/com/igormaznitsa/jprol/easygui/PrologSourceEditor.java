package com.igormaznitsa.jprol.easygui;

import com.igormaznitsa.jprol.annotations.JProlConsultText;
import com.igormaznitsa.jprol.annotations.JProlOperator;
import com.igormaznitsa.jprol.annotations.JProlOperators;
import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.easygui.tokenizer.JProlTokenMaker;
import com.igormaznitsa.jprol.utils.ProlPair;
import com.igormaznitsa.jprol.utils.ProlUtils;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.io.IOException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.logging.Logger;
import java.util.prefs.Preferences;
import java.util.stream.Collectors;
import javax.swing.Box;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.ListCellRenderer;
import javax.swing.event.DocumentListener;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
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
    final List<ProlPair<String, JProlOperator>> operatorList = new ArrayList<>();
    final List<ProlPair<String, JProlPredicate>> predicateList = new ArrayList<>();

    for (final String className : MainFrame.PROL_LIBRARIES) {
      final Class<?> libClass;
      try {
        libClass = Class.forName(className);
      } catch (Exception ex) {
        throw new RuntimeException(ex);
      }

      final String libraryName = libClass.getSimpleName();

      final JProlOperator operator = libClass.getAnnotation(JProlOperator.class);
      final JProlOperators operators = libClass.getAnnotation(JProlOperators.class);
      if (operator != null) {
        operatorList.add(ProlPair.makeOf(libraryName, operator));
      }
      if (operators != null) {
        for (final JProlOperator p : operators.value()) {
          operatorList.add(ProlPair.makeOf(libraryName, p));
        }
      }

      final JProlConsultText consultText = libClass.getAnnotation(JProlConsultText.class);
      if (consultText != null) {
        for (final JProlPredicate p : consultText.declaredPredicates()) {
          predicateList.add(ProlPair.makeOf(libraryName, p));
        }
      }

      for (final Method method : libClass.getMethods()) {
        final JProlPredicate predicate = method.getAnnotation(JProlPredicate.class);
        if (predicate != null) {
          predicateList.add(ProlPair.makeOf(libraryName, predicate));
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
      final ProlPair<String, Integer> parseSignature = ProlUtils.parseSignaturePair(signature);
      final String name = parseSignature.getLeft();
      final int arity = parseSignature.getRight();
      final StringBuilder result = new StringBuilder(name);
      if (arity > 0) {
        result.append('(');
        for (int i = 0; i < arity; i++) {
          if (i > 0) {
            result.append(", ");
          }
          result.append('_');
        }
        result.append(')');
      }
      return result.toString();
    };

    final List<Completion> result = new ArrayList<>();
    final Set<JProlPredicate> predicatesAddedAsOperators = new HashSet<>();

    operatorList.forEach(op -> {
      final String libraryName = op.getLeft();
      final JProlOperator operator = op.getRight();

      final String signature = operator.name() + '/' + operator.type().getArity();

      final JProlPredicate predicate = predicateList.stream().map(ProlPair::getRight)
          .filter(x -> signature.equals(x.signature()))
          .findFirst().orElse(null);

      if (predicate != null) {
        predicatesAddedAsOperators.add(predicate);
      }

      result.add(new ProlShorthandCompletion(
          operator.name(),
          operator.type().getArity(),
          completionProvider,
          signature,
          opReplacement.apply(operator),
          libraryName,
          predicate == null ?
              String.format("type %s priority %d", operator.type().getText(), operator.priority()) :
              predicate.reference()));
    });
    predicateList.forEach(p -> {
      final String libraryName = p.getLeft();
      final JProlPredicate predicate = p.getRight();

      if (!predicatesAddedAsOperators.contains(predicate)) {

        final String signature = predicate.signature();
        final int lastIndex = signature.lastIndexOf('/');
        if (lastIndex < 0) {
          throw new IllegalArgumentException("Wrong signature format " + signature);
        }
        final String functor = signature.substring(0, lastIndex);
        final int arity;
        try {
          arity = Integer.parseInt(signature.substring(lastIndex + 1));
        } catch (Exception e) {
          throw new IllegalArgumentException(signature);
        }
        result.add(new ProlShorthandCompletion(
            functor,
            arity,
            completionProvider,
            predicate.signature(),
            replacementPred.apply(predicate.signature()),
            libraryName, predicate.reference()));
        for (final String s : predicate.synonyms()) {
          final ProlPair<String, Integer> parse = ProlUtils.parseSignaturePair(s);
          final String sfunctor = parse.getLeft();
          final int sarity = parse.getRight();
          result.add(new ProlShorthandCompletion(
                  sfunctor,
                  sarity,
                  completionProvider,
                  s,
                  replacementPred.apply(s),
                  libraryName,
                  predicate.reference()
              )
          );
        }
      }
    });
    return result;
  }

  private CompletionProvider makeCompletionProvider() {
    final DefaultCompletionProvider result = new DefaultCompletionProvider() {
      @Override
      protected List<Completion> getCompletionsImpl(JTextComponent comp) {
        final String text = getAlreadyEnteredText(comp);
        if (text.isBlank()) {
          return this.completions;
        } else {
          return this.completions.stream()
              .filter(x -> ((ProlShorthandCompletion) x).isAllow(text))
              .collect(Collectors.toList());
        }
      }
    };
    result.setListCellRenderer(new ProlShorthandListCellRenderer());
    result.addCompletions(makeShorthandCompletions(result));
    return result;
  }

  private static final class ProlShorthandListCellRenderer
      extends JPanel
      implements ListCellRenderer<Object> {

    private final JLabel signatureLabel;
    private final JLabel libraryLabel;
    private final JComponent glue;
    private final DefaultListCellRenderer defaultListCellRenderer;

    ProlShorthandListCellRenderer() {
      super(new BorderLayout(0, 0));
      this.defaultListCellRenderer = new DefaultListCellRenderer();
      this.signatureLabel = new JLabel();
      this.libraryLabel = new JLabel();
      this.libraryLabel.setOpaque(true);
      this.signatureLabel.setOpaque(true);

      this.signatureLabel.setBackground(this.defaultListCellRenderer.getBackground());
      this.signatureLabel.setForeground(this.defaultListCellRenderer.getForeground());
      this.signatureLabel.setFont(this.defaultListCellRenderer.getFont());

      this.libraryLabel.setBackground(this.defaultListCellRenderer.getBackground());
      this.libraryLabel.setForeground(this.defaultListCellRenderer.getForeground());
      this.libraryLabel.setFont(this.defaultListCellRenderer.getFont());

      this.signatureLabel.setFont(this.signatureLabel.getFont().deriveFont(Font.BOLD));
      this.libraryLabel.setFont(this.libraryLabel.getFont().deriveFont(Font.PLAIN | Font.ITALIC));
      this.add(this.signatureLabel, BorderLayout.WEST);
      this.glue = (JComponent) Box.createHorizontalGlue();
      this.glue.setOpaque(true);
      this.add(glue, BorderLayout.CENTER);
      this.add(this.libraryLabel, BorderLayout.EAST);
    }

    @Override
    public Component getListCellRendererComponent(
        JList<?> list, Object value, int index,
        boolean isSelected, boolean cellHasFocus) {

      final Component component =
          this.defaultListCellRenderer.getListCellRendererComponent(list, value, index, isSelected,
              cellHasFocus);

      this.signatureLabel.setBackground(component.getBackground());
      this.signatureLabel.setForeground(component.getForeground());
      this.libraryLabel.setBackground(component.getBackground());
      this.libraryLabel.setForeground(component.getForeground());
      this.glue.setBackground(component.getBackground());
      this.glue.setForeground(component.getForeground());

      if (value instanceof ProlShorthandCompletion) {
        final ProlShorthandCompletion completion = (ProlShorthandCompletion) value;
        this.signatureLabel.setText(' ' + completion.getInputText());
        this.libraryLabel.setText('<' + completion.getShortDescription() + "> ");
      } else {
        this.signatureLabel.setText("");
        this.libraryLabel.setText("");
      }

      return this;
    }
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

  private static final class ProlShorthandCompletion extends ShorthandCompletion {
    private final String functor;
    private final int arity;

    ProlShorthandCompletion(
        final String functor,
        final int arity,
        final CompletionProvider provider,
        final String inputText,
        final String replacementText,
        final String shortDesc,
        final String summary
    ) {
      super(provider, inputText, replacementText, shortDesc, summary);
      this.functor = functor;
      this.arity = arity;
    }

    boolean isAllow(final String text) {
      return functor.contains(text);
    }
  }
}
