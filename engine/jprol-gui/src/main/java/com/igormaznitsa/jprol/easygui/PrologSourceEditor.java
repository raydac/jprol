package com.igormaznitsa.jprol.easygui;

import com.igormaznitsa.jprol.annotations.JProlConsultText;
import com.igormaznitsa.jprol.annotations.JProlOperator;
import com.igormaznitsa.jprol.annotations.JProlOperators;
import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.easygui.tokenizer.JProlTokenMaker;
import com.igormaznitsa.jprol.utils.ProlPair;
import com.igormaznitsa.jprol.utils.ProlUtils;
import com.igormaznitsa.prologparser.DefaultParserContext;
import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.PrologParser;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.tokenizer.Op;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.io.IOException;
import java.io.StringReader;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.logging.Logger;
import java.util.prefs.Preferences;
import java.util.stream.Collectors;
import java.util.stream.Stream;
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
      LocalFontContainer.LOCAL_JET_BRAINS_MONO.getFont().deriveFont(Font.PLAIN, 18);
  /**
   * Inside logger, the logger id = "PROL_NOTE_PAD"
   */
  protected static final Logger LOGGER = Logger.getLogger(PrologSourceEditor.class.getName());
  protected RUndoManager undoManager;


  public PrologSourceEditor() {
    super("Editor", new ScalableRsyntaxTextArea(JProlTokenMaker.MIME), true, true);
    replacePropertyLink(PROPERTY_ED_FONT, new PropertyLink(this, "Font", "EdAndBaseFont"));

    final ScalableRsyntaxTextArea theEditor = (ScalableRsyntaxTextArea) this.editor;
    theEditor.setAutoscrolls(true);
    theEditor.setTabsEmulated(true);
    theEditor.setMarkOccurrences(true);

    this.applyScheme(theEditor);

    if (UiUtils.isMacOs()) {
      theEditor.getInputMap().put(KeyStroke.getKeyStroke("meta Z"), "none");
      theEditor.getInputMap().put(KeyStroke.getKeyStroke("meta Y"), "none");
    } else {
      theEditor.getInputMap().put(KeyStroke.getKeyStroke("control Z"), "none");
      theEditor.getInputMap().put(KeyStroke.getKeyStroke("control Y"), "none");
    }

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
      return makeReplacementText(name, arity);
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

  private static String makeReplacementText(final String functor, final int arity) {
    if (arity == 0) {
      return functor;
    }
    final StringBuilder buffer = new StringBuilder();
    buffer.append(functor).append('(');
    for (int i = 0; i < arity; i++) {
      if (i > 0) {
        buffer.append(',');
      }
      buffer.append('_');
    }
    buffer.append(')');
    return buffer.toString();
  }

  private CompletionProvider makeCompletionProvider() {
    final DefaultCompletionProvider result = new DefaultCompletionProvider() {

      private final List<Completion> additionalCompletions = new ArrayList<>();
      private final ParserContext parserContext = DefaultParserContext.of(
          ParserContext.FLAG_BLOCK_COMMENTS | ParserContext.FLAG_ZERO_STRUCT, Op.SWI);
      private final Map<String, PrologStruct> foundSignaturesInSources = new HashMap<>();

      @Override
      public List<Completion> getCompletions(final JTextComponent comp) {
        this.additionalCompletions.clear();

        final String sources = comp.getText();

        this.foundSignaturesInSources.clear();
        try (final PrologParser parser = new GenericPrologParser(new StringReader(sources),
            this.parserContext)) {
          while (parser.hasNext()) {
            final PrologTerm term = parser.next();
            if (term == null) {
              break;
            }
            PrologStruct foundStruct = null;
            if (term.getType() == TermType.STRUCT) {
              if (term.getArity() == 2 && ":-".equals(term.getFunctor().getText())) {
                final PrologStruct struct = (PrologStruct) term;
                final PrologTerm leftPart = struct.getTermAt(0);
                if (leftPart.getType() == TermType.STRUCT) {
                  foundStruct = (PrologStruct) leftPart;
                }
              } else {
                foundStruct = (PrologStruct) term;
              }
            }

            if (foundStruct != null) {
              final String signature =
                  foundStruct.getFunctor().getText() + '/' + foundStruct.getArity();
              this.foundSignaturesInSources.put(signature, foundStruct);
            }
          }
        } catch (Exception ex) {
          // do nothing in the case
        }

        this.foundSignaturesInSources.forEach((s, p) -> this.additionalCompletions.add(
            new AdditionalProlShorthandCompletion(p.getFunctor().getText(), p.getArity(), this, s,
                makeReplacementText(p.getFunctor().getText(), p.getArity()),
                "editor",
                "Sources at " + p.getLine() + ':' + p.getPos())));

        return super.getCompletions(comp);
      }

      @Override
      protected List<Completion> getCompletionsImpl(final JTextComponent comp) {
        final String text = getAlreadyEnteredText(comp);
        if (text.isBlank()) {
          final List<Completion> result = new ArrayList<>();
          result.addAll(this.additionalCompletions);
          result.addAll(this.completions);
          return result;
        } else {
          return Stream.concat(this.additionalCompletions.stream()
                      .filter(x -> ((AdditionalProlShorthandCompletion) x).isAllow(text)),
                  this.completions.stream()
                      .filter(x -> ((ProlShorthandCompletion) x).isAllow(text)))
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

      if (value instanceof AdditionalProlShorthandCompletion) {
        final AdditionalProlShorthandCompletion completion =
            (AdditionalProlShorthandCompletion) value;
        this.signatureLabel.setText(' ' + completion.getInputText());
        this.libraryLabel.setText(completion.getShortDescription());

        this.signatureLabel.setBackground(this.signatureLabel.getBackground().darker());
        this.libraryLabel.setBackground(this.libraryLabel.getBackground().darker());
        this.glue.setBackground(this.glue.getBackground().darker());

      } else if (value instanceof ProlShorthandCompletion) {
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

  @Override
  public boolean doesSupportTextCut() {
    return true;
  }

  @Override
  public boolean doesSupportTextPaste() {
    return true;
  }

  private static final class AdditionalProlShorthandCompletion extends ProlShorthandCompletion {
    AdditionalProlShorthandCompletion(
        final String functor,
        final int arity,
        final CompletionProvider provider,
        final String signatureText,
        final String replacementText, String shortDesc,
        final String summary) {
      super(functor, arity, provider, signatureText, replacementText, shortDesc, summary);
    }
  }

  private static class ProlShorthandCompletion extends ShorthandCompletion {
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

  public void toggleComment() {
    final RSyntaxTextArea textArea = (RSyntaxTextArea) this.editor;
    try {
      int start = textArea.getSelectionStart();
      int end = textArea.getSelectionEnd();

      if (start == end) {
        // No selection, process the current line
        int line = textArea.getCaretLineNumber();
        int lineStart = textArea.getLineStartOffset(line);
        int lineEnd = textArea.getLineEndOffset(line);
        String lineText = textArea.getText(lineStart, lineEnd - lineStart);

        if (lineText.trim().startsWith("%")) {
          // Uncomment
          textArea.replaceRange(lineText.replaceFirst("%", ""), lineStart, lineEnd);
        } else {
          // Comment
          textArea.insert("%", lineStart);
        }
      } else {
        // Process selected lines
        int startLine = textArea.getLineOfOffset(start);
        int endLine = textArea.getLineOfOffset(end);

        for (int i = startLine; i <= endLine; i++) {
          int lineStart = textArea.getLineStartOffset(i);
          int lineEnd = textArea.getLineEndOffset(i);
          String lineText = textArea.getText(lineStart, lineEnd - lineStart);

          if (lineText.trim().startsWith("%")) {
            // Uncomment
            textArea.replaceRange(lineText.replaceFirst("%", ""), lineStart, lineEnd);
          } else {
            // Comment
            textArea.insert("%", lineStart);
          }
        }
      }
    } catch (BadLocationException e) {
      // do nothing
    }
  }
}
