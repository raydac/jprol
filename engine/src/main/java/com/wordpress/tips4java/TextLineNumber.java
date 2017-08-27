package com.wordpress.tips4java;

import com.igormaznitsa.prol.easygui.AbstractProlEditor;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import javax.swing.event.*;
import javax.swing.text.*;
import javax.swing.undo.UndoManager;
import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.prefs.Preferences;

/**
 * The class implements the source editor pane of the IDE.
 * As the Base for the class I have used the open sources class developed by Rob Camick and placed at http://tips4java.wordpress.com/2009/05/23/text-component-line-number/
 */
public class TextLineNumber extends AbstractProlEditor {
    /**
     * Inside logger, the logger id = "PROL_NOTE_PAD"
     */
    protected static final Logger LOG = Logger.getLogger("PROL_NOTE_PAD");
    private static final long serialVersionUID = -2223529439306867844L;
    protected UndoManager undoManager;

    public TextLineNumber() {
        super("Editor");

        editor.setContentType("text/prol");

        removePropertyFromList("EdWordWrap");

        editor.setForeground(Color.BLACK);
        editor.setBackground(Color.WHITE);
        editor.setCaretColor(Color.BLACK);
        editor.setFont(new Font("Courier", Font.BOLD, 14));

        editor.setVisible(true);

        setEnabled(true);

        undoManager = new UndoManager();

        final LineNumberComponent lineNumerator = new LineNumberComponent(editor);
        lineNumerator.setUpdateFont(true);

        scrollPane.setRowHeaderView(lineNumerator);
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
        setEdFont(loadFontFromPrefs(prefs, "sourcefont"));
    }

    @Override
    public void savePreferences(final Preferences prefs) {
        prefs.putInt("sourceedbackcolor", getEdBackground().getRGB());
        prefs.putInt("sourcecaretcolor", getEdCaretColor().getRGB());
        prefs.putInt("sourceforegroundcolor", getEdForeground().getRGB());
        prefs.putBoolean("sourcewordwrap", getEdWordWrap());
        saveFontToPrefs(prefs, "sourcefont", editor.getFont());
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

    /**
     * The component allows to add the number line counter in the editor
     */
    private static final class LineNumberComponent extends JPanel implements CaretListener, DocumentListener, PropertyChangeListener {
        public final static float LEFT = 0.0f;
        public final static float CENTER = 0.5f;
        public final static float RIGHT = 1.0f;
        private static final long serialVersionUID = -981371742373160068L;
        private final static Border OUTER = new MatteBorder(0, 0, 0, 2, Color.GRAY);
        private final static int HGHT = Integer.MAX_VALUE - 1000000;
        //  Text component this TextTextLineNumber component is in sync with
        private JTextComponent component;
        //  Properties that can be changed
        private boolean updateFont;
        private int borderGap;
        private Color currentLineForeground;
        private float digitAlignment;
        private int minimumDisplayDigits;
        //  Keep history information to reduce the number of times the component
        //  needs to be repainted
        private int lastDigits;
        private int lastHeight;
        private int lastLine;
        private HashMap<String, FontMetrics> fonts;

        /**
         * Create a line number component for a text component. This minimum display
         * width will be based on 3 digits.
         *
         * @param component the related text component
         */
        public LineNumberComponent(JTextComponent component) {
            this(component, 3);
        }

        /**
         * Create a line number component for a text component.
         *
         * @param component            the related text component
         * @param minimumDisplayDigits the number of digits used to calculate the
         *                             minimum width of the component
         */
        public LineNumberComponent(JTextComponent component, int minimumDisplayDigits) {
            this.component = component;

            setBackground(Color.LIGHT_GRAY);

            setFont(component.getFont());

            setBorderGap(5);
            setCurrentLineForeground(Color.YELLOW);
            setForeground(Color.DARK_GRAY);
            setDigitAlignment(RIGHT);
            setMinimumDisplayDigits(minimumDisplayDigits);

            component.getDocument().addDocumentListener(this);
            component.addCaretListener(this);
            component.addPropertyChangeListener("font", this);
        }

        /**
         * Gets the update font property
         *
         * @return the update font property
         */
        public boolean getUpdateFont() {
            return updateFont;
        }

        /**
         * Set the update font property. Indicates whether this Font should be
         * updated automatically when the Font of the related text component is
         * changed.
         *
         * @param updateFont when true update the Font and repaint the line numbers,
         *                   otherwise just repaint the line numbers.
         */
        public void setUpdateFont(boolean updateFont) {
            this.updateFont = updateFont;
        }

        /**
         * Gets the border gap
         *
         * @return the border gap in pixels
         */
        public int getBorderGap() {
            return borderGap;
        }

        /**
         * The border gap is used in calculating the left and right insets of the
         * border. Default value is 5.
         *
         * @param borderGap the gap in pixels
         */
        public void setBorderGap(int borderGap) {
            this.borderGap = borderGap;
            Border inner = new EmptyBorder(0, borderGap, 0, borderGap);
            setBorder(new CompoundBorder(OUTER, inner));
            lastDigits = 0;
            setPreferredWidth();
        }

        /**
         * Gets the current line rendering Color
         *
         * @return the Color used to render the current line number
         */
        public Color getCurrentLineForeground() {
            return currentLineForeground == null ? getForeground() : currentLineForeground;
        }

        /**
         * The Color used to render the current line digits. Default is Coolor.RED.
         *
         * @param currentLineForeground the Color used to render the current line
         */
        public void setCurrentLineForeground(Color currentLineForeground) {
            this.currentLineForeground = currentLineForeground;
        }

        /**
         * Gets the digit alignment
         *
         * @return the alignment of the painted digits
         */
        public float getDigitAlignment() {
            return digitAlignment;
        }

        /**
         * Specify the horizontal alignment of the digits within the component.
         * Common values would be:
         * <ul>
         * <li>TextLineNumber.LEFT
         * <li>TextLineNumber.CENTER
         * <li>TextLineNumber.RIGHT (default)
         * </ul>
         *
         * @param currentLineForeground the Color used to render the current line
         */
        public void setDigitAlignment(float digitAlignment) {
            this.digitAlignment = digitAlignment > 1.0f ? 1.0f : digitAlignment < 0.0f ? -1.0f : digitAlignment;
        }

        /**
         * Gets the minimum display digits
         *
         * @return the minimum display digits
         */
        public int getMinimumDisplayDigits() {
            return minimumDisplayDigits;
        }

        /**
         * Specify the minimum number of digits used to calculate the preferred
         * width of the component. Default is 3.
         *
         * @param minimumDisplayDigits the number digits used in the preferred width
         *                             calculation
         */
        public void setMinimumDisplayDigits(int minimumDisplayDigits) {
            this.minimumDisplayDigits = minimumDisplayDigits;
            setPreferredWidth();
        }

        /**
         * Calculate the width needed to display the maximum line number
         */
        private void setPreferredWidth() {
            Element root = component.getDocument().getDefaultRootElement();
            int lines = root.getElementCount();
            int digits = Math.max(String.valueOf(lines).length(), minimumDisplayDigits);

            //  Update sizes when number of digits in the line number changes
            if (lastDigits != digits) {
                lastDigits = digits;
                FontMetrics fontMetrics = getFontMetrics(getFont());
                int width = fontMetrics.charWidth('0') * digits;
                Insets insets = getInsets();
                int preferredWidth = insets.left + insets.right + width;

                Dimension d = getPreferredSize();
                d.setSize(preferredWidth, HGHT);
                setPreferredSize(d);
                setSize(d);
            }
        }

        /**
         * Draw the line numbers
         */
        @Override
        public void paintComponent(Graphics g) {
            super.paintComponent(g);

            //	Determine the width of the space available to draw the line number
            FontMetrics fontMetrics = component.getFontMetrics(component.getFont());
            Insets insets = getInsets();
            int availableWidth = getSize().width - insets.left - insets.right;

            //  Determine the rows to draw within the clipped bounds.
            Rectangle clip = g.getClipBounds();
            int rowStartOffset = component.viewToModel(new Point(0, clip.y));
            int endOffset = component.viewToModel(new Point(0, clip.y + clip.height));

            while (rowStartOffset <= endOffset) {
                try {
                    if (isCurrentLine(rowStartOffset)) {
                        g.setColor(getCurrentLineForeground());
                    } else {
                        g.setColor(getForeground());
                    }

                    //  Get the line number as a string and then determine the
                    //  "X" and "Y" offsets for drawing the string.
                    final String lineNumber = getTextLineNumber(rowStartOffset);
                    final int stringWidth = fontMetrics.stringWidth(lineNumber);
                    final int x = getOffsetX(availableWidth, stringWidth) + insets.left;
                    final int y = getOffsetY(rowStartOffset, fontMetrics);

                    if (y < 0) {
                        break;
                    }

                    g.drawString(lineNumber, x, y);

                    //  Move to the next row
                    rowStartOffset = Utilities.getRowEnd(component, rowStartOffset) + 1;
                } catch (final Exception e) {
                    LOG.log(Level.SEVERE, "Exception at textLineNumber", e);
                    break;
                }
            }
        }

        /*
         *  We need to know if the caret is currently positioned on the line we
         *  are about to paint so the line number can be highlighted.
         */
        private boolean isCurrentLine(int rowStartOffset) {
            int caretPosition = component.getCaretPosition();
            Element root = component.getDocument().getDefaultRootElement();

            return root.getElementIndex(rowStartOffset) == root.getElementIndex(caretPosition);
        }

        /*
         *	Get the line number to be drawn. The empty string will be returned
         *  when a line of text has wrapped.
         */
        protected final String getTextLineNumber(final int rowStartOffset) {
            final Element root = component.getDocument().getDefaultRootElement();
            final int index = root.getElementIndex(rowStartOffset);
            final Element line = root.getElement(index);

            if (line.getStartOffset() == rowStartOffset) {
                return String.valueOf(index + 1);
            } else {
                return "";
            }
        }

        /*
         *  Determine the X offset to properly align the line number when drawn
         */
        private int getOffsetX(final int availableWidth, final int stringWidth) {
            return (int) ((availableWidth - stringWidth) * digitAlignment);
        }

        /**
         * Determine the Y offset for the current row
         *
         * @return coordinate or -1 if component has not size
         */
        private int getOffsetY(final int rowStartOffset, final FontMetrics fontMetrics)
                throws BadLocationException {
            //  Get the bounding rectangle of the row

            final Rectangle r = component.modelToView(rowStartOffset);

            if (r == null) {
                return -1;
            }

            final int lineHeight = fontMetrics.getHeight();
            final int y = r.y + r.height;
            int descent = 0;

            //  The text needs to be positioned above the bottom of the bounding
            //  rectangle based on the descent of the font(s) contained on the row.
            if (r.height == lineHeight) // default font is being used
            {
                descent = fontMetrics.getDescent();
            } else // We need to check all the attributes for font changes
            {
                if (fonts == null) {
                    fonts = new HashMap<String, FontMetrics>();
                }

                final Element root = component.getDocument().getDefaultRootElement();
                final int index = root.getElementIndex(rowStartOffset);
                final Element line = root.getElement(index);

                for (int i = 0; i < line.getElementCount(); i++) {
                    final Element child = line.getElement(i);
                    final AttributeSet attrset = child.getAttributes();
                    final String fontFamily = (String) attrset.getAttribute(StyleConstants.FontFamily);
                    final Integer fontSize = (Integer) attrset.getAttribute(StyleConstants.FontSize);
                    final String key = fontFamily + fontSize;

                    FontMetrics fntmtrcs = fonts.get(key);

                    if (fntmtrcs == null) {
                        final Font font = new Font(fontFamily, Font.PLAIN, fontSize);
                        fntmtrcs = component.getFontMetrics(font);
                        fonts.put(key, fntmtrcs);
                    }

                    descent = Math.max(descent, fntmtrcs.getDescent());
                }
            }

            return y - descent;
        }

        //
//  Implement CaretListener interface
//
        @Override
        public final void caretUpdate(final CaretEvent e) {
            //  Get the line the caret is positioned on

            final int caretPosition = component.getCaretPosition();
            final Element root = component.getDocument().getDefaultRootElement();
            final int currentLine = root.getElementIndex(caretPosition);

            //  Need to repaint so the correct line number can be highlighted
            if (lastLine != currentLine) {
                repaint();
                lastLine = currentLine;
            }
        }

        //
//  Implement DocumentListener interface
//
        @Override
        public void changedUpdate(DocumentEvent e) {
            documentChanged();
        }

        @Override
        public void insertUpdate(DocumentEvent e) {
            documentChanged();
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
            documentChanged();
        }

        /*
         *  A document change may affect the number of displayed lines of text.
         *  Therefore the lines numbers will also change.
         */
        private void documentChanged() {
            //  Preferred size of the component has not been updated at the time
            //  the DocumentEvent is fired

            SwingUtilities.invokeLater(new Runnable() {

                @Override
                public void run() {
                    int preferredHeight = component.getPreferredSize().height;

                    //  Document change has caused a change in the number of lines.
                    //  Repaint to reflect the new line numbers
                    if (lastHeight != preferredHeight) {
                        setPreferredWidth();
                        repaint();
                        lastHeight = preferredHeight;
                    }
                }
            });
        }

        //
//  Implement PropertyChangeListener interface
//
        @Override
        public void propertyChange(final PropertyChangeEvent evt) {
            if (evt.getNewValue() instanceof Font) {
                if (updateFont) {
                    final Font newFont = (Font) evt.getNewValue();
                    setFont(newFont);
                    lastDigits = 0;
                    setPreferredWidth();
                } else {
                    repaint();
                }
            }
        }
    }

}
