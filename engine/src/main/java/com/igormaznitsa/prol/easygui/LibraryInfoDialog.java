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

import com.igormaznitsa.prol.utils.Utils;
import java.awt.Color;
import java.awt.event.*;
import java.io.*;
import java.util.regex.*;
import javax.swing.JFrame;
import javax.swing.text.*;

/**
 * The class implements the dialog window shows the help information about prol
 * libraries because it is a very specialized auxiliary class, it is not
 * described very precisely
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public final class LibraryInfoDialog extends javax.swing.JDialog {

  private static final long serialVersionUID = 8821096803790541482L;
  private static final DefaultHighlighter.DefaultHighlightPainter highlightPainter = new DefaultHighlighter.DefaultHighlightPainter(new Color(0x00CED1));

  /**
   * Creates new form LibraryInfoDialog
   */
  public LibraryInfoDialog(final JFrame parent, final String[] libraries) throws Exception {
    super(parent, true);
    initComponents();

    textPaneLibText.setText(fillTextInfoFromLibraries(libraries));
    textPaneLibText.setCaretPosition(0);
;

    textFieldTextToSearch.requestFocus();
  }

  private String getDocText() {
    final Document doc = textPaneLibText.getDocument();
    try {
      return doc.getText(0, doc.getLength());
    }
    catch (BadLocationException ex) {
      return "";
    }
  }

  private static Pattern makePattern(final String str) {
    if (str.isEmpty()) {
      return null;
    }

    final StringBuilder buffer = new StringBuilder(str.length() << 1);

    for (final char c : str.toCharArray()) {
      if (Character.isAlphabetic(c) || Character.isDigit(c)) {
        buffer.append(c);
      }
      else {
        if (Character.isWhitespace(c)) {
          buffer.append("\\s");
        }
        else {
          switch (c) {
            case '*':
              buffer.append(".*");
              break;
            case '?':
              buffer.append(".");
              break;
            default:
              buffer.append("\\").append(c);
          }
        }
      }
    }
    return Pattern.compile(buffer.toString(), Pattern.CASE_INSENSITIVE);
  }

  private boolean searchTextFromPosition(final Matcher matcher, final int startPos) {
    textPaneLibText.getHighlighter().removeAllHighlights();

    if (matcher.find(startPos)) {
      final int foundStart = matcher.start();
      final int foundEnd = matcher.end();

      textFieldTextToSearch.setForeground(Color.green.darker());
      // find caret position
//      textPaneLibText.setCaretPosition(0);
//      textPaneLibText.setCaretPosition(foundStart);

      try{
        textPaneLibText.getHighlighter().addHighlight(foundStart, foundEnd, highlightPainter);
      }catch(BadLocationException ex){
      }

      textPaneLibText.select(foundStart,foundEnd);

      return true;
    }
    else {
      textFieldTextToSearch.setForeground(Color.red);
    }
    return false;
  }

  private void searchCurrentTextFromPosition(final boolean fromStart) {
    try {
      String textToSearch = textFieldTextToSearch.getText().trim();
      if (textToSearch.isEmpty()) {
        return;
      }

      final Pattern patternToFindText = makePattern(textToSearch);
      int curIndex = textPaneLibText.getCaretPosition();

      final Matcher matcher = patternToFindText.matcher(getDocText());

      if (!searchTextFromPosition(matcher, curIndex)) {
        searchTextFromPosition(matcher, 0);
      }
    }
    catch (Exception exx) {
      //exx.printStackTrace();
    }
  }

  public String fillTextInfoFromLibraries(final String[] libraries) throws Exception {
    final ByteArrayOutputStream baos = new ByteArrayOutputStream(16384);
    final PrintStream out = new PrintStream(baos);

    for (int li = 0; li < libraries.length; li++) {
      final Class<?> libraryClass = Class.forName(libraries[li]);
      Utils.printPredicatesForLibrary(out, libraryClass);
      if (li < libraries.length) {
        out.println(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\r\n\r\n");
      }
    }

    out.close();

    return new String(baos.toByteArray(), "UTF-8");
  }

  /**
   * This method is called from within the constructor to initialize the form.
   * WARNING: Do NOT modify this code. The content of this method is always
   * regenerated by the Form Editor.
   */
  @SuppressWarnings("unchecked")
  // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
  private void initComponents() {

    jScrollPane1 = new javax.swing.JScrollPane();
    textPaneLibText = new javax.swing.JTextPane();
    ButtonClose = new javax.swing.JButton();
    textFieldTextToSearch = new javax.swing.JTextField();
    labelTextToSearch = new javax.swing.JLabel();

    setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
    setTitle("Information about libraries");
    setLocationByPlatform(true);

    textPaneLibText.setEditable(false);
    textPaneLibText.setForeground(new java.awt.Color(0, 0, 0));
    textPaneLibText.setCaretColor(new java.awt.Color(153, 153, 255));
    jScrollPane1.setViewportView(textPaneLibText);

    ButtonClose.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/cross.png"))); // NOI18N
    ButtonClose.setText("Close");
    ButtonClose.setToolTipText("Close the dialog");
    ButtonClose.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        ButtonCloseActionPerformed(evt);
      }
    });

    textFieldTextToSearch.setFont(new java.awt.Font("Dialog", 1, 12)); // NOI18N
    textFieldTextToSearch.setToolTipText("Enter word to find and press enter (? and * wildcard characters are supported)");
    textFieldTextToSearch.addKeyListener(new java.awt.event.KeyAdapter() {
      public void keyReleased(java.awt.event.KeyEvent evt) {
        textFieldTextToSearchKeyReleased(evt);
      }
    });

    labelTextToSearch.setText("Text to search:");

    javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
    getContentPane().setLayout(layout);
    layout.setHorizontalGroup(
      layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
        .addContainerGap()
        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
          .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 717, Short.MAX_VALUE)
          .addGroup(layout.createSequentialGroup()
            .addComponent(labelTextToSearch)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(textFieldTextToSearch, javax.swing.GroupLayout.DEFAULT_SIZE, 434, Short.MAX_VALUE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(ButtonClose, javax.swing.GroupLayout.PREFERRED_SIZE, 170, javax.swing.GroupLayout.PREFERRED_SIZE)))
        .addContainerGap())
    );
    layout.setVerticalGroup(
      layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(layout.createSequentialGroup()
        .addContainerGap()
        .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 354, Short.MAX_VALUE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
          .addComponent(ButtonClose)
          .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE, false)
            .addComponent(labelTextToSearch)
            .addComponent(textFieldTextToSearch, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
        .addContainerGap())
    );

    layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {ButtonClose, labelTextToSearch, textFieldTextToSearch});

    pack();
  }// </editor-fold>//GEN-END:initComponents

    private void ButtonCloseActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_ButtonCloseActionPerformed
      setVisible(false);
    }//GEN-LAST:event_ButtonCloseActionPerformed

    private void textFieldTextToSearchKeyReleased(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_textFieldTextToSearchKeyReleased
      if (evt.getKeyCode() == KeyEvent.VK_ENTER) {
        searchCurrentTextFromPosition(false);
      }
    }//GEN-LAST:event_textFieldTextToSearchKeyReleased

  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JButton ButtonClose;
  private javax.swing.JScrollPane jScrollPane1;
  private javax.swing.JLabel labelTextToSearch;
  private javax.swing.JTextField textFieldTextToSearch;
  private javax.swing.JTextPane textPaneLibText;
  // End of variables declaration//GEN-END:variables
}
