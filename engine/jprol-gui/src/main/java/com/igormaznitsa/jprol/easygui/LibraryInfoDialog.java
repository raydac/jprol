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

import org.fife.ui.rtextarea.RTextScrollPane;
import org.fife.ui.rtextarea.SearchContext;
import org.fife.ui.rtextarea.SearchEngine;

import javax.swing.*;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.Objects;

/**
 * The class implements the dialog window shows the help information about prol
 * libraries because it is a very specialized auxiliary class, it is not
 * described very precisely
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public final class LibraryInfoDialog extends javax.swing.JDialog {

  private static final long serialVersionUID = 8821096803790541482L;
  private javax.swing.JButton ButtonClose;
  private javax.swing.JScrollPane textPaneScroll;
  private javax.swing.JLabel labelTextToSearch;
  private javax.swing.JTextField textFieldTextToSearch;
  private ScalableRsyntaxTextArea textPaneLibText;

  /**
   * Creates new form LibraryInfoDialog
   */
  public LibraryInfoDialog(final JFrame parent, final String[] libraries) throws Exception {
    super(parent, true);
    initComponents();

    textPaneLibText.setText(fillTextInfoFromLibraries(libraries));
    textPaneLibText.setCaretPosition(0);
    textFieldTextToSearch.requestFocus();

    this.ButtonClose.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "CloseForm");
    this.ButtonClose.getActionMap().put("CloseForm", new AbstractAction() {
      private static final long serialVersionUID = -5644390861803492172L;

      @Override
      public void actionPerformed(final ActionEvent e) {
        ButtonClose.doClick();
      }
    });
  }

  private String getDocText() {
    final Document doc = textPaneLibText.getDocument();
    try {
      return doc.getText(0, doc.getLength());
    } catch (BadLocationException ex) {
      return "";
    }
  }

  private void searchCurrentTextFromPosition() {
    final SearchContext searchContext = new SearchContext(textFieldTextToSearch.getText().trim(), false);
    searchContext.setMarkAll(true);
    searchContext.setSearchForward(true);
    if (SearchEngine.find(textPaneLibText, searchContext).wasFound()) {
      textPaneLibText.repaint();
    }
  }

  public String fillTextInfoFromLibraries(final String[] libraries) throws Exception {
    final ByteArrayOutputStream baos = new ByteArrayOutputStream(16384);
    try (PrintStream out = new PrintStream(baos)) {
      for (String library : libraries) {
        final Class<?> libraryClass = Class.forName(library);
        UiUtils.printPredicatesForLibrary(out, libraryClass);
        out.println(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n\n");
      }
    }

    return new String(baos.toByteArray(), StandardCharsets.UTF_8);
  }

  private void initComponents() {
    java.awt.GridBagConstraints gridBagConstraints;

    textPaneLibText = new ScalableRsyntaxTextArea();
    textPaneScroll = new RTextScrollPane(textPaneLibText, false);
    textPaneLibText.setEditable(false);
    textPaneLibText.setHighlightCurrentLine(false);

    ButtonClose = new javax.swing.JButton();
    textFieldTextToSearch = new javax.swing.JTextField();
    labelTextToSearch = new javax.swing.JLabel();

    setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
    setTitle("Information about libraries");
    setLocationByPlatform(true);
    getContentPane().setLayout(new java.awt.GridBagLayout());

    textPaneLibText.setEditable(false);
    textPaneLibText.setForeground(new java.awt.Color(0, 0, 0));
    textPaneLibText.setCaretColor(new java.awt.Color(153, 153, 255));
    textPaneScroll.setViewportView(textPaneLibText);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.gridwidth = 3;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 1000.0;
    gridBagConstraints.weighty = 1000.0;
    getContentPane().add(textPaneScroll, gridBagConstraints);

    ButtonClose.setIcon(new javax.swing.ImageIcon(Objects.requireNonNull(getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/cross.png")))); // NOI18N
    ButtonClose.setText("Close");
    ButtonClose.setToolTipText("Close the dialog");
    ButtonClose.addActionListener(this::ButtonCloseActionPerformed);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
    gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 0);
    getContentPane().add(ButtonClose, gridBagConstraints);

    textFieldTextToSearch.setFont(new java.awt.Font("Dialog", Font.BOLD, 12)); // NOI18N
    textFieldTextToSearch.setToolTipText("Enter word to find and press enter (? and * wildcard characters are supported)");
    textFieldTextToSearch.addKeyListener(new java.awt.event.KeyAdapter() {
      @Override
      public void keyReleased(java.awt.event.KeyEvent evt) {
        textFieldTextToSearchKeyReleased(evt);
      }
    });
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
    gridBagConstraints.weightx = 1000.0;
    getContentPane().add(textFieldTextToSearch, gridBagConstraints);

    labelTextToSearch.setText("Text to search:");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
    gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 8);
    getContentPane().add(labelTextToSearch, gridBagConstraints);

    pack();
  }

  private void ButtonCloseActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_ButtonCloseActionPerformed
    setVisible(false);
  }

  private void textFieldTextToSearchKeyReleased(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_textFieldTextToSearchKeyReleased
    if (evt.getKeyCode() == KeyEvent.VK_ENTER) {
      searchCurrentTextFromPosition();
    }
  }
}
