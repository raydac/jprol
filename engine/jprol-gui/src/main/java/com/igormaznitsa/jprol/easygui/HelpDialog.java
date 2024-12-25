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

import java.util.Objects;
import org.fife.ui.rtextarea.RTextScrollPane;

/**
 * The class implements the dialog window contains the help about the IDE
 * because it is a very specialized auxiliary class, it is not described very
 * precisely
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class HelpDialog extends javax.swing.JDialog {

  private static final long serialVersionUID = -7171671787513628228L;
  // Variables declaration - do not modify                     
  private javax.swing.JButton buttonClose;
  private javax.swing.JScrollPane scrollTextHelp;
  private ScalableRsyntaxTextArea textHelp;

  /**
   * Creates new form HelpDialog
   */
  public HelpDialog(final java.awt.Frame parent) {
    super(parent, true);
    initComponents();
    this.textHelp.setCaretPosition(0);
    this.setSize(512, 400);
    this.getRootPane().setDefaultButton(this.buttonClose);
  }

  private void initComponents() {
    java.awt.GridBagConstraints gridBagConstraints;

    textHelp = new ScalableRsyntaxTextArea();
    scrollTextHelp = new RTextScrollPane(textHelp, false);
    buttonClose = new javax.swing.JButton();

    setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
    setTitle("HELP");
    setLocationByPlatform(true);
    getContentPane().setLayout(new java.awt.GridBagLayout());

    textHelp.setEditable(false);
    textHelp.setHighlightCurrentLine(false);
    textHelp.setColumns(20);
    textHelp.setLineWrap(true);
    textHelp.setRows(5);
    textHelp.setText("It is a small GUI editor with JProl as executing Prolog engine. It allows to create, edit and execute small Prolog applications written in Edinburgh style. It has own predicate library providing even work with graphics and it is not fully compatible with standard official Prolog standard. Since 2014 the sources of the engine has been opened as an OSS project and published under Apache License 2.0 license.\n\nJProl Notepad has 4 main windows which provides functions:\n\n\"Editor window\"\n---------------------------\nIt is the main window of the application. It allows to enter and edit text of Prolog application. It can show and process only one source file in the same time.\n\n\"Dialog window\"\n---------------------------\nThe Window provides communication between user and started application. If the current goal of the executing program has several variants, then user can get next solution through pressing ';' key (as the system asks for it) and any other key will be interpreted as stop event. \nWarning! As User press enter key, it simulates '.' pressing and '<SPACE>' key (I made it to avoid writing '.' when the program executes the 'read/1' predicate).\n\n\"Message window\"\n---------------------------\nThe Window shows system messages gotten from the interpreter during application execution. \n\n\"Trace window\"\n---------------------------\nThe Window shows trace information about calls and traceable predicates which were marked throughr ':-trace(...).' directive during program execution. NB! The Mode dramatically decreases speed!");
    textHelp.setWrapStyleWord(true);
    scrollTextHelp.setViewportView(textHelp);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
    gridBagConstraints.weightx = 1000.0;
    gridBagConstraints.weighty = 1000.0;
    getContentPane().add(scrollTextHelp, gridBagConstraints);

    buttonClose.setIcon(new javax.swing.ImageIcon(Objects.requireNonNull(getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/cross.png")))); // NOI18N
    buttonClose.setText("Close");
    buttonClose.addActionListener(this::buttonCloseActionPerformed);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
    getContentPane().add(buttonClose, gridBagConstraints);

    pack();
  }

  private void buttonCloseActionPerformed(java.awt.event.ActionEvent evt) {
    dispose();
  }
}
