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
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * The class implements a small dialog windows to give possibility to custom
 * font because it is a very specialized auxiliary class, it is not described
 * very precisely
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
@SuppressWarnings("rawtypes")
public class FontChooserDialog extends javax.swing.JDialog implements ActionListener, ChangeListener {

  private static final long serialVersionUID = -1141137262767344186L;

  private static final String[] STYLES = new String[]{"PLAIN", "BOLD", "ITALIC", "BOLDI+TALIC"};
  private Font result;
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JComboBox ComboBoxFont;
  private javax.swing.JComboBox ComboBoxStyle;
  private javax.swing.JTextArea LabelPreview;
  private javax.swing.JSpinner SpinnerSize;
  private javax.swing.JButton buttonCancel;
  private javax.swing.JButton buttonOk;
  private javax.swing.JLabel jLabel1;
  private javax.swing.JLabel jLabel2;
  private javax.swing.JLabel jLabel3;
  private javax.swing.JPanel jPanel1;
  private javax.swing.JPanel jPanel2;
  private javax.swing.JScrollPane jScrollPane1;

  /**
   * Creates new form FontChooserDialog
   *
   * @param parent
   * @param title
   * @param font
   * @param testText
   */
  @SuppressWarnings({"unchecked"})
  public FontChooserDialog(final Dialog parent, final String title, Font font, String testText) {
    super(parent, true);
    initComponents();

    setTitle(title);

    if (testText == null) {
      testText = "Sample text. <?!;:,.>";
    }

    LabelPreview.setText(testText);

    if (font == null) {
      font = Font.decode(null);
    }

    ComboBoxFont.removeAllItems();

    final String[] fonts = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
    for (final String name : fonts) {
      ComboBoxFont.addItem(name);
    }

    SpinnerSize.setModel(new SpinnerNumberModel(12, 4, 100, 1));

    ComboBoxStyle.removeAllItems();
    for (final String style : STYLES) {
      ComboBoxStyle.addItem(style);
    }

    ComboBoxFont.setSelectedItem(font.getFamily());
    SpinnerSize.setValue(font.getSize());

    String style;
    switch (font.getStyle()) {
      case Font.BOLD: {
        style = "BOLD";
      }
      break;
      case Font.ITALIC: {
        style = "ITALIC";
      }
      break;
      case (Font.BOLD | Font.ITALIC): {
        style = "BOLDITALIC";
      }
      break;
      default: {
        style = "PLAIN";
      }
      break;
    }

    ComboBoxStyle.setSelectedItem(style);

    ComboBoxFont.addActionListener(this);
    ComboBoxStyle.addActionListener(this);
    SpinnerSize.addChangeListener(this);

    updateSampleFont();

    result = null;

    this.getRootPane().setDefaultButton(this.buttonOk);

    this.setSize(512, 400);

    this.setLocationRelativeTo(parent);
  }

  /**
   * This method is called from within the constructor to initialize the form.
   * WARNING: Do NOT modify this code. The content of this method is always
   * regenerated by the Form Editor.
   */
  @SuppressWarnings({"unchecked"})
  // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
  private void initComponents() {
    java.awt.GridBagConstraints gridBagConstraints;

    jPanel1 = new javax.swing.JPanel();
    jScrollPane1 = new javax.swing.JScrollPane();
    LabelPreview = new javax.swing.JTextArea();
    SpinnerSize = new javax.swing.JSpinner();
    ComboBoxFont = new javax.swing.JComboBox();
    ComboBoxStyle = new javax.swing.JComboBox();
    jLabel1 = new javax.swing.JLabel();
    jLabel2 = new javax.swing.JLabel();
    jLabel3 = new javax.swing.JLabel();
    jPanel2 = new javax.swing.JPanel();
    buttonOk = new javax.swing.JButton();
    buttonCancel = new javax.swing.JButton();

    setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
    setTitle("Font");
    setLocationByPlatform(true);
    setResizable(false);
    getContentPane().setLayout(new java.awt.GridBagLayout());

    jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Preview"));
    jPanel1.setLayout(new java.awt.BorderLayout());

    LabelPreview.setColumns(20);
    LabelPreview.setRows(5);
    jScrollPane1.setViewportView(LabelPreview);

    jPanel1.add(jScrollPane1, java.awt.BorderLayout.CENTER);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.gridwidth = 3;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
    gridBagConstraints.weightx = 1000.0;
    gridBagConstraints.weighty = 1000.0;
    gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
    getContentPane().add(jPanel1, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 8);
    getContentPane().add(SpinnerSize, gridBagConstraints);

    ComboBoxFont.setModel(new javax.swing.DefaultComboBoxModel(new String[]{"Item 1", "Item 2", "Item 3", "Item 4"}));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
    gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 16);
    getContentPane().add(ComboBoxFont, gridBagConstraints);

    ComboBoxStyle.setModel(new javax.swing.DefaultComboBoxModel(new String[]{"Item 1", "Item 2", "Item 3", "Item 4"}));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 16);
    getContentPane().add(ComboBoxStyle, gridBagConstraints);

    jLabel1.setText("Font");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 16);
    getContentPane().add(jLabel1, gridBagConstraints);

    jLabel2.setText("Style");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 16);
    getContentPane().add(jLabel2, gridBagConstraints);

    jLabel3.setText("Size");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 8);
    getContentPane().add(jLabel3, gridBagConstraints);

    jPanel2.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.RIGHT));

    buttonOk.setText("Ok");
    buttonOk.addActionListener((java.awt.event.ActionEvent evt) -> {
      buttonOkActionPerformed(evt);
    });
    jPanel2.add(buttonOk);

    buttonCancel.setText("Cancel");
    buttonCancel.addActionListener((java.awt.event.ActionEvent evt) -> {
      buttonCancelActionPerformed(evt);
    });
    jPanel2.add(buttonCancel);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 3;
    gridBagConstraints.gridwidth = 3;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    getContentPane().add(jPanel2, gridBagConstraints);

    pack();
  }// </editor-fold>//GEN-END:initComponents

  private void buttonOkActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_buttonOkActionPerformed
    result = LabelPreview.getFont();
    dispose();
  }//GEN-LAST:event_buttonOkActionPerformed

  private void buttonCancelActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_buttonCancelActionPerformed
    result = null;
    dispose();
  }//GEN-LAST:event_buttonCancelActionPerformed
  // End of variables declaration//GEN-END:variables

  public Font getResult() {
    return result;
  }

  @Override
  public void actionPerformed(final ActionEvent e) {
    updateSampleFont();
  }

  @Override
  public void stateChanged(final ChangeEvent e) {
    updateSampleFont();
  }

  private synchronized void updateSampleFont() {
    final String fontName = (String) ComboBoxFont.getSelectedItem();
    final String fontStyle = (String) ComboBoxStyle.getSelectedItem();
    final int fontSize = (Integer) SpinnerSize.getValue();

    LabelPreview.setFont(Font.decode(fontName + ' ' + fontStyle + ' ' + fontSize));
    LabelPreview.invalidate();
    LabelPreview.repaint();
  }
}
