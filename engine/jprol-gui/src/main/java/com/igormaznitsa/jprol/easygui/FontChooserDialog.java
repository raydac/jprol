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

import java.awt.Component;
import java.awt.Dialog;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Stream;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTextArea;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * The class implements a small dialog windows to give possibility to custom
 * font because it is a very specialized auxiliary class, it is not described
 * very precisely
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
@SuppressWarnings("rawtypes")
public class FontChooserDialog extends javax.swing.JDialog
    implements ActionListener, ChangeListener {

  private static final String[] STYLES = new String[] {"PLAIN", "BOLD", "ITALIC", "BOLD+ITALIC"};
  private Font result;

  private JComboBox<Font> comboBoxFont;
  private JComboBox<String> comboBoxStyle;
  private JTextArea labelPreview;
  private JSpinner spinnerSize;
  private JButton buttonCancel;
  private JButton buttonOk;
  private JLabel jLabel1;
  private JLabel jLabel2;
  private JLabel jLabel3;
  private JPanel jPanel1;
  private JPanel jPanel2;
  private JScrollPane labelScrollPane;

  /**
   * Creates new form FontChooserDialog
   *
   * @param parent   parent component
   * @param title    dialog title
   * @param font     font to be processed
   * @param testText test text string
   */
  @SuppressWarnings({"unchecked"})
  public FontChooserDialog(final Dialog parent, final String title, final Font font,
                           String testText) {
    super(parent, true);
    initComponents();

    setTitle(title);

    if (testText == null) {
      testText = "Sample text. <?!;:,.>";
    }

    this.labelPreview.setText(testText);
    this.comboBoxFont.removeAllItems();

    final LocalFontContainer localFontContainer =
        font instanceof LocalFontContainer.ResourceBasedFont ?
            ((LocalFontContainer.ResourceBasedFont) font).getParent() : null;

    LocalFontContainer.VALUES.forEach(x -> {
      if (x == localFontContainer) {
        comboBoxFont.addItem(font);
      } else {
        comboBoxFont.addItem(x.getFont());
      }
    });

    final Set<String> duplicationSet = new HashSet<>();
    Stream.of(GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames())
        .map(Font::decode)
        .forEach(x -> {
          final String family = x.getFamily();
          if (duplicationSet.add(family)) {
            this.comboBoxFont.addItem(x);
          }
        });

    this.comboBoxFont.setRenderer(new DefaultListCellRenderer() {
      @Override
      public Component getListCellRendererComponent(JList<?> list, Object value, int index,
                                                    boolean isSelected, boolean cellHasFocus) {
        final JLabel label =
            (JLabel) super.getListCellRendererComponent(list, value, index, isSelected,
                cellHasFocus);
        final Font font = (Font) value;
        if (font instanceof LocalFontContainer.ResourceBasedFont) {
          label.setFont(label.getFont().deriveFont(Font.BOLD));
          label.setText(((LocalFontContainer.ResourceBasedFont) font).getLocalFont().getTitle());
        } else {
          label.setText(font.getFontName());
        }
        return label;
      }
    });

    spinnerSize.setModel(new SpinnerNumberModel(12, 4, 100, 1));

    comboBoxStyle.removeAllItems();
    for (final String style : STYLES) {
      comboBoxStyle.addItem(style);
    }

    comboBoxFont.setSelectedItem(font);
    spinnerSize.setValue(font.getSize());
    comboBoxStyle.setSelectedItem(LocalFontContainer.styleAsString(font));

    comboBoxFont.addActionListener(this);
    comboBoxStyle.addActionListener(this);
    spinnerSize.addChangeListener(this);

    updateSampleFont();

    result = null;

    this.getRootPane().setDefaultButton(this.buttonOk);

    this.pack();

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

    jPanel1 = new JPanel();
    labelScrollPane = new JScrollPane();
    labelPreview = new JTextArea();
    spinnerSize = new JSpinner();
    comboBoxFont = new JComboBox<Font>();
    comboBoxStyle = new JComboBox<String>();
    jLabel1 = new JLabel();
    jLabel2 = new JLabel();
    jLabel3 = new JLabel();
    jPanel2 = new JPanel();
    buttonOk = new JButton();
    buttonCancel = new JButton();

    setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
    setTitle("Font");
    setLocationByPlatform(true);
    setResizable(false);
    getContentPane().setLayout(new java.awt.GridBagLayout());

    jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Preview"));
    jPanel1.setLayout(new java.awt.BorderLayout());

    labelPreview.setColumns(20);
    labelPreview.setRows(5);
    labelScrollPane.setViewportView(labelPreview);

    jPanel1.add(labelScrollPane, java.awt.BorderLayout.CENTER);

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
    getContentPane().add(spinnerSize, gridBagConstraints);

    comboBoxFont.setModel(new javax.swing.DefaultComboBoxModel(
        new String[] {"Item 1", "Item 2", "Item 3", "Item 4"}));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
    gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 16);
    getContentPane().add(comboBoxFont, gridBagConstraints);

    comboBoxStyle.setModel(new javax.swing.DefaultComboBoxModel(
        new String[] {"Item 1", "Item 2", "Item 3", "Item 4"}));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 16);
    getContentPane().add(comboBoxStyle, gridBagConstraints);

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
    buttonOk.addActionListener(this::buttonOkActionPerformed);
    jPanel2.add(buttonOk);

    buttonCancel.setText("Cancel");
    buttonCancel.addActionListener(this::buttonCancelActionPerformed);
    jPanel2.add(buttonCancel);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 3;
    gridBagConstraints.gridwidth = 3;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    getContentPane().add(jPanel2, gridBagConstraints);

    pack();
  }// </editor-fold>//GEN-END:initComponents

  private void buttonOkActionPerformed(java.awt.event.ActionEvent evt) {
    result = labelPreview.getFont();
    dispose();
  }

  private void buttonCancelActionPerformed(java.awt.event.ActionEvent evt) {
    result = null;
    dispose();
  }

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

  private void updateSampleFont() {
    final Font selectedFont = (Font) comboBoxFont.getSelectedItem();
    final String fontStyle = (String) comboBoxStyle.getSelectedItem();
    final int fontSize = (Integer) spinnerSize.getValue();
    final Font font = selectedFont.deriveFont(LocalFontContainer.decodeStyle(fontStyle), fontSize);
    labelPreview.setFont(font);
    labelPreview.invalidate();
    labelPreview.repaint();
  }
}
