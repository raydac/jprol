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

import com.igormaznitsa.prol.containers.KnowledgeBase;
import com.igormaznitsa.prol.logic.ProlContext;

import java.io.CharArrayWriter;
import java.io.PrintWriter;

/**
 * The class implements the dialog window which allows user to take a look at
 * the snapshot of the current knowledge base state because it is a very
 * specialized auxiliary class, it is not described very precisely
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class KnowledgeBaseSnapshotViewDialog extends javax.swing.JDialog {
    private static final long serialVersionUID = -4927120918645871928L;
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton ButtonClose;
    private com.igormaznitsa.prol.easygui.PrologSourceEditor SnapshotViewer;

    /**
     * Creates new form KnowledgeBaseSnapshotViewDialog
     */
    public KnowledgeBaseSnapshotViewDialog(final java.awt.Frame parent, final ProlContext context) {
        super(parent, true);
        initComponents();
        SnapshotViewer.getEditor().setEditable(false);
        SnapshotViewer.setTitle("Knowledge base");

        if (context != null) {
            KnowledgeBase base = context.getKnowledgeBase();
            CharArrayWriter charArray = new CharArrayWriter(8096);
            PrintWriter writer = new PrintWriter(charArray, true);
            base.write(writer);
            writer.close();
            SnapshotViewer.getEditor().setText(charArray.toString());
        }

        this.getRootPane().setDefaultButton(this.ButtonClose);
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        SnapshotViewer = new com.igormaznitsa.prol.easygui.PrologSourceEditor();
        ButtonClose = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Current or last knowledge base snapshot");
        getContentPane().setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1000.0;
        gridBagConstraints.weighty = 1000.0;
        getContentPane().add(SnapshotViewer, gridBagConstraints);

        ButtonClose.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/cross.png"))); // NOI18N
        ButtonClose.setText("Close");
        ButtonClose.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                ButtonCloseActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 8;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        getContentPane().add(ButtonClose, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void ButtonCloseActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_ButtonCloseActionPerformed
        dispose();
    }//GEN-LAST:event_ButtonCloseActionPerformed
    // End of variables declaration//GEN-END:variables

}
