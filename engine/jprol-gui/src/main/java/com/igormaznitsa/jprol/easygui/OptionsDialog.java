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

import static java.util.Objects.requireNonNull;

import com.igormaznitsa.jprol.easygui.AbstractProlEditor.PropertyLink;
import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

/**
 * The class implements a small dialog window contains the tree which allows
 * user to change values of different IDE parts because it is a very specialized
 * auxiliary class, it is not described very precisely
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class OptionsDialog extends javax.swing.JDialog implements TreeSelectionListener, TreeModel {

  private final ArrayList<TreeModel> items;
  private final ArrayList<TreeModelListener> treeModelListeners;
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private JButton buttonCloseDialog;
  private JButton buttonEditOption;
  private JScrollPane mainTreeScrollPane;
  private JTree mainOptionTree;

  /**
   * Creates new form OptionsDialog
   *
   * @param parent the parent frame, can be null
   * @param items  the tree model items, must not be null
   */
  public OptionsDialog(final JFrame parent, final TreeModel[] items) {
    super(parent, true);
    initComponents();

    treeModelListeners = new ArrayList<>();

    this.items = new ArrayList<>();
    this.items.addAll(Arrays.asList(items));

    mainOptionTree.setModel(this);
    mainOptionTree.setCellRenderer(new TreeRenderer());
    mainOptionTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    mainOptionTree.addTreeSelectionListener(this);
  }

  private void initComponents() {

    mainTreeScrollPane = new JScrollPane();
    mainOptionTree = new JTree();
    buttonEditOption = new JButton();
    buttonCloseDialog = new JButton();

    setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
    setTitle("Options");
    setLocationByPlatform(true);

    mainOptionTree.setRootVisible(false);
    mainOptionTree.addMouseListener(new java.awt.event.MouseAdapter() {
      @Override
      public void mouseClicked(java.awt.event.MouseEvent evt) {
        optionsTreeMouseClicked(evt);
      }
    });
    mainTreeScrollPane.setViewportView(mainOptionTree);

    buttonEditOption.setIcon(new ImageIcon(requireNonNull(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/cog_edit.png")))); // NOI18N
    buttonEditOption.setText("Edit");
    buttonEditOption.setToolTipText("Edit or change the selected option");
    buttonEditOption.setEnabled(false);
    buttonEditOption.addActionListener(this::buttonEditOptionActionPerformed);

    buttonCloseDialog.setIcon(new ImageIcon(requireNonNull(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/cross.png")))); // NOI18N
    buttonCloseDialog.setText("Close");
    buttonCloseDialog.setToolTipText("Close the dialog");
    buttonCloseDialog.addActionListener(this::buttonCloseDialogActionPerformed);

    javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
    getContentPane().setLayout(layout);
    layout.setHorizontalGroup(
        layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(mainTreeScrollPane)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addComponent(buttonEditOption, javax.swing.GroupLayout.PREFERRED_SIZE, 112, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(buttonCloseDialog, javax.swing.GroupLayout.PREFERRED_SIZE, 135, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap())
    );

    layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, buttonCloseDialog, buttonEditOption);

    layout.setVerticalGroup(
        layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(mainTreeScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 349,
                    Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(buttonEditOption)
                    .addComponent(buttonCloseDialog))
                .addContainerGap())
    );

    pack();
  }

  private void buttonCloseDialogActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_buttonCloseDialogActionPerformed
    dispose();
  }

  private void buttonEditOptionActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_buttonEditOptionActionPerformed
    final TreePath path = mainOptionTree.getSelectionPath();
    if (path == null) {
      return;
    }
    final AbstractProlEditor.PropertyLink prop = (PropertyLink) path.getLastPathComponent();
    final Object val = prop.getProperty();
    if (val instanceof Font) {
      final FontChooserDialog dialog = new FontChooserDialog(this, "Tune the font for '" + prop + "'", (Font) val, "?-repeat,write('Hello world'),nl,fail.\r\n:-X is 2*2,write(X).");
      dialog.setVisible(true);
      final Font font = dialog.getResult();
      if (font != null) {
        prop.setProperty(font);
      }
    } else if (val instanceof Color) {
      final Color color = (Color) val;
      JColorChooser chooser = new JColorChooser(color);

      final boolean[] array = new boolean[1];

      ActionListener actionListenerOk = (ActionEvent e) -> array[0] = true;

      JDialog colorChooser = JColorChooser.createDialog(this, "Choose color for '" + prop + "'", true, chooser, actionListenerOk, null);
      colorChooser.setVisible(true);
      if (array[0]) {
        prop.setProperty(chooser.getColor());
      }
    } else if (val instanceof Boolean) {
      prop.setProperty(!(Boolean) val);
    }

    firePropertyChanged(path);
  }

  private void optionsTreeMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_optionsTreeMouseClicked
    final JTree tree = (JTree) evt.getSource();
    int selRow = tree.getRowForLocation(evt.getX(), evt.getY());
    final TreePath selPath = tree.getPathForLocation(evt.getX(), evt.getY());
    if (selRow != -1) {
      if (evt.getClickCount() == 2) {
        if (buttonEditOption.isEnabled()) {
          buttonEditOption.doClick();
        }
      }
    }
  }

  @Override
  public void valueChanged(final TreeSelectionEvent e) {
    TreePath path = mainOptionTree.getSelectionPath();
    if (path != null) {
      buttonEditOption.setEnabled(path.getLastPathComponent() instanceof AbstractProlEditor.PropertyLink);
      return;
    }

    buttonEditOption.setEnabled(false);
  }

  @Override
  public Object getRoot() {
    return this;
  }

  @Override
  public Object getChild(Object parent, int index) {
    if (this.equals(parent)) {
      return items.get(index);
    } else {
      return ((TreeModel) parent).getChild(parent, index);
    }
  }

  @Override
  public int getChildCount(Object parent) {
    if (this.equals(parent)) {
      return items.size();
    } else {
      return ((TreeModel) parent).getChildCount(parent);
    }
  }

  @Override
  public boolean isLeaf(final Object node) {
    return node instanceof AbstractProlEditor.PropertyLink;
  }

  @Override
  public void valueForPathChanged(TreePath path, Object newValue) {
  }

  @Override
  public int getIndexOfChild(Object parent, Object child) {
    if (this.equals(parent)) {
      return items.indexOf(child);
    } else {
      return ((TreeModel) parent).getIndexOfChild(parent, child);
    }

  }

  private void firePropertyChanged(TreePath path) {
    for (TreeModelListener listener : treeModelListeners) {
      listener.treeNodesChanged(new TreeModelEvent(this, path));
    }
  }

  @Override
  public void addTreeModelListener(TreeModelListener l) {
    treeModelListeners.add(l);
  }

  @Override
  public void removeTreeModelListener(TreeModelListener l) {
    treeModelListeners.remove(l);
  }

  private static final class TreeRenderer extends DefaultTreeCellRenderer {

    private static final Icon ICON_CLOSE = UiUtils.loadIcon("folder_wrench");
    private static final Icon ICON_OPEN = UiUtils.loadIcon("folder");
    private static final Icon ICON_LEAF = UiUtils.loadIcon("wrench");

    @Override
    public Icon getLeafIcon() {
      return ICON_LEAF;
    }

    @Override
    public Icon getClosedIcon() {
      return ICON_CLOSE;
    }

    @Override
    public Icon getOpenIcon() {
      return ICON_OPEN;
    }
  }

}
