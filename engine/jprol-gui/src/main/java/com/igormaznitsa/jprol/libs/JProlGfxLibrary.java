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

package com.igormaznitsa.jprol.libs;

import static com.igormaznitsa.jprol.data.TermType.VAR;
import static java.util.Objects.requireNonNull;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.easygui.MainFrame;
import com.igormaznitsa.jprol.easygui.UiUtils;
import com.igormaznitsa.jprol.exceptions.ProlPermissionErrorException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.WeakHashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.filechooser.FileFilter;

public final class JProlGfxLibrary extends AbstractJProlLibrary implements WindowListener, ActionListener {

  private static final Logger LOGGER = Logger.getLogger(JProlGfxLibrary.class.getName());
  private final WeakHashMap<JMenuItem, RegisteredAction> registeredActions = new WeakHashMap<>();
  private final JFrame graphicFrame;
  private final JLabel label;
  private final ReentrantLock internalLocker = new ReentrantLock();
  private final JMenu bindedMenu = new JMenu("Actions");
  private final JMenuBar menuBar = new JMenuBar();
  private File lastSavedImage = null;
  private BufferedImage bufferedImage;
  private Graphics bufferGraphics;
  private Color penColor;
  private Color brushColor;
  private int lastPointX;
  private int lastPointY;

  public JProlGfxLibrary() {
    super("ProlGfxLib");

    this.graphicFrame = new JFrame("JProl Graphics");
    this.graphicFrame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
    this.graphicFrame.setResizable(false);
    this.graphicFrame.addWindowListener(this);

    this.penColor = Color.green;
    this.brushColor = Color.black;

    label = new JLabel();

    final JPanel rootPanel = new JPanel(new BorderLayout(0, 0));
    rootPanel.add(label, BorderLayout.CENTER);

    final JMenu saveMenu = new JMenu("Save");

    final JMenuItem menuItemSaveImage = new JMenuItem("To Disk", UiUtils.loadIcon("disk"));
    menuItemSaveImage.setActionCommand("DISK");
    menuItemSaveImage.addActionListener(this);

    final JMenuItem menuItemCopyToClipboard = new JMenuItem("To Clipboard", UiUtils.loadIcon("page_copy"));
    menuItemCopyToClipboard.addActionListener(this);
    menuItemCopyToClipboard.setActionCommand("CLIPBOARD");

    menuItemSaveImage.setToolTipText("Save graphics in PNG file");
    menuItemCopyToClipboard.setToolTipText("Save current screenshot from the graphics window into system clipboard");

    saveMenu.add(menuItemSaveImage);
    saveMenu.add(menuItemCopyToClipboard);

    this.menuBar.add(saveMenu);
    this.graphicFrame.setJMenuBar(this.menuBar);

    this.graphicFrame.setContentPane(rootPanel);

    this.lastPointX = 0;
    this.lastPointY = 0;

    changeResolution(128, 128);

    this.graphicFrame.setAlwaysOnTop(true);
    this.graphicFrame.setVisible(false);

    this.graphicFrame.setIconImage(UiUtils.loadIcon("appico").getImage());

    this.graphicFrame.pack();

    final JFrame mainFrame = MainFrame.MAIN_FRAME_INSTANCE.get();
    if (mainFrame != null) {
      this.graphicFrame.setLocationRelativeTo(mainFrame);
    }
  }

  private static Color getColorForName(final String color) {
    final Color result;
    if (color.charAt(0) == '#') {
      final int hex = Integer.parseInt(color.substring(1), 16);
      result = new Color(hex);
    } else {
      result = textToColor(color);
      if (result == null) {
        throw new IllegalArgumentException("Wrong color name '" + color + '\'');
      }
    }
    return result;
  }

  /**
   * Inside function to convert a Java Color into its Term representation
   *
   * @param color a Color object to be converted, must not be null
   * @return a Term which represents the color from the argument, the RGB
   * color as #RRGGBB
   */
  private static Term getColorAsTerm(final Color color) {
    final String colorhex = Integer.toHexString(color.getRGB() & 0xFFFFFF).toUpperCase();
    final int len = colorhex.length();

    final StringBuilder bldr = new StringBuilder(7);
    bldr.append('#');

    if (len < 6) {
      bldr.append("00000".substring(len - 1));
    }
    bldr.append(colorhex);

    return Terms.newAtom(bldr.toString());
  }

  /**
   * Inside function to convert a string representation of a color (as an
   * example 'red' or '#FF0000') into its Color object representation
   *
   * @param text the text to be decoded, must not be null
   * @return the Color object or null if it is impossible to decode the text
   * value
   */
  private static Color textToColor(String text) {
    boolean colorAsInteger;
    if (text.length() == 0) {
      return null;
    }
    try {
      final Field field = Color.class.getField(text);
      if (field.getType() == Color.class && Modifier.isStatic(field.getModifiers())) {
        return (Color) field.get(null);
      } else {
        colorAsInteger = true;
      }
    } catch (NoSuchFieldException ex) {
      colorAsInteger = true;
    } catch (Exception ex) {
      LOGGER.log(Level.WARNING, "textToColor(" + text + ")", ex);
      return null;
    }

    if (text.charAt(0) == '#') {
      text = text.substring(1);
      try {
        return new Color(Integer.parseInt(text, 16) & 0xFF000000);
      } catch (NumberFormatException ex) {
        // ignore
      }
    }
    return null;
  }

  @JProlPredicate(determined = true, signature = "beep/0", reference = "Make a short sound. It depends on the OS.")
  public static void predicateBEEP(final JProlChoicePoint goal, final TermStruct predicate) {
    Toolkit.getDefaultToolkit().beep();
  }

  private void saveImageAsFile(final BufferedImage image) {
    final JFileChooser fileChooser = new JFileChooser(this.lastSavedImage);
    fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    fileChooser.setFileFilter(new FileFilter() {

      @Override
      public boolean accept(final File f) {
        if (f == null) {
          return false;
        }
        return f.isDirectory() || f.getName().toLowerCase().endsWith(".png");
      }

      @Override
      public String getDescription() {
        return "PNG file (*.png)";
      }
    });

    if (fileChooser.showSaveDialog(this.graphicFrame) == JFileChooser.APPROVE_OPTION) {
      File selectedFile = fileChooser.getSelectedFile();
      if (selectedFile != null) {
        this.lastSavedImage = selectedFile;
        try {
          if (!selectedFile.getName().toLowerCase().endsWith(".png")) {
            selectedFile = new File(selectedFile.getParentFile(), selectedFile.getName() + ".png");
          }

          if (!ImageIO.write(image, "png", selectedFile)) {
            throw new IOException("Can't find the png encoder");
          }
        } catch (Exception ex) {
          LOGGER.log(Level.WARNING, "Can't save an image file", ex);
          JOptionPane.showMessageDialog(this.graphicFrame,
              "Can't save the image as a file [" + ex.getMessage() + ']', "Error",
              JOptionPane.ERROR_MESSAGE);
        }
      }
    }
  }

  @Override
  public void actionPerformed(final ActionEvent e) {
    if (e == null) {
      return;
    }

    // make snapshot of current buffer
    BufferedImage bufferCopy;
    internalLocker.lock();
    try {
      if (bufferedImage == null) {
        return;
      }
      final int imgw = bufferedImage.getWidth();
      final int imgh = bufferedImage.getHeight();

      if (imgw <= 0 || imgh <= 0) {
        return;
      }

      bufferCopy = new BufferedImage(imgw, imgh, BufferedImage.TYPE_INT_RGB);
      bufferCopy.getGraphics().drawImage(bufferedImage, 0, 0, null);
    } finally {
      internalLocker.unlock();
    }
    final String command = e.getActionCommand();
    if ("DISK".equals(command)) {
      saveImageAsFile(bufferCopy);
    } else if ("CLIPBOARD".equals(command)) {
      final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();

      if (clipboard != null) {
        try {
          clipboard.setContents(new ImageToClipboard(bufferCopy), null);
        } catch (IllegalStateException ex) {
          JOptionPane.showMessageDialog(graphicFrame, "Clippoard is unavailable", "Error", JOptionPane.ERROR_MESSAGE);
        }
      }
    }
  }

  @JProlPredicate(determined = true, signature = "dot/2", args = {"+number,+number"}, reference = "Draw a point in the coordinates (X,Y) with the current pen color.")
  public void predicateDOT(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term targx = predicate.getElement(0).findNonVarOrSame();
    final Term targy = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNumber(targx);
      ProlAssertions.assertNumber(targy);
    }

    final int argx = targx.toNumber().intValue();
    final int argy = targy.toNumber().intValue();

    final ReentrantLock locker = internalLocker;

    locker.lock();
    try {
      lastPointX = argx;
      lastPointY = argy;

      if (bufferedImage != null) {
        bufferGraphics.setColor(penColor);
        bufferGraphics.drawLine(argx, argy, argx, argy);
      }
    } finally {
      locker.unlock();
    }

    label.repaint();
  }

  @JProlPredicate(determined = true, signature = "removeaction/1", args = {"+term"}, reference = "Remove an action from the action menu for its name.")
  public void predicateREMOVEACTION(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term menuitem = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      ProlAssertions.assertNonVar(menuitem);
    }
    final String menuItemName = menuitem.forWrite();

    JMenuItem removedItem = null;

    synchronized (menuBar) {
      // remove registered action for the name
      final Component[] components = bindedMenu.getMenuComponents();
      for (final Component compo : components) {
        final JMenuItem item = (JMenuItem) compo;
        if (item.getText().equals(menuItemName)) {
          removedItem = item;
          bindedMenu.remove(item);
          break;
        }
      }

      if (bindedMenu.getMenuComponents().length == 0) {
        menuBar.remove(bindedMenu);
      }

      if (removedItem != null) {
        synchronized (registeredActions) {
          registeredActions.remove(removedItem);
        }
      }

      bindedMenu.revalidate();
      menuBar.revalidate();
    }

    graphicFrame.repaint();
  }

  @JProlPredicate(determined = true, signature = "removeallactions/0", reference = "Remove all actions from the action menu")
  public void predicateREMOVEALLACTIONS0(final JProlChoicePoint goal, final TermStruct predicate) {
    synchronized (menuBar) {
      bindedMenu.removeAll();
      menuBar.remove(bindedMenu);

      synchronized (registeredActions) {
        registeredActions.clear();
      }
    }

    graphicFrame.repaint();
  }

  @JProlPredicate(determined = true, signature = "bindaction/2", args = {"+term,+callable"}, reference = "Bind a goal to an action menu item (menu_item_name, action) which can be selected by user.")
  public void predicateBINDACTION(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term menuitem = predicate.getElement(0).findNonVarOrSame();
    final Term action = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNonVar(menuitem);
      ProlAssertions.assertCallable(action);
    }

    final String menuItemName = menuitem.forWrite();

    final RegisteredAction registeredAction = new RegisteredAction(menuItemName, action, goal.getContext());

    synchronized (menuBar) {
      menuBar.remove(bindedMenu);

      // remove already registered action for the name
      final Component[] components = bindedMenu.getMenuComponents();
      for (final Component compo : components) {
        final JMenuItem item = (JMenuItem) compo;
        if (item.getText().equals(menuItemName)) {
          menuBar.remove(item);
          break;
        }
      }

      final JMenuItem newItem = new JMenuItem(menuItemName);
      newItem.addActionListener((ActionEvent e) -> {
        RegisteredAction foundAction;
        synchronized (registeredActions) {
          foundAction = registeredActions.get(e.getSource());
        }
        if (foundAction != null) {
          if (!foundAction.execute()) {
            //remove registered menu item
            synchronized (bindedMenu) {
              bindedMenu.remove((JMenuItem) e.getSource());
              bindedMenu.revalidate();
            }
          }
        } else {
          synchronized (menuBar) {
            menuBar.remove((JMenuItem) e.getSource());
            menuBar.revalidate();
          }
        }
      });

      synchronized (registeredActions) {
        registeredActions.put(newItem, registeredAction);
      }

      bindedMenu.add(newItem);

      menuBar.add(bindedMenu);

      menuBar.revalidate();
    }

    graphicFrame.repaint();
  }

  @JProlPredicate(determined = true, signature = "rectangle/4", args = {"+number,+number,+number,+number"}, reference = "Draw a rectangle in the coordinates (X,Y,Width,Height) with the current pen color.")
  public void predicateRECTANGLE(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term targx = predicate.getElement(0).findNonVarOrSame();
    final Term targy = predicate.getElement(1).findNonVarOrSame();
    final Term targw = predicate.getElement(2).findNonVarOrSame();
    final Term targh = predicate.getElement(3).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNumber(targx);
      ProlAssertions.assertNumber(targy);
      ProlAssertions.assertNumber(targw);
      ProlAssertions.assertNumber(targh);
    }

    final int argx = targx.toNumber().intValue();
    final int argy = targy.toNumber().intValue();
    final int argw = targw.toNumber().intValue();
    final int argh = targh.toNumber().intValue();

    final ReentrantLock locker = internalLocker;

    locker.lock();
    try {
      lastPointX = argx;
      lastPointY = argy;

      if (bufferedImage != null) {
        bufferGraphics.setColor(penColor);
        bufferGraphics.drawRect(argx, argy, argw, argh);
      }
    } finally {
      locker.unlock();
    }
    label.repaint();
  }

  @JProlPredicate(determined = true, signature = "fillrectangle/4", args = {"+number,+number,+number,+number"}, reference = "Fill a rectangle in the coordinates (X,Y,Width,Height) with the current brush color.")
  public void predicateFILLRECTANGLE(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term targx = predicate.getElement(0).findNonVarOrSame();
    final Term targy = predicate.getElement(1).findNonVarOrSame();
    final Term targw = predicate.getElement(2).findNonVarOrSame();
    final Term targh = predicate.getElement(3).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNumber(targx);
      ProlAssertions.assertNumber(targy);
      ProlAssertions.assertNumber(targw);
      ProlAssertions.assertNumber(targh);
    }

    final int argx = targx.toNumber().intValue();
    final int argy = targy.toNumber().intValue();
    final int argw = targw.toNumber().intValue();
    final int argh = targh.toNumber().intValue();

    final ReentrantLock locker = internalLocker;

    locker.lock();
    try {
      lastPointX = argx;
      lastPointY = argy;

      if (bufferedImage != null) {
        bufferGraphics.setColor(brushColor);
        bufferGraphics.fillRect(argx, argy, argw, argh);
      }
    } finally {
      locker.unlock();
    }
    label.repaint();
  }

  @JProlPredicate(determined = true, signature = "plot/4", args = {"+number,+number,+number,+number"}, reference = "Draw a line (X1,Y1,X2,Y2) with the current pen color.")
  public void predicatePLOT(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term targx = predicate.getElement(0).findNonVarOrSame();
    final Term targy = predicate.getElement(1).findNonVarOrSame();
    final Term targxx = predicate.getElement(2).findNonVarOrSame();
    final Term targyy = predicate.getElement(3).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNumber(targx);
      ProlAssertions.assertNumber(targy);
      ProlAssertions.assertNumber(targxx);
      ProlAssertions.assertNumber(targyy);
    }

    final int argx = targx.toNumber().intValue();
    final int argy = targy.toNumber().intValue();
    final int argxx = targxx.toNumber().intValue();
    final int argyy = targyy.toNumber().intValue();

    final ReentrantLock locker = internalLocker;

    locker.lock();
    try {
      lastPointX = argxx;
      lastPointY = argyy;

      if (bufferedImage != null) {
        bufferGraphics.setColor(penColor);
        bufferGraphics.drawLine(argx, argy, argxx, argyy);
      }
    } finally {
      locker.unlock();
    }
    label.repaint();
  }

  @JProlPredicate(determined = true, signature = "plot/2", args = {"+number,+number"}, reference = "Draw a line from the last point to (X,Y) with the current pen color.")
  public void predicatePLOT2(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term targx = predicate.getElement(0).findNonVarOrSame();
    final Term targy = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNumber(targx);
      ProlAssertions.assertNumber(targy);
    }

    final int argx = targx.toNumber().intValue();
    final int argy = targy.toNumber().intValue();

    final ReentrantLock locker = internalLocker;
    locker.lock();
    try {
      if (bufferedImage != null) {
        bufferGraphics.setColor(penColor);
        bufferGraphics.drawLine(lastPointX, lastPointY, argx, argy);
      }

      lastPointX = argx;
      lastPointY = argy;
    } finally {
      locker.unlock();
    }
    label.repaint();
  }

  @JProlPredicate(determined = true, signature = "oval/4", args = {"+number,+number,+number,+number"}, reference = "Draw an oval into a rectangle area with coords (X,Y,Width,Height) with the current pen color.")
  public void predicateOVAL(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term targx = predicate.getElement(0).findNonVarOrSame();
    final Term targy = predicate.getElement(1).findNonVarOrSame();
    final Term targw = predicate.getElement(2).findNonVarOrSame();
    final Term targh = predicate.getElement(3).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNumber(targx);
      ProlAssertions.assertNumber(targy);
      ProlAssertions.assertNumber(targw);
      ProlAssertions.assertNumber(targh);
    }

    final int argx = targx.toNumber().intValue();
    final int argy = targy.toNumber().intValue();
    final int argw = targw.toNumber().intValue();
    final int argh = targh.toNumber().intValue();

    final ReentrantLock locker = internalLocker;
    locker.lock();
    try {
      if (bufferedImage != null) {
        bufferGraphics.setColor(penColor);
        bufferGraphics.drawOval(argx, argy, argw, argh);
      }

      lastPointX = argx;
      lastPointY = argy;
    } finally {
      locker.unlock();
    }
    label.repaint();
  }

  @JProlPredicate(determined = true, signature = "filloval/4", args = {"+number,+number,+number,+number"}, reference = "Fill an oval into a rectangle area with coords (X,Y,Width,Height) with the current pen color.")
  public void predicateFILLOVAL(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term targx = predicate.getElement(0).findNonVarOrSame();
    final Term targy = predicate.getElement(1).findNonVarOrSame();
    final Term targw = predicate.getElement(2).findNonVarOrSame();
    final Term targh = predicate.getElement(3).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNumber(targx);
      ProlAssertions.assertNumber(targy);
      ProlAssertions.assertNumber(targw);
      ProlAssertions.assertNumber(targh);
    }

    final int argx = targx.toNumber().intValue();
    final int argy = targy.toNumber().intValue();
    final int argw = targw.toNumber().intValue();
    final int argh = targh.toNumber().intValue();

    final ReentrantLock locker = internalLocker;
    locker.lock();
    try {
      if (bufferedImage != null) {
        bufferGraphics.setColor(brushColor);
        bufferGraphics.fillOval(argx, argy, argw, argh);
      }

      lastPointX = argx;
      lastPointY = argy;
    } finally {
      locker.unlock();
    }
    label.repaint();
  }

  @JProlPredicate(determined = true, signature = "fillscreen/0", reference = "Fill all screen by the brush color.")
  public void predicateFILLSCREEN(final JProlChoicePoint goal, final TermStruct predicate) {
    final ReentrantLock locker = internalLocker;
    locker.lock();
    try {
      if (bufferedImage != null) {
        bufferGraphics.setColor(brushColor);
        bufferGraphics.fillRect(0, 0, bufferedImage.getWidth(), bufferedImage.getHeight());
      }
    } finally {
      locker.unlock();
    }
    label.repaint();
  }

  @JProlPredicate(signature = "brushcolor/1", args = {"?atom"}, reference = "Change or get the current brush color. If it can't set color then it will return false")
  public boolean predicateBRUSHCOLOR(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      if (arg.getTermType() != VAR) {
        ProlAssertions.assertAtom(arg);
      }
    }

    final ReentrantLock locker = internalLocker;

    if (arg.getTermType() == VAR) {
      locker.lock();
      try {
        return arg.unifyTo(getColorAsTerm(brushColor));
      } finally {
        locker.unlock();
      }
    }

    final String text = arg.getText();

    final Color color;
    try {
      color = getColorForName(text);

    } catch (Exception ex) {
      LOGGER.log(Level.WARNING, "brushcolor/1", ex);
      return false;
    }

    locker.lock();
    try {
      brushColor = color;
    } finally {
      locker.unlock();
    }

    return true;
  }

  @JProlPredicate(determined = true, signature = "settitle/1", args = {"+atom"}, reference = "Set the title for the current graphic screen")
  public void predicateSETTITLE(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term term = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      ProlAssertions.assertAtom(term);
    }
    final String title = term.getText();
    SwingUtilities.invokeLater(() -> graphicFrame.setTitle(title));
  }

  @JProlPredicate(determined = true, signature = "pencolor/1", args = {"?atom"}, reference = "Change or get the current pen color. If it can't set color then it will return false")
  public boolean predicatePENCOLOR(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    final ReentrantLock locker = internalLocker;

    if (goal.isArgsValidate()) {
      if (arg.getTermType() != VAR) {
        ProlAssertions.assertAtom(arg);
      }
    }

    if (arg.getTermType() == VAR) {
      locker.lock();
      try {
        return arg.unifyTo(getColorAsTerm(penColor));
      } finally {
        locker.unlock();
      }
    }

    final String text = arg.getText();

    final Color color;
    try {
      color = getColorForName(text);

    } catch (Exception ex) {
      LOGGER.log(Level.WARNING, "pencolor/1", ex);
      return false;
    }

    locker.lock();
    try {
      penColor = color;
    } finally {
      locker.unlock();
    }

    return true;
  }

  @JProlPredicate(determined = true, signature = "cursor/2", args = {"?number,?number"}, reference = "Set or get the current cursor position (X,Y).")
  public boolean predicateCURSOR(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term elemX = predicate.getElement(0).findNonVarOrSame();
    final Term elemY = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      if (elemX.getTermType() != VAR) {
        ProlAssertions.assertNumber(elemX);
      }
      if (elemY.getTermType() != VAR) {
        ProlAssertions.assertNumber(elemY);
      }
    }

    final ReentrantLock locker = internalLocker;

    locker.lock();
    try {
      final TermLong lpx = Terms.newLong(lastPointX);
      final TermLong lpy = Terms.newLong(lastPointY);

      if (elemX.getTermType() == VAR) {
        if (!elemX.unifyTo(lpx)) {
          return false;
        }
      } else {
        lastPointX = elemX.toNumber().intValue();
      }

      if (elemY.getTermType() == VAR) {
        if (!elemY.unifyTo(lpy)) {
          return false;
        }
      } else {
        lastPointY = elemY.toNumber().intValue();
      }
    } finally {
      locker.unlock();
    }

    return true;
  }

  @JProlPredicate(determined = true, signature = "print/1", args = {"+term"}, reference = "Print the text representation of the term with the current pen color. The baseline of the leftmost character is at the cursor position.")
  public void predicatePRINT(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term elem = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      ProlAssertions.assertNonVar(elem);
    }

    final String text = elem.forWrite();
    final ReentrantLock locker = internalLocker;

    locker.lock();
    try {
      bufferGraphics.setColor(penColor);
      bufferGraphics.drawString(text, lastPointX, lastPointY);
    } finally {
      locker.unlock();
    }
    label.repaint();
  }

  /**
   * Inside function to save current graphic buffer state
   *
   * @param name      the image file name
   * @param type      the type of the image format (jpg, png or gif, remember that
   *                  it relates to possibilities of the ImageIO API from JRE)
   * @param predicate the predicate which is calling the operation
   */
  private void saveCurrentBuffer(final String name, final String type, final TermStruct predicate) {
    FileOutputStream outStream = null;

    try {
      final File file = new File(name);
      outStream = new FileOutputStream(file);

      internalLocker.lock();
      try {
        ImageIO.write(bufferedImage, type, outStream);
      } finally {
        internalLocker.unlock();
      }
      outStream.flush();
      outStream.close();
      outStream = null;
    } catch (Exception ex) {
      LOGGER.log(Level.WARNING, "saveCurrentBuffer()", ex);
      throw new ProlPermissionErrorException("write", "image_output", predicate, ex);
    } finally {
      if (outStream != null) {
        try {
          outStream.close();
        } catch (Exception ex) {
          LOGGER.log(Level.WARNING, "saveCurrentBuffer().close()", ex);
        }
      }
    }
  }

  @JProlPredicate(determined = true, signature = "saveimage/2", args = {"+atom,+atom"}, reference = "Arguments (image_name,format_name). Format can be 'png','jpg' or 'gif'. Save the current graphic buffer state as a named image with the type. It can throw 'permission_error' exception if it is not possible to write the image.")
  public void predicateSAVEIMAGE(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    final Term format = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertAtom(arg);
      ProlAssertions.assertAtom(format);
    }

    saveCurrentBuffer(arg.getText(), format.getText().trim().toLowerCase(), predicate);
  }

  @JProlPredicate(determined = true, signature = "graphics/2", args = {"?integer,?integer"}, reference = "Change or get the graphic screen size (width,heigh) and fill it with the curren background color. Pay attention, the predicate creates the new offscreen buffer so don't use it to clear screen.")
  public boolean predicateGRAPHICS(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term width = predicate.getElement(0).findNonVarOrSame();
    final Term height = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      if (width.getTermType() != VAR) {
        ProlAssertions.assertInteger(width);
      }
      if (height.getTermType() != VAR) {
        ProlAssertions.assertInteger(height);
      }
    }

    internalLocker.lock();
    try {
      final int widthOrig = bufferedImage.getWidth();
      final int heightOrig = bufferedImage.getHeight();

      int width2 = widthOrig;
      int height2 = heightOrig;

      if (width.getTermType() == VAR) {
        width.unifyTo(Terms.newLong(widthOrig));
      } else {
        width2 = width.toNumber().intValue();
      }

      if (height.getTermType() == VAR) {
        height.unifyTo(Terms.newLong(widthOrig));
      } else {
        height2 = height.toNumber().intValue();
      }

      if (widthOrig == width2 && heightOrig == height2) {
        return true;
      }

      if (width2 <= 0 || height2 <= 0) {
        return false;
      }

      changeResolution(width2, height2);

      activateFrame();
    } finally {
      internalLocker.unlock();
    }

    return true;
  }

  @Override
  public void onContextDispose(final JProlContext context) {
    super.onContextDispose(context);

    internalLocker.lock();
    try {
      if (this.graphicFrame != null) {
        SwingUtilities.invokeLater(graphicFrame::dispose);
        this.bufferedImage = null;
      }
    } finally {
      internalLocker.unlock();
    }
  }

  /**
   * Inside function allows to change the back buffer resolution for new
   * values
   *
   * @param width  the new width of the back buffer at pixels, must be more
   *               than zero
   * @param height the new height of the back buffer at pixels, must be more
   *               than zero
   */
  private void changeResolution(final int width, final int height) {
    if (width <= 0 || height <= 0) {
      return;
    }

    final BufferedImage newImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
    final Graphics gfx = newImage.getGraphics();

    final ReentrantLock locker = this.internalLocker;

    locker.lock();
    try {
      gfx.setColor(this.brushColor);
      gfx.fillRect(0, 0, width, height);

      this.bufferedImage = newImage;
      this.bufferGraphics = gfx;

      this.label.setIcon(new ImageIcon(this.bufferedImage));
      this.label.invalidate();
    } finally {
      locker.unlock();
    }
    this.graphicFrame.invalidate();
    this.graphicFrame.pack();
    this.graphicFrame.repaint();
  }

  /**
   * Inside function to make inside frame visible.
   */
  private void activateFrame() {
    if (this.graphicFrame != null) {
      this.graphicFrame.setVisible(true);
      this.graphicFrame.toFront();
    }
  }

  @Override
  public void windowOpened(WindowEvent e) {
  }

  @Override
  public void windowClosing(WindowEvent e) {
    graphicFrame.setExtendedState(JFrame.ICONIFIED);
  }

  @Override
  public void windowClosed(WindowEvent e) {
  }

  @Override
  public void windowIconified(WindowEvent e) {
  }

  @Override
  public void windowDeiconified(WindowEvent e) {
  }

  @Override
  public void windowActivated(WindowEvent e) {
  }

  @Override
  public void windowDeactivated(WindowEvent e) {
  }

  public static class ImageToClipboard implements Transferable {

    private final BufferedImage image;

    public ImageToClipboard(final BufferedImage image) {
      this.image = image;
    }

    @Override
    public DataFlavor[] getTransferDataFlavors() {
      return new DataFlavor[] {DataFlavor.imageFlavor};
    }

    @Override
    public boolean isDataFlavorSupported(final DataFlavor flavor) {
      return DataFlavor.imageFlavor.equals(flavor);
    }

    @Override
    public Object getTransferData(final DataFlavor flavor) throws UnsupportedFlavorException {
      if (!DataFlavor.imageFlavor.equals(flavor)) {
        throw new UnsupportedFlavorException(flavor);
      }
      return image;
    }
  }

  /**
   * The class describes registered UI action which can be called through a
   * menu item
   */
  private static class RegisteredAction {

    private final String menuText;
    private final Term action;
    private final JProlContext contextForTheAction;

    public RegisteredAction(final String menuText, final Term action, final JProlContext context) {
      this.menuText = requireNonNull(menuText);
      this.action = requireNonNull(action);
      this.contextForTheAction = requireNonNull(context);
    }

    public boolean execute() {
      if (this.contextForTheAction.isDisposed()) {
        return false;
      }
      try {
        this.contextForTheAction.proveAllAsync(action, true);
        return true;
      } catch (Throwable thr) {
        LOGGER.log(Level.SEVERE, "Can't execute registered action " + this.menuText, thr);
      }
      return false;
    }
  }
}
