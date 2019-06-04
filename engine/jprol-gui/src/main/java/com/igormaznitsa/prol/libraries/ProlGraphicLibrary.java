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
package com.igormaznitsa.prol.libraries;

import com.igormaznitsa.prol.annotations.Determined;
import com.igormaznitsa.prol.annotations.Predicate;
import com.igormaznitsa.prol.data.NumericTerm;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermInteger;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.easygui.MainFrame;
import com.igormaznitsa.prol.easygui.UIUtils;
import com.igormaznitsa.prol.exceptions.ProlPermissionErrorException;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.utils.Utils;

import javax.imageio.ImageIO;
import javax.swing.*;
import javax.swing.filechooser.FileFilter;
import java.awt.*;
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

/**
 * The class implements the Prol Graphic library and it can be used to show to
 * draw simple graphic primitives and save images. It uses the JDK graphic
 * possibilities at its work.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public final class ProlGraphicLibrary extends AbstractProlLibrary implements WindowListener, ActionListener {

  /**
   * Inside logger, the canonical class name is used as the logger identifier
   * (ProlGraphicLibrary.class.getCanonicalName())
   */
  protected static final Logger LOG = Logger.getLogger(ProlGraphicLibrary.class.getCanonicalName());
  /**
   * The hash map contains registered actions
   */
  private final WeakHashMap<JMenuItem, RegisteredAction> registeredActions = new WeakHashMap<>();
  /**
   * The graphic frame which will be used to show current graphic buffer
   */
  private final JFrame graphicFrame;
  /**
   * The label is used as the container to show the back buffer
   */
  private final JLabel label;
  /**
   * Inside access locker
   */
  private final ReentrantLock insideLocker = new ReentrantLock();
  /**
   * Registered actions allow to prove registered goals
   */
  private final JMenu bindedMenu = new JMenu("Actions");
  /**
   * The menu bar
   */
  private final JMenuBar menuBar = new JMenuBar();
  /**
   * Contains the last saved image file
   */
  private File lastSavedImage = null;
  /**
   * The memory placed image back buffer
   */
  private BufferedImage bufferedImage;
  /**
   * The graphics object from the back buffer
   */
  private Graphics bufferGraphics;
  /**
   * The variable contains the current pen color
   */
  private Color penColor;
  /**
   * The variable contains the current brush color
   */
  private Color brushColor;
  /**
   * The last X coordinate of a graphic operation
   */
  private int lastPointX;
  /**
   * The last Y coordinate of a graphic operation
   */
  private int lastPointY;

  /**
   * The constructor
   */
  public ProlGraphicLibrary(final IoActionProvider controller) {
    super("ProlGraphicLib", controller);

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

    final JMenuItem menuItemSaveImage = new JMenuItem("To Disk", UIUtils.loadIcon("disk"));
    menuItemSaveImage.setActionCommand("DISK");
    menuItemSaveImage.addActionListener(this);

    final JMenuItem menuItemCopyToClipboard = new JMenuItem("To Clipboard", UIUtils.loadIcon("page_copy"));
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

    this.graphicFrame.setIconImage(UIUtils.loadIcon("appico").getImage());

    this.graphicFrame.pack();

    final JFrame mainFrame = MainFrame.MAIN_FRAME_INSTANCE.get();
    if (mainFrame != null) {
      this.graphicFrame.setLocationRelativeTo(mainFrame);
    }
  }

  private static Color getColorForName(final String color) {

    if (color == null) {
      throw new NullPointerException("Color name is null");
    }

    final Color result;
    if (color.charAt(0) == '#') {
      final int hex = Integer.parseInt(color.substring(1), 16);
      result = new Color(hex);
    } else {
      result = textToColor(color);
      if (result == null) {
        throw new IllegalArgumentException("Wrong color name \'" + color + '\'');
      }
    }
    return result;
  }

  /**
   * Inside function to convert a Java Color into its Term representation
   *
   * @param color a Color object to be converted, must not be null
   * @return a Term which represents the color from the argument, the RGB color
   * as #RRGGBB
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

    return new Term(bldr.toString());
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
      LOG.log(Level.WARNING, "textToColor(" + text + ")", ex);
      return null;
    }

    if (colorAsInteger && text.charAt(0) == '#') {
      text = text.substring(1);
      try {
        return new Color(Integer.parseInt(text, 16) & 0xFF000000);
      } catch (NumberFormatException ex) {
      }
    }
    return null;
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
      File choosed = fileChooser.getSelectedFile();

      if (choosed != null) {

        try {
          if (!choosed.getName().toLowerCase().endsWith(".png")) {
            choosed = new File(choosed.getParentFile(), choosed.getName() + ".png");
          }

          if (!ImageIO.write(image, "png", choosed)) {
            throw new IOException("Can't find the png encoder");
          }
        } catch (Exception ex) {
          LOG.log(Level.WARNING, "Can't save an image file", ex);
          JOptionPane.showMessageDialog(this.graphicFrame, "Can't save the image as a file [" + ex.getMessage() + ']', "Error", JOptionPane.ERROR_MESSAGE);
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
    BufferedImage bufferCopy = null;
    insideLocker.lock();
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
      insideLocker.unlock();
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

  @Predicate(Signature = "dot/2", Template = {"+number,+number"}, Reference = "Draw a point in the coordinates (X,Y) with the current pen color.")
  @Determined
  public void predicateDOT(final Goal goal, final TermStruct predicate) {
    final int argx = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(0))).getNumericValue().intValue();
    final int argy = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(1))).getNumericValue().intValue();

    final ReentrantLock locker = insideLocker;

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

  @Predicate(Signature = "removeaction/1", Template = {"+term"}, Reference = "Remove an action from the action menu for its name.")
  @Determined
  public final void predicateREMOVEACTION(final Goal goal, final TermStruct predicate) {
    final Term menuitem = Utils.getTermFromElement(predicate.getElement(0));
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

  @Predicate(Signature = "removeallactions/0", Template = {}, Reference = "Remove all actions from the action menu")
  @Determined
  public final void predicateREMOVEALLACTIONS(final Goal goal, final TermStruct predicate) {
    synchronized (menuBar) {
      bindedMenu.removeAll();
      menuBar.remove(bindedMenu);

      synchronized (registeredActions) {
        registeredActions.clear();
      }
    }

    graphicFrame.repaint();
  }

  @Predicate(Signature = "bindaction/2", Template = {"+term,+callable_term"}, Reference = "Bind a goal to an action menu item (menu_item_name, action) which can be selected by user.")
  @Determined
  public final void predicateBINDACTION(final Goal goal, final TermStruct predicate) {
    final Term menuitem = Utils.getTermFromElement(predicate.getElement(0));
    final Term action = Utils.getTermFromElement(predicate.getElement(1));

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

      final JMenuItem newitem = new JMenuItem(menuItemName);
      newitem.addActionListener((ActionEvent e) -> {
        RegisteredAction registeredAction1;
        synchronized (registeredActions) {
          registeredAction1 = registeredActions.get(e.getSource());
        }
        if (registeredAction1 != null) {
          if (!registeredAction1.execute()) {
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
        registeredActions.put(newitem, registeredAction);
      }

      bindedMenu.add(newitem);

      menuBar.add(bindedMenu);

      menuBar.revalidate();
    }

    graphicFrame.repaint();
  }

  @Predicate(Signature = "rectangle/4", Template = {"+number,+number,+number,+number"}, Reference = "Draw a rectangle in the coordinates (X,Y,Width,Height) with the current pen color.")
  @Determined
  public final void predicateRECTANGLE(final Goal goal, final TermStruct predicate) {
    final int argx = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(0))).getNumericValue().intValue();
    final int argy = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(1))).getNumericValue().intValue();
    final int argw = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(2))).getNumericValue().intValue();
    final int argh = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(3))).getNumericValue().intValue();

    final ReentrantLock locker = insideLocker;

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

  @Predicate(Signature = "fillrectangle/4", Template = {"+number,+number,+number,+number"}, Reference = "Fill a rectangle in the coordinates (X,Y,Width,Height) with the current brush color.")
  @Determined
  public void predicateFILLRECTANGLE(final Goal goal, final TermStruct predicate) {
    final int argx = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(0))).getNumericValue().intValue();
    final int argy = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(1))).getNumericValue().intValue();
    final int argw = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(2))).getNumericValue().intValue();
    final int argh = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(3))).getNumericValue().intValue();

    final ReentrantLock locker = insideLocker;

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

  @Predicate(Signature = "plot/4", Template = {"+number,+number,+number,+number"}, Reference = "Draw a line (X1,Y1,X2,Y2) with the current pen color.")
  @Determined
  public void predicatePLOT(final Goal goal, final TermStruct predicate) {
    final int argx = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(0))).getNumericValue().intValue();
    final int argy = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(1))).getNumericValue().intValue();
    final int argxx = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(2))).getNumericValue().intValue();
    final int argyy = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(3))).getNumericValue().intValue();

    final ReentrantLock locker = insideLocker;

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

  @Predicate(Signature = "plot/2", Template = {"+number,+number"}, Reference = "Draw a line from the last point to (X,Y) with the current pen color.")
  @Determined
  public void predicatePLOT2(final Goal goal, final TermStruct predicate) {
    final int argx = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(0))).getNumericValue().intValue();
    final int argy = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(1))).getNumericValue().intValue();

    final ReentrantLock locker = insideLocker;
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

  @Predicate(Signature = "oval/4", Template = {"+number,+number,+number,+number"}, Reference = "Draw an oval into a rectangle area with coords (X,Y,Width,Height) with the current pen color.")
  @Determined
  public void predicateOVAL(final Goal goal, final TermStruct predicate) {
    final int argx = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(0))).getNumericValue().intValue();
    final int argy = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(1))).getNumericValue().intValue();
    final int argw = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(2))).getNumericValue().intValue();
    final int argh = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(3))).getNumericValue().intValue();

    final ReentrantLock locker = insideLocker;
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

  @Predicate(Signature = "filloval/4", Template = {"+number,+number,+number,+number"}, Reference = "Fill an oval into a rectangle area with coords (X,Y,Width,Height) with the current pen color.")
  @Determined
  public void predicateFILLOVAL(final Goal goal, final TermStruct predicate) {
    final int argx = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(0))).getNumericValue().intValue();
    final int argy = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(1))).getNumericValue().intValue();
    final int argw = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(2))).getNumericValue().intValue();
    final int argh = ((NumericTerm) Utils.getTermFromElement(predicate.getElement(3))).getNumericValue().intValue();

    final ReentrantLock locker = insideLocker;
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

  @Predicate(Signature = "fillscreen/0", Reference = "Fill all screen by the brush color.")
  @Determined
  public void predicateFILLSCREEN(final Goal goal, final TermStruct predicate) {
    final ReentrantLock locker = insideLocker;
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

  @Predicate(Signature = "brushcolor/1", Template = {"?atom"}, Reference = "Change or get the current brush color. If it can't set color then it will return false")
  @Determined
  public boolean predicateBRUSHCOLOR(final Goal goal, final TermStruct predicate) {
    final Term arg = Utils.getTermFromElement(predicate.getElement(0));
    final ReentrantLock locker = insideLocker;

    if (arg.getTermType() == Term.TYPE_VAR) {
      locker.lock();
      try {
        return arg.Equ(getColorAsTerm(brushColor));
      } finally {
        locker.unlock();
      }
    }

    final String text = arg.getText();

    final Color color;
    try {
      color = getColorForName(text);

    } catch (Exception ex) {
      LOG.log(Level.WARNING, "brushcolor/1", ex);
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

  @Predicate(Signature = "settitle/1", Template = {"+atom"}, Reference = "Set the title for the current graphic screen")
  @Determined
  public void predicateSETTITLE(final Goal goal, final TermStruct predicate) {
    final String title = Utils.getTermFromElement(predicate.getElement(0)).getText();
    SwingUtilities.invokeLater(() -> graphicFrame.setTitle(title));
  }

  @Predicate(Signature = "pencolor/1", Template = {"?atom"}, Reference = "Change or get the current pen color. If it can't set color then it will return false")
  @Determined
  public boolean predicatePENCOLOR(final Goal goal, final TermStruct predicate) {
    final Term arg = Utils.getTermFromElement(predicate.getElement(0));
    final ReentrantLock locker = insideLocker;

    if (arg.getTermType() == Term.TYPE_VAR) {
      locker.lock();
      try {
        return arg.Equ(getColorAsTerm(penColor));
      } finally {
        locker.unlock();
      }
    }

    final String text = arg.getText();

    final Color color;
    try {
      color = getColorForName(text);

    } catch (Exception ex) {
      LOG.log(Level.WARNING, "pencolor/1", ex);
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

  @Predicate(Signature = "cursor/2", Template = {"?number,?number"}, Reference = "Set or get the current cursor position (X,Y).")
  @Determined
  public boolean predicateCURSOR(final Goal goal, final TermStruct predicate) {
    final Term elemX = Utils.getTermFromElement(predicate.getElement(0));
    final Term elemY = Utils.getTermFromElement(predicate.getElement(1));

    final ReentrantLock locker = insideLocker;

    locker.lock();
    try {
      final TermInteger lpx = new TermInteger(lastPointX);
      final TermInteger lpy = new TermInteger(lastPointY);

      if (elemX.getTermType() == Term.TYPE_VAR) {
        if (!elemX.Equ(lpx)) {
          return false;
        }
      } else {
        lastPointX = ((NumericTerm) elemX).getNumericValue().intValue();
      }

      if (elemY.getTermType() == Term.TYPE_VAR) {
        if (!elemY.Equ(lpy)) {
          return false;
        }
      } else {
        lastPointY = ((NumericTerm) elemY).getNumericValue().intValue();
      }
    } finally {
      locker.unlock();
    }

    return true;
  }

  @Predicate(Signature = "print/1", Template = {"+term"}, Reference = "Print the text representation of the term with the current pen color. The baseline of the leftmost character is at the cursor position.")
  @Determined
  public void predicatePRINT(final Goal goal, final TermStruct predicate) {
    final Term elem = Utils.getTermFromElement(predicate.getElement(0));

    final String text = elem.forWrite();
    final ReentrantLock locker = insideLocker;

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
   * @param name the image file name
   * @param type the type of the image format (jpg, png or gif, remember that it
   * relates to possibilities of the ImageIO API from JRE)
   * @param predicate the predicate which is calling the operation
   */
  private void saveCurrentBuffer(final String name, final String type, final TermStruct predicate) {
    FileOutputStream outStream = null;

    try {
      final File file = new File(name);
      outStream = new FileOutputStream(file);

      insideLocker.lock();
      try {
        ImageIO.write(bufferedImage, type, outStream);
      } finally {
        insideLocker.unlock();
      }
      outStream.flush();
      outStream.close();
      outStream = null;
    } catch (Exception ex) {
      LOG.log(Level.WARNING, "saveCurrentBuffer()", ex);
      throw new ProlPermissionErrorException("write", "image_output", predicate, ex);
    } finally {
      if (outStream != null) {
        try {
          outStream.close();
        } catch (Exception ex) {
          LOG.log(Level.WARNING, "saveCurrentBuffer().close()", ex);
        }
      }
    }
  }

  @Predicate(Signature = "saveimage/2", Template = {"+atom,+atom"}, Reference = "Arguments (image_name,format_name). Format can be 'png','jpg' or 'gif'. Save the current graphic buffer state as a named image with the type. It can throw \'permission_error\' exception if it is not possible to write the image.")
  @Determined
  public void predicateSAVEIMAGE(final Goal goal, final TermStruct predicate) {
    final Term arg = Utils.getTermFromElement(predicate.getElement(0));
    final Term format = Utils.getTermFromElement(predicate.getElement(1));
    saveCurrentBuffer(arg.getText(), format.getText().trim().toLowerCase(), predicate);
  }

  @Predicate(Signature = "graphics/2", Template = {"?integer,?integer"}, Reference = "Change or get the graphic screen size (width,heigh) and fill it with the curren background color. Pay attention, the predicate creates the new offscreen buffer so don't use it to clear screen.")
  @Determined
  public boolean predicateGRAPHICS(final Goal goal, final TermStruct predicate) {
    final Term width = Utils.getTermFromElement(predicate.getElement(0));
    final Term height = Utils.getTermFromElement(predicate.getElement(1));

    insideLocker.lock();
    try {
      final int widthOrig = bufferedImage.getWidth();
      final int heightOrig = bufferedImage.getHeight();

      int width2 = widthOrig;
      int height2 = heightOrig;

      if (width.getTermType() == Term.TYPE_VAR) {
        width.Equ(new TermInteger(widthOrig));
      } else {
        width2 = ((TermInteger) width).getNumericValue().intValue();
      }

      if (height.getTermType() == Term.TYPE_VAR) {
        height.Equ(new TermInteger(widthOrig));
      } else {
        height2 = ((TermInteger) height).getNumericValue().intValue();
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
      insideLocker.unlock();
    }

    return true;
  }

  @Override
  public void contextHasBeenHalted(final ProlContext context) {
    super.contextHasBeenHalted(context);
    insideLocker.lock();
    try {
      if (graphicFrame != null) {
        SwingUtilities.invokeLater(graphicFrame::dispose);

        bufferedImage = null;
      }
    } finally {
      insideLocker.unlock();
    }
  }

  /**
   * Inside function allows to change the back buffer resolution for new values
   *
   * @param width the new width of the back buffer at pixels, must be more than
   * zero
   * @param height the new height of the back buffer at pixels, must be more
   * than zero
   */
  private void changeResolution(final int width, final int height) {
    if (width <= 0 || height <= 0) {
      return;
    }

    final BufferedImage newImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
    final Graphics gfx = newImage.getGraphics();

    final ReentrantLock locker = this.insideLocker;

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
      return new DataFlavor[]{DataFlavor.imageFlavor};
    }

    @Override
    public boolean isDataFlavorSupported(final DataFlavor flavor) {
      return DataFlavor.imageFlavor.equals(flavor);
    }

    @Override
    public Object getTransferData(final DataFlavor flavor) throws UnsupportedFlavorException, IOException {
      if (!DataFlavor.imageFlavor.equals(flavor)) {
        throw new UnsupportedFlavorException(flavor);
      }
      return image;
    }
  }

  /**
   * The class describes an registered UI action which can be called through a
   * menu item
   */
  private static class RegisteredAction {

    private final String menuText;
    private final Term action;
    private final ProlContext contextForTheAction;

    public RegisteredAction(final String menuText, final Term action, final ProlContext context) {
      if (menuText == null || action == null || context == null) {
        throw new NullPointerException("An argument is null");
      }

      this.menuText = menuText;
      this.action = action;
      this.contextForTheAction = context;
    }

    public boolean execute() {
      if (contextForTheAction.isHalted()) {
        return false;
      }
      try {
        contextForTheAction.solveAsynchronously(action, null);
        return true;
      } catch (Throwable thr) {
        LOG.log(Level.SEVERE, "Can't execute registered action " + menuText, thr);
      }
      return false;
    }
  }
}
