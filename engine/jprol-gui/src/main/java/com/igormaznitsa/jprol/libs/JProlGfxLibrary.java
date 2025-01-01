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

import static com.igormaznitsa.jprol.data.TermType.ATOM;
import static com.igormaznitsa.jprol.data.TermType.STRUCT;
import static com.igormaznitsa.jprol.data.TermType.VAR;
import static java.util.Objects.requireNonNull;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.easygui.MainFrame;
import com.igormaznitsa.jprol.easygui.UiUtils;
import com.igormaznitsa.jprol.exceptions.ProlException;
import com.igormaznitsa.jprol.exceptions.ProlExistenceErrorException;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
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
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.ImageIO;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.SourceDataLine;
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

public final class JProlGfxLibrary extends AbstractJProlLibrary
    implements WindowListener, ActionListener, MouseListener, MouseMotionListener,
    MouseWheelListener {

  private static final Set<String> allowedMouseActions =
      Set.of("clicked", "pressed", "released", "entered", "exited", "wheel", "moved", "dragged");

  private static final Logger LOGGER = Logger.getLogger(JProlGfxLibrary.class.getName());
  private final Map<String, JProlMenuRegisteredAction> registeredActions =
      new ConcurrentHashMap<>();
  private final Map<String, List<JProlMouseAction>> registeredMouseActions =
      new ConcurrentHashMap<>();
  private final AtomicReference<JFrame> graphicFrame = new AtomicReference<>();
  private final AtomicReference<JLabel> label = new AtomicReference<>();
  private final JMenu boundActionsMenu = new JMenu("Actions");
  private final JMenuBar menuBar = new JMenuBar();
  private final Map<String, BufferedImage> imageSpriteMap = new ConcurrentHashMap<>();
  private final Map<String, SoundClip> soundClipMap = new ConcurrentHashMap<>();
  private final AtomicReference<Color> penColor = new AtomicReference<>();
  private final AtomicReference<Color> brushColor = new AtomicReference<>();
  private final AtomicReference<SourceDataLine> soundDataLine = new AtomicReference<>();
  private final Lock soundAccessLocker = new ReentrantLock();
  private File lastSavedImage = null;
  private BufferedImage bufferedImage;
  private Graphics bufferGraphics;
  private int lastPointX;
  private int lastPointY;

  public JProlGfxLibrary() {
    super("ProlGfxLib");

    this.penColor.set(Color.green);
    this.brushColor.set(Color.black);

    try {
      SwingUtilities.invokeAndWait(() -> {
        final JLabel theLabel = new JLabel();

        theLabel.addMouseListener(this);
        theLabel.addMouseMotionListener(this);
        theLabel.addMouseWheelListener(this);

        this.label.set(theLabel);

        final JFrame frame = new JFrame("JProl Graphics");
        frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        frame.setResizable(false);
        frame.addWindowListener(this);

        final JPanel rootPanel = new JPanel(new BorderLayout(0, 0));
        rootPanel.add(theLabel, BorderLayout.CENTER);

        final JMenu saveMenu = new JMenu("Save");

        final JMenuItem menuItemSaveImage = new JMenuItem("To Disk", UiUtils.loadIcon("disk"));
        menuItemSaveImage.setActionCommand("DISK");
        menuItemSaveImage.addActionListener(this);

        final JMenuItem menuItemCopyToClipboard =
            new JMenuItem("To Clipboard", UiUtils.loadIcon("page_copy"));
        menuItemCopyToClipboard.addActionListener(this);
        menuItemCopyToClipboard.setActionCommand("CLIPBOARD");

        menuItemSaveImage.setToolTipText("Save graphics in PNG file");
        menuItemCopyToClipboard.setToolTipText(
            "Save current screenshot from the graphics window into system clipboard");

        saveMenu.add(menuItemSaveImage);
        saveMenu.add(menuItemCopyToClipboard);

        this.menuBar.add(saveMenu);
        frame.setJMenuBar(this.menuBar);

        frame.setContentPane(rootPanel);

        this.lastPointX = 0;
        this.lastPointY = 0;

        changeResolution(128, 128);

        frame.setAlwaysOnTop(true);
        frame.setVisible(false);

        frame.setIconImage(UiUtils.loadIcon("appico").getImage());

        frame.pack();

        final JFrame mainFrame = MainFrame.MAIN_FRAME_INSTANCE.get();
        if (mainFrame != null) {
          frame.setLocationRelativeTo(mainFrame);
        }

        this.graphicFrame.set(frame);
      });
    } catch (InterruptedException ex) {
      Thread.currentThread().interrupt();
    } catch (InvocationTargetException ex) {
      throw new RuntimeException(ex);
    }
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
    if (text.isEmpty()) {
      return null;
    }
    try {
      final Field field = Color.class.getField(text);
      if (field.getType() == Color.class && Modifier.isStatic(field.getModifiers())) {
        return (Color) field.get(null);
      }
    } catch (NoSuchFieldException ex) {
      // do nothing
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
    final String colorHex = Integer.toHexString(color.getRGB() & 0xFFFFFF).toUpperCase();
    final int len = colorHex.length();

    final StringBuilder buffer = new StringBuilder(7);
    buffer.append('#');

    if (len < 6) {
      buffer.append("00000".substring(len - 1));
    }
    buffer.append(colorHex);

    return Terms.newAtom(buffer.toString());
  }

  @JProlPredicate(determined = true, signature = "beep/0", reference = "Make a short sound. It depends on the OS.")
  public static void predicateBEEP(final JProlChoicePoint goal, final TermStruct predicate) {
    Toolkit.getDefaultToolkit().beep();
  }

  private void doMouseAction(final int x, final int y, final int clicks, final String actionId) {
    final List<JProlMouseAction> actions = this.registeredMouseActions.get(actionId);
    if (actions != null) {
      actions.forEach(action -> action.execute(x, y, clicks, actionId));
    }
  }

  @Override
  public void mouseClicked(final MouseEvent e) {
    this.doMouseAction(e.getX(), e.getY(), e.getClickCount(), "clicked");
  }

  @Override
  public void mousePressed(MouseEvent e) {
    this.doMouseAction(e.getX(), e.getY(), e.getClickCount(), "pressed");
  }

  @Override
  public void mouseReleased(MouseEvent e) {
    this.doMouseAction(e.getX(), e.getY(), e.getClickCount(), "released");
  }

  @Override
  public void mouseEntered(MouseEvent e) {
    this.doMouseAction(e.getX(), e.getY(), e.getClickCount(), "entered");
  }

  @Override
  public void mouseExited(MouseEvent e) {
    this.doMouseAction(e.getX(), e.getY(), e.getClickCount(), "exited");
  }

  @Override
  public void mouseDragged(MouseEvent e) {
    this.doMouseAction(e.getX(), e.getY(), e.getClickCount(), "dragged");
  }

  @Override
  public void mouseMoved(MouseEvent e) {
    this.doMouseAction(e.getX(), e.getY(), e.getClickCount(), "moved");
  }

  @Override
  public void mouseWheelMoved(MouseWheelEvent e) {
    this.doMouseAction(e.getX(), e.getY(), e.getWheelRotation(), "wheel");
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

    final JFrame frame = this.graphicFrame.get();
    if (frame != null) {
      if (fileChooser.showSaveDialog(frame) == JFileChooser.APPROVE_OPTION) {
        File selectedFile = fileChooser.getSelectedFile();
        if (selectedFile != null) {
          this.lastSavedImage = selectedFile;
          try {
            if (!selectedFile.getName().toLowerCase().endsWith(".png")) {
              selectedFile =
                  new File(selectedFile.getParentFile(), selectedFile.getName() + ".png");
            }

            if (!ImageIO.write(image, "png", selectedFile)) {
              throw new IOException("Can't find the png encoder");
            }
          } catch (Exception ex) {
            LOGGER.log(Level.WARNING, "Can't save an image file", ex);
            JOptionPane.showMessageDialog(frame,
                "Can't save the image as a file [" + ex.getMessage() + ']', "Error",
                JOptionPane.ERROR_MESSAGE);
          }
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
      if (bufferedImage == null) {
        return;
      }
      final int imageWidth = bufferedImage.getWidth();
      final int imageHeight = bufferedImage.getHeight();

      if (imageWidth <= 0 || imageHeight <= 0) {
        return;
      }

      bufferCopy = new BufferedImage(imageWidth, imageHeight, BufferedImage.TYPE_INT_RGB);
      bufferCopy.getGraphics().drawImage(bufferedImage, 0, 0, null);

    final String command = e.getActionCommand();
    if ("DISK".equals(command)) {
      saveImageAsFile(bufferCopy);
    } else if ("CLIPBOARD".equals(command)) {
      final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();

      if (clipboard != null) {
        try {
          clipboard.setContents(new ImageToClipboard(bufferCopy), null);
        } catch (IllegalStateException ex) {
          final JFrame frame = this.graphicFrame.get();
          if (frame != null) {
            JOptionPane.showMessageDialog(frame, "Clipboard is unavailable", "Error",
                JOptionPane.ERROR_MESSAGE);
          }
        }
      }
    }
  }

  @JProlPredicate(determined = true, signature = "dot/2", args = {
      "+number,+number"}, reference = "Draw a point in the coordinates (X,Y) with the current pen color.")
  public void predicateDOT(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term termArgX = predicate.getElement(0).findNonVarOrSame();
    final Term termArgY = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNumber(termArgX);
      ProlAssertions.assertNumber(termArgY);
    }

    this.safeSwingSynchroCall(() -> {
      final int argX = termArgX.toNumber().intValue();
      final int argY = termArgY.toNumber().intValue();

      lastPointX = argX;
      lastPointY = argY;

      if (bufferedImage != null) {
        bufferGraphics.setColor(penColor.get());
        bufferGraphics.drawLine(argX, argY, argX, argY);
      }
      this.repaintLabel();
    });
  }

  @JProlPredicate(determined = true, signature = "bindaction/0", reference = "Remove all actions from the action menu")
  public void predicateBINDACTION0(final JProlChoicePoint goal, final TermStruct predicate) {
    this.registeredActions.clear();
    this.safeSwingCall(() -> {
      boundActionsMenu.removeAll();
      menuBar.remove(boundActionsMenu);
      menuBar.invalidate();
      menuBar.repaint();
    });
  }

  @JProlPredicate(determined = true, signature = "bindmouse/2", reference = "List in format [x,y,clicksOrWheel,mouseAction]. Bind mouse actions.")
  public void predicateBINDMOUSE(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term action = predicate.getElement(0).findNonVarOrSame();
    final Term callable = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertAtom(action);
      ProlAssertions.assertCallable(callable);
    }

    final String actionType = action.getTermType() == ATOM ? action.getText() : "";

    if (!allowedMouseActions.contains(actionType)) {
      throw new ProlInstantiationErrorException("Not-allowed mouse action: " + actionType,
          callable);
    }

    if (callable.getTermType() != STRUCT || ((TermStruct) callable).getArity() != 4) {
      throw new ProlInstantiationErrorException("Expected callable structure with arity 4",
          callable);
    }

    final List<JProlMouseAction> list = this.registeredMouseActions.computeIfAbsent(actionType,
        name -> new CopyOnWriteArrayList<>());
    list.add(new JProlMouseAction((TermStruct) callable, goal.getContext()));
  }

  @JProlPredicate(determined = true, signature = "bindaction/1", args = {
      "+term"}, reference = "Remove a goal bound to an action menu item.")
  public void predicateBINDACTION1(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term menuItem = predicate.getElement(0).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNonVar(menuItem);
    }

    final String menuItemName = menuItem.forWrite();
    final JProlMenuRegisteredAction registeredAction = this.registeredActions.remove(menuItemName);

    if (registeredAction != null) {
      this.safeSwingCall(() -> {
        final Component[] components = boundActionsMenu.getMenuComponents();
        for (final Component compo : components) {
          final JMenuItem item = (JMenuItem) compo;
          if (item.getClientProperty("jprol-action") != null) {
            if (item.getText().equals(menuItemName)) {
              boundActionsMenu.remove(item);
              break;
            }
          }
        }
      });
    }
  }

  @JProlPredicate(determined = true, signature = "bindaction/2", args = {
      "+term,+callable"}, reference = "Bind a goal to an action menu item (menu_item_name, action) which can be selected by user.")
  public void predicateBINDACTION2(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term menuItem = predicate.getElement(0).findNonVarOrSame();
    final Term action = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNonVar(menuItem);
      ProlAssertions.assertCallable(action);
    }

    final String menuItemName = menuItem.forWrite();

    final JProlMenuRegisteredAction registeredAction =
        new JProlMenuRegisteredAction(menuItemName, action, goal.getContext());

    this.registeredActions.put(menuItemName, registeredAction);

    this.safeSwingCall(() -> {
      JMenuItem foundItem = null;

      final Component[] components = boundActionsMenu.getMenuComponents();
      for (final Component compo : components) {
        final JMenuItem item = (JMenuItem) compo;
        if (item.getClientProperty("jprol-action") != null) {
          if (item.getText().equals(menuItemName)) {
            foundItem = item;
            break;
          }
        }
      }

      if (foundItem == null) {
        foundItem = new JMenuItem(menuItemName);
        foundItem.putClientProperty("jprol-action", true);
        boundActionsMenu.add(foundItem);
      } else {
        for (final ActionListener l : foundItem.getActionListeners()) {
          foundItem.removeActionListener(l);
        }
      }

      foundItem.addActionListener((ActionEvent e) -> {
        if (registeredAction.execute()) {
          menuBar.revalidate();
        } else {
          registeredActions.remove(registeredAction.menuText);
          menuBar.remove((JMenuItem) e.getSource());
          menuBar.revalidate();
        }
      });
      if (boundActionsMenu.getParent() == null) {
        menuBar.add(boundActionsMenu);
      }
      menuBar.invalidate();
      menuBar.repaint();
    });
  }

  @JProlPredicate(determined = true, signature = "rectangle/4", args = {
      "+number,+number,+number,+number"}, reference = "Draw a rectangle in the coordinates (X,Y,Width,Height) with the current pen color.")
  public void predicateRECTANGLE(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term term0 = predicate.getElement(0).findNonVarOrSame();
    final Term term1 = predicate.getElement(1).findNonVarOrSame();
    final Term term2 = predicate.getElement(2).findNonVarOrSame();
    final Term term3 = predicate.getElement(3).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNumber(term0);
      ProlAssertions.assertNumber(term1);
      ProlAssertions.assertNumber(term2);
      ProlAssertions.assertNumber(term3);
    }

    final int argumentTargetX = term0.toNumber().intValue();
    final int argumentTargetY = term1.toNumber().intValue();
    final int argumentTargetWidth = term2.toNumber().intValue();
    final int argumentTargetHeight = term3.toNumber().intValue();

    safeSwingCall(() -> {
      this.lastPointX = argumentTargetX;
      this.lastPointY = argumentTargetY;

      if (this.bufferGraphics != null) {
        this.bufferGraphics.setColor(penColor.get());
        this.bufferGraphics.drawRect(argumentTargetX, argumentTargetY, argumentTargetWidth,
            argumentTargetHeight);
      }
    });
    this.repaintLabel();
  }

  @JProlPredicate(determined = true, signature = "fillrectangle/4", args = {
      "+number,+number,+number,+number"}, reference = "Fill a rectangle in the coordinates (X,Y,Width,Height) with the current brush color.")
  public void predicateFILLRECTANGLE(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term term0 = predicate.getElement(0).findNonVarOrSame();
    final Term term1 = predicate.getElement(1).findNonVarOrSame();
    final Term term2 = predicate.getElement(2).findNonVarOrSame();
    final Term term3 = predicate.getElement(3).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNumber(term0);
      ProlAssertions.assertNumber(term1);
      ProlAssertions.assertNumber(term2);
      ProlAssertions.assertNumber(term3);
    }

    final int argumentTargetX = term0.toNumber().intValue();
    final int argumentTargetY = term1.toNumber().intValue();
    final int argumentTargetWidth = term2.toNumber().intValue();
    final int argumentTargetHeight = term3.toNumber().intValue();

    safeSwingCall(() -> {
      this.lastPointX = argumentTargetX;
      this.lastPointY = argumentTargetY;

      if (this.bufferGraphics != null) {
        this.bufferGraphics.setColor(brushColor.get());
        this.bufferGraphics.fillRect(argumentTargetX, argumentTargetY, argumentTargetWidth,
            argumentTargetHeight);
      }
    });
    this.repaintLabel();
  }

  @JProlPredicate(determined = true, signature = "plot/4", args = {
      "+number,+number,+number,+number"}, reference = "Draw a line (X1,Y1,X2,Y2) with the current pen color.")
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

    this.safeSwingSynchroCall(() -> {
      lastPointX = argxx;
      lastPointY = argyy;

      if (bufferedImage != null) {
        bufferGraphics.setColor(penColor.get());
        bufferGraphics.drawLine(argx, argy, argxx, argyy);
      }
      this.repaintLabel();
    });
  }

  @JProlPredicate(determined = true, signature = "plot/2", args = {
      "+number,+number"}, reference = "Draw a line from the last point to (X,Y) with the current pen color.")
  public void predicatePLOT2(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term targetX = predicate.getElement(0).findNonVarOrSame();
    final Term targetY = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNumber(targetX);
      ProlAssertions.assertNumber(targetY);
    }

    final int argumentX = targetX.toNumber().intValue();
    final int argumentY = targetY.toNumber().intValue();

    this.safeSwingSynchroCall(() -> {
      if (bufferedImage != null) {
        bufferGraphics.setColor(penColor.get());
        bufferGraphics.drawLine(lastPointX, lastPointY, argumentX, argumentY);
      }

      lastPointX = argumentX;
      lastPointY = argumentY;

      this.repaintLabel();
    });
  }

  @JProlPredicate(determined = true, signature = "oval/4", args = {
      "+number,+number,+number,+number"}, reference = "Draw an oval into a rectangle area with coords (X,Y,Width,Height) with the current pen color.")
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

    this.safeSwingSynchroCall(() -> {
      if (bufferedImage != null) {
        bufferGraphics.setColor(penColor.get());
        bufferGraphics.drawOval(argx, argy, argw, argh);
      }

      lastPointX = argx;
      lastPointY = argy;
      this.repaintLabel();
    });
  }

  @JProlPredicate(determined = true, signature = "filloval/4", args = {
      "+number,+number,+number,+number"}, reference = "Fill an oval into a rectangle area with coords (X,Y,Width,Height) with the current pen color.")
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

    this.safeSwingSynchroCall(() -> {
      if (bufferedImage != null) {
        bufferGraphics.setColor(brushColor.get());
        bufferGraphics.fillOval(argx, argy, argw, argh);
      }

      lastPointX = argx;
      lastPointY = argy;
      this.repaintLabel();
    });
  }

  @JProlPredicate(determined = true, signature = "fillscreen/0", reference = "Fill all screen by the brush color.")
  public void predicateFILLSCREEN(final JProlChoicePoint goal, final TermStruct predicate) {
    this.safeSwingSynchroCall(() -> {
      if (bufferedImage != null) {
        bufferGraphics.setColor(brushColor.get());
        bufferGraphics.fillRect(0, 0, bufferedImage.getWidth(), bufferedImage.getHeight());
      }
      this.repaintLabel();
    });
  }

  @JProlPredicate(signature = "brushcolor/1", args = {
      "?atom"}, reference = "Change or get the current brush color. If it can't set color then it will return false")
  public boolean predicateBRUSHCOLOR(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      if (arg.getTermType() != VAR) {
        ProlAssertions.assertAtom(arg);
      }
    }

    if (arg.getTermType() == VAR) {
      return arg.unifyTo(getColorAsTerm(brushColor.get()));
    }

    final String text = arg.getText();

    final Color color;
    try {
      color = getColorForName(text);

    } catch (Exception ex) {
      LOGGER.log(Level.WARNING, "brushcolor/1", ex);
      return false;
    }
    this.brushColor.set(color);
    return true;
  }

  @JProlPredicate(determined = true, signature = "settitle/1", args = {
      "+atom"}, reference = "Set the title for the current graphic screen")
  public void predicateSETTITLE(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term term = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      ProlAssertions.assertAtom(term);
    }
    final String title = term.getText();
    this.safeSwingCall(() -> {
      final JFrame frame = this.graphicFrame.get();
      if (frame != null) {
        frame.setTitle(title);
      }
    });
  }

  @JProlPredicate(determined = true, signature = "pencolor/1", args = {
      "?atom"}, reference = "Change or get the current pen color. If it can't set color then it will return false")
  public boolean predicatePENCOLOR(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      if (arg.getTermType() != VAR) {
        ProlAssertions.assertAtom(arg);
      }
    }

    if (arg.getTermType() == VAR) {
      return arg.unifyTo(getColorAsTerm(penColor.get()));
    }

    final String text = arg.getText();

    final Color color;
    try {
      color = getColorForName(text);
    } catch (Exception ex) {
      LOGGER.log(Level.WARNING, "pencolor/1", ex);
      return false;
    }

    this.penColor.set(color);

    return true;
  }

  @JProlPredicate(determined = true, signature = "cursor/2", args = {
      "?number,?number"}, reference = "Set or get the current cursor position (X,Y).")
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

    final AtomicBoolean result = new AtomicBoolean(true);
    this.safeSwingSynchroCall(() -> {
      final TermLong lpx = Terms.newLong(lastPointX);
      final TermLong lpy = Terms.newLong(lastPointY);

      if (elemX.getTermType() == VAR) {
        if (!elemX.unifyTo(lpx)) {
          result.set(false);
        }
      } else {
        lastPointX = elemX.toNumber().intValue();
      }

      if (elemY.getTermType() == VAR) {
        if (!elemY.unifyTo(lpy)) {
          result.set(false);
        }
      } else {
        lastPointY = elemY.toNumber().intValue();
      }
    });
    return result.get();
  }

  @JProlPredicate(determined = true, signature = "print/1", args = {
      "+term"}, reference = "Print the text representation of the term with the current pen color. The baseline of the leftmost character is at the cursor position.")
  public void predicatePRINT(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term elem = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      ProlAssertions.assertNonVar(elem);
    }

    final String text = elem.forWrite();
    this.safeSwingSynchroCall(() -> {
      bufferGraphics.setColor(penColor.get());
      bufferGraphics.drawString(text, lastPointX, lastPointY);
      this.repaintLabel();
    });
  }

  private void repaintLabel() {
    final JLabel theLabel = this.label.get();
    if (theLabel != null) {
      theLabel.repaint();
    }
  }

  /**
   * Inside function to save current graphic buffer state
   *
   * @param folder    base folder
   * @param name      the image file name
   * @param type      the type of the image format (jpg, png or gif, remember that
   *                  it relates to possibilities of the ImageIO API from JRE)
   * @param predicate the predicate which is calling the operation
   */
  private void saveCurrentBuffer(final File folder, final String name, final String type,
                                 final TermStruct predicate) {
    this.safeSwingSynchroCall(() -> {
      try (final FileOutputStream fileOutputStream = new FileOutputStream(new File(folder, name),
          false)) {
        ImageIO.write(bufferedImage, type, fileOutputStream);
      } catch (IOException e) {
        LOGGER.log(Level.WARNING, "saveCurrentBuffer()", e);
        throw new ProlPermissionErrorException("write", "image_output", predicate, e);
      }
    });
  }

  @JProlPredicate(determined = true, signature = "play_sound/2", args = {
      "+number,+number"}, reference = "Arguments (frequency_hz,length_ms). Generate sound tone with frequency in hertz and length in milliseconds.")
  public void predicatePLAYSOUND(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term freqHz = predicate.getElement(0).findNonVarOrSame();
    final Term lengthMs = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNumber(freqHz);
      ProlAssertions.assertNumber(lengthMs);
    }

    final int frequency = 44100;

    this.soundAccessLocker.lock();
    try {
      SourceDataLine sourceDataLine = this.soundDataLine.get();
      if (sourceDataLine == null) {
        try {
          final AudioFormat audioFormat;
          audioFormat = new AudioFormat((float) frequency, 16, 1, true, false);

          sourceDataLine = AudioSystem.getSourceDataLine(audioFormat);
          sourceDataLine.open();
          sourceDataLine.start();

          this.soundDataLine.set(sourceDataLine);
        } catch (Exception ex) {
          LOGGER.log(Level.SEVERE, "Can't open sound channel in PLAY_SOUND/2", ex);
          try {
            Thread.sleep(Math.max(1, lengthMs.toNumber().intValue()));
          } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
          }
          return;
        }
      }

      try {
        final byte[] buf = new byte[2];
        final int durationMs = Math.max(lengthMs.toNumber().intValue(), 0);
        final int numberOfTimesFullSinFuncPerSec = Math.max(1, freqHz.toNumber().intValue());

        final int iterations = Math.round(durationMs * 44100.0f / 1000.0f);

        for (int i = 0; i < iterations && !goal.getContext().isDisposed(); i++) {
          float numberOfSamplesToRepresentFullSin =
              (float) frequency / numberOfTimesFullSinFuncPerSec;
          double angle = i / (numberOfSamplesToRepresentFullSin / 2.0) * Math.PI;
          short a = (short) (Math.sin(angle) *
              32767);
          buf[0] = (byte) (a & 0xFF);
          buf[1] = (byte) (a >> 8);
          sourceDataLine.write(buf, 0, 2);
        }
      } catch (Exception ex) {
        LOGGER.log(Level.SEVERE, "Error during PLAY_SOUND/2", ex);
      }
    } finally {
      this.soundAccessLocker.unlock();
    }
  }

  @JProlPredicate(determined = true, signature = "draw_sprite/3", args = {
      "+atom, +number, +number"}, reference = "Arguments (sprite_id, x, y). Draw a sprite by its sprite id at coordinates.")
  public boolean predicateDRAWSPRITE(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term path = predicate.getElement(0).findNonVarOrSame();
    final Term x = predicate.getElement(1).findNonVarOrSame();
    final Term y = predicate.getElement(2).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertAtom(path);
      ProlAssertions.assertNumber(x);
      ProlAssertions.assertNumber(y);
    }

    final BufferedImage sprite = this.imageSpriteMap.get(path.getText());
    if (sprite == null) {
      return false;
    } else {
      this.safeSwingSynchroCall(() -> {
        if (bufferedImage != null) {
          bufferGraphics.drawImage(sprite, x.toNumber().intValue(), y.toNumber().intValue(), null);
        }
        this.repaintLabel();
      });
      return true;
    }
  }

  @JProlPredicate(determined = true, signature = "load_sprite/2", args = {
      "+atom,+atom"}, reference = "Arguments (sprite_id, image_path). Format can be 'png','jpg' or 'gif'. Load sprite from file and keep it as named by sprite id. It can throw 'permission_error' exception if it is not possible to read the image.")
  public boolean predicateLOADSPRITE(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term spriteId = predicate.getElement(0).findNonVarOrSame();
    final Term path = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertAtom(path);
      ProlAssertions.assertAtom(spriteId);
    }
    final File spriteFile = new File(goal.getContext().getCurrentFolder(), path.getText());

    if (!spriteFile.isFile()) {
      throw new ProlExistenceErrorException("file",
          "Can't find file: " + spriteFile.getAbsolutePath(), predicate);
    }

    if (!spriteFile.canRead()) {
      throw new ProlPermissionErrorException("read", "image_input", predicate);
    }

    try {
      final BufferedImage image = ImageIO.read(spriteFile);
      this.imageSpriteMap.put(spriteId.getText(), image);
      return true;
    } catch (Exception ex) {
      return false;
    }
  }

  @JProlPredicate(determined = true, signature = "play_soundclip/2", args = {
      "+atom, +number"}, reference = "Arguments (soundclip_id, loop). Play sound clip.")
  public boolean predicatePLAYSOUNDCLIP2(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term soundClipId = predicate.getElement(0).findNonVarOrSame();
    final Term loop = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertAtom(soundClipId);
      ProlAssertions.assertNumber(loop);
    }

    final SoundClip soundClip = this.soundClipMap.get(soundClipId.getText());
    if (soundClip == null) {
      return false;
    } else {
      soundClip.play(loop.toNumber().intValue());
      return true;
    }
  }

  @JProlPredicate(determined = true, signature = "play_soundclip/1", args = {
      "+atom"}, reference = "Arguments (soundclip_id). Play sound clip.")
  public boolean predicatePLAYSOUNDCLIP1(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term soundClipId = predicate.getElement(0).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertAtom(soundClipId);
    }

    final SoundClip soundClip = this.soundClipMap.get(soundClipId.getText());
    if (soundClip == null) {
      return false;
    } else {
      soundClip.play();
      return true;
    }
  }

  @JProlPredicate(determined = true, signature = "stop_soundclip/1", args = {
      "+atom"}, reference = "Arguments (soundclip_id). Stop play sound clip.")
  public boolean predicateSTOPSOUNDCLIP(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term soundClipId = predicate.getElement(0).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertAtom(soundClipId);
    }

    final SoundClip soundClip = this.soundClipMap.get(soundClipId.getText());
    if (soundClip == null) {
      return false;
    } else {
      soundClip.stop();
      return true;
    }
  }

  @JProlPredicate(determined = true, signature = "is_soundclip_playing/1", args = {
      "+atom"}, reference = "Arguments (soundclip_id). Check sound clip play status.")
  public boolean predicateISSOUNDCLIPPLAYING(final JProlChoicePoint goal,
                                             final TermStruct predicate) {
    final Term soundClipId = predicate.getElement(0).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertAtom(soundClipId);
    }

    final SoundClip soundClip = this.soundClipMap.get(soundClipId.getText());
    if (soundClip == null) {
      return false;
    } else {
      return soundClip.isPlaying();
    }
  }

  @JProlPredicate(determined = true, signature = "load_soundclip/2", args = {
      "+atom,+atom"}, reference = "Arguments (soundclip_id, soundclip_path). Format can be AIFF, AU or WAV. Load sound clip from file and keep it as named by sound clip id. It can throw 'permission_error' exception if it is not possible to read the file.")
  public boolean predicateLOADSOUNDCLIP(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term soundClipId = predicate.getElement(0).findNonVarOrSame();
    final Term path = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertAtom(path);
      ProlAssertions.assertAtom(soundClipId);
    }

    final File soundClipFile = new File(goal.getContext().getCurrentFolder(), path.getText());

    if (!soundClipFile.isFile()) {
      throw new ProlExistenceErrorException("file",
          "Can't find file: " + soundClipFile.getAbsolutePath(), predicate);
    }

    if (!soundClipFile.canRead()) {
      throw new ProlPermissionErrorException("read", "soundclip_input", predicate);
    }

    try {
      final byte[] fileData = Files.readAllBytes(soundClipFile.toPath());
      this.soundClipMap.put(soundClipId.getText(), new SoundClip(fileData));
      return true;
    } catch (Exception ex) {
      return false;
    }
  }

  @JProlPredicate(determined = true, signature = "saveimage/2", args = {
      "+atom,+atom"}, reference = "Arguments (image_name,format_name). Format can be 'png','jpg' or 'gif'. Save the current graphic buffer state as a named image with the type. It can throw 'permission_error' exception if it is not possible to write the image.")
  public void predicateSAVEIMAGE(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    final Term format = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertAtom(arg);
      ProlAssertions.assertAtom(format);
    }

    saveCurrentBuffer(goal.getContext().getCurrentFolder(), arg.getText(),
        format.getText().trim().toLowerCase(), predicate);
  }

  @JProlPredicate(determined = true, signature = "graphics/2", args = {
      "?integer,?integer"}, reference = "Change or get the graphic screen size (width,height) and fill it with the current background color. Pay attention, the predicate creates the new offscreen buffer so don't use it to clear screen.")
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

    this.safeSwingSynchroCall(() -> {
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

      final int newW = Math.max(1, width2);
      final int newH = Math.max(1, height2);

      if (widthOrig != newW && heightOrig != newH) {
        changeResolution(width2, height2);
      }
      activateFrame();
    });

    return true;
  }

  @Override
  protected void onCallContextDispose(final JProlContext context,
                                      final Map<String, Object> contextNamedObjects) {
    super.onCallContextDispose(context, contextNamedObjects);

    this.registeredActions.clear();
    this.registeredMouseActions.clear();

    try {
      this.imageSpriteMap.clear();
      final SourceDataLine sourceDataLine = this.soundDataLine.getAndSet(null);
      if (sourceDataLine != null) {
        try {
          sourceDataLine.stop();
        } catch (Exception ex) {
          // do nothing
        }
      }
    } finally {
      this.soundClipMap.values().forEach(soundClip -> {
        try {
          soundClip.close();
        } catch (Exception ex) {
          // ignore
        }
      });
      this.soundClipMap.clear();
      final JFrame frame = this.graphicFrame.getAndSet(null);
      this.safeSwingCall(() -> {
        try {
          if (this.bufferGraphics != null) {
            this.bufferGraphics.dispose();
          }
        } finally {
          this.bufferGraphics = null;
          this.bufferedImage = null;
          if (frame != null) {
            try {
              frame.dispose();
            } catch (Exception ex) {
              // do nothing
            }
          }
        }
      });
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
  private boolean changeResolution(final int width, final int height) {
    if (width <= 0 || height <= 0) {
      return false;
    }

    final BufferedImage newImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
    final Graphics gfx = newImage.getGraphics();
    final JLabel theLabel = this.label.get();

    if (theLabel != null) {
      return safeSwingSynchroCall(() -> {
        gfx.setColor(this.brushColor.get());
        gfx.fillRect(0, 0, width, height);

        this.bufferedImage = newImage;
        this.bufferGraphics = gfx;
        theLabel.setIcon(new ImageIcon(this.bufferedImage));
        theLabel.invalidate();

        final JFrame frame = this.graphicFrame.get();
        if (frame != null) {
          frame.invalidate();
          frame.pack();
          frame.repaint();
        }
      });
    }
    return true;
  }

  private void safeSwingCall(final Runnable action) {
    if (SwingUtilities.isEventDispatchThread()) {
      action.run();
    } else {
      SwingUtilities.invokeLater(action);
    }
  }

  private boolean safeSwingSynchroCall(final Runnable action) {
    if (SwingUtilities.isEventDispatchThread()) {
      action.run();
    } else {
      try {
        SwingUtilities.invokeAndWait(action);
      } catch (InterruptedException ex) {
        Thread.currentThread().interrupt();
        return false;
      } catch (InvocationTargetException ex) {
        if (ex.getCause() instanceof ProlException) {
          throw ((ProlException) ex.getCause());
        }
        LOGGER.log(Level.SEVERE, "Can't change size", ex);
        return false;
      }
    }
    return true;
  }

  private void activateFrame() {
    final JFrame frame = this.graphicFrame.get();
    if (frame != null) {
      frame.setVisible(true);
      frame.toFront();
    }
  }

  @Override
  public void windowOpened(WindowEvent e) {
  }

  @Override
  public void windowClosing(WindowEvent e) {
    final JFrame frame = this.graphicFrame.get();
    if (frame != null) {
      frame.setExtendedState(JFrame.ICONIFIED);
    }
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

  private static class JProlMouseAction {
    private final TermStruct action;
    private final JProlContext context;

    JProlMouseAction(final TermStruct action, final JProlContext context) {
      this.action = requireNonNull(action);
      this.context = requireNonNull(context);
      if (action.getArity() != 4) {
        throw new IllegalArgumentException("Unsupported action term: " + action);
      }
    }

    boolean execute(final int x, final int y, final int clicksOrWheel, final String actionId) {
      if (this.context.isDisposed()) {
        return false;
      }

      final TermStruct clone = (TermStruct) this.action.makeClone();
      final TermStruct args = Terms.newStruct(clone.getFunctor(),
          new Term[] {Terms.newLong(x), Terms.newLong(y), Terms.newLong(clicksOrWheel),
              Terms.newAtom(actionId)});
      if (clone.unifyTo(args)) {
        try {
          this.context.proveAllAsync(clone, true);
          return true;
        } catch (Throwable thr) {
          LOGGER.log(Level.SEVERE, "Can't execute registered mouse action " + actionId, thr);
        }
      }
      return false;
    }
  }

  private static class JProlMenuRegisteredAction {

    private final String menuText;
    private final Term action;
    private final JProlContext context;

    JProlMenuRegisteredAction(final String menuText, final Term action,
                              final JProlContext context) {
      this.menuText = requireNonNull(menuText);
      this.action = requireNonNull(action);
      this.context = requireNonNull(context);
    }

    boolean execute() {
      if (this.context.isDisposed()) {
        return false;
      }
      try {
        this.context.proveAllAsync(action, true);
        return true;
      } catch (Throwable thr) {
        LOGGER.log(Level.SEVERE, "Can't execute registered action " + this.menuText, thr);
      }
      return false;
    }
  }
}
