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

import static java.lang.String.format;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.exceptions.ProlChoicePointInterruptedException;
import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.exceptions.ProlInterruptException;
import com.igormaznitsa.jprol.exceptions.ProlPermissionErrorException;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.libs.JProlCoreLibrary;
import com.igormaznitsa.jprol.libs.JProlGfxLibrary;
import com.igormaznitsa.jprol.libs.JProlHttpLibrary;
import com.igormaznitsa.jprol.libs.JProlIoLibrary;
import com.igormaznitsa.jprol.libs.JProlJsonLibrary;
import com.igormaznitsa.jprol.libs.JProlRegexLibrary;
import com.igormaznitsa.jprol.libs.JProlStrLibrary;
import com.igormaznitsa.jprol.libs.JProlThreadLibrary;
import com.igormaznitsa.jprol.libs.TPrologPredicateLibrary;
import com.igormaznitsa.jprol.logic.ConsultInteract;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.JProlSystemFlag;
import com.igormaznitsa.jprol.logic.io.IoResourceProvider;
import com.igormaznitsa.jprol.trace.JProlContextListener;
import com.igormaznitsa.jprol.trace.TraceEvent;
import com.igormaznitsa.jprol.utils.Utils;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import java.awt.Desktop;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.lang.ref.WeakReference;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.prefs.Preferences;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.imageio.ImageIO;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.KeyStroke;
import javax.swing.Painter;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.tree.TreeModel;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoManager;

public final class MainFrame extends javax.swing.JFrame
    implements ConsultInteract, IoResourceProvider, Runnable, UndoableEditListener, WindowListener,
    DocumentListener, HyperlinkListener, JProlContextListener {

  private static final String PROL_EXTENSION = ".prl";
  private static final long serialVersionUID = 72348723421332L;

  private static final String[] PROL_LIBRARIES = new String[] {
      JProlCoreLibrary.class.getCanonicalName(),
      JProlThreadLibrary.class.getCanonicalName(),
      JProlIoLibrary.class.getCanonicalName(),
      JProlStrLibrary.class.getCanonicalName(),
      JProlJsonLibrary.class.getCanonicalName(),
      JProlRegexLibrary.class.getCanonicalName(),
      JProlGfxLibrary.class.getCanonicalName(),
      JProlHttpLibrary.class.getCanonicalName(),
      TPrologPredicateLibrary.class.getCanonicalName()
  };

  private static final Logger LOG = Logger.getLogger(MainFrame.class.getName());
  private static final FileFilter PROL_FILE_FILTER = new FileFilter() {

    @Override
    public boolean accept(File f) {
      if (f == null) {
        return false;
      }
      if (f.isDirectory()) {
        return true;
      }
      return f.getName().toLowerCase().endsWith(PROL_EXTENSION);
    }

    @Override
    public String getDescription() {
      return "Prol files (*" + PROL_EXTENSION + ')';
    }
  };
  private static final int MAX_RECENT_FILES = 10;
  public static volatile WeakReference<MainFrame> MAIN_FRAME_INSTANCE;
  private final LogLibrary logLibrary;
  private final AtomicReference<Thread> currentExecutedScriptThread = new AtomicReference<>();
  private final AtomicBoolean startedInTracing = new AtomicBoolean();
  /**
   * The version of the IDE
   */
  private final String VERSION =
      this.getClass().getPackage().getImplementationVersion() == null ? "<Development>"
          : this.getClass().getPackage().getImplementationVersion();
  private final ThreadGroup executingScripts = new ThreadGroup("ProlExecutingScripts");
  private final RecentlyOpenedFileFixedList recentFiles =
      new RecentlyOpenedFileFixedList(MAX_RECENT_FILES);
  private Map<String, LookAndFeelInfo> lookAndFeelMap;
  private File currentOpenedFile;
  private File lastOpenedFile;
  private boolean documentHasBeenChangedFlag;
  private volatile JProlContext lastContext;
  // Variables declaration - do not modify
  private javax.swing.JButton buttonCloseFind;
  private javax.swing.JButton buttonStopExecuting;
  private com.igormaznitsa.jprol.easygui.DialogEditor dialogEditor;
  private javax.swing.JPanel editorPanel;
  private javax.swing.Box.Filler filler1;
  private javax.swing.JMenuBar jMenuBar1;
  private javax.swing.JPopupMenu.Separator jSeparator1;
  private javax.swing.JPopupMenu.Separator jSeparator2;
  private javax.swing.JPopupMenu.Separator jSeparator3;
  private javax.swing.JPopupMenu.Separator jSeparator4;
  private javax.swing.JPopupMenu.Separator jSeparator5;
  private javax.swing.JLabel labelFind;
  private javax.swing.JMenuItem menuAbout;
  private javax.swing.JMenuItem menuClearText;
  private javax.swing.JMenu menuEdit;
  private javax.swing.JMenuItem menuEditCommentSelected;
  private javax.swing.JMenuItem menuEditOptions;
  private javax.swing.JMenuItem menuEditUncommentSelected;
  private javax.swing.JMenuItem menuExit;
  private javax.swing.JMenu menuFile;
  private javax.swing.JMenuItem menuFileNew;
  private javax.swing.JMenuItem menuFileOpen;
  private javax.swing.JMenu menuFileRecentFiles;
  private javax.swing.JMenuItem menuFileSave;
  private javax.swing.JMenuItem menuFileSaveAs;
  private javax.swing.JMenu menuHelp;
  private javax.swing.JMenuItem menuHelpHelp;
  private javax.swing.JMenuItem menuItemFullScreen;
  private javax.swing.JMenuItem menuItemLibraryInfo;
  private javax.swing.JCheckBoxMenuItem menuItemWordWrapSources;
  private javax.swing.JMenu menuLookAndFeel;
  private javax.swing.JMenuItem menuRedo;
  private javax.swing.JMenu menuRun;
  private javax.swing.JMenuItem menuRunScript;
  private javax.swing.JMenuItem menuRunStop;
  private javax.swing.JMenuItem menuTraceScript;
  private javax.swing.JMenuItem menuUndo;
  private javax.swing.JMenu menuView;
  private javax.swing.JMenuItem menuViewKnowledgeBase;
  private javax.swing.JMenuItem menuitemFindText;
  private com.igormaznitsa.jprol.easygui.MessageEditor messageEditor;
  private javax.swing.JPanel panelFindText;
  private javax.swing.JPanel panelProgress;
  private javax.swing.JProgressBar progressBarTask;
  private com.igormaznitsa.jprol.easygui.PrologSourceEditor sourceEditor;
  private javax.swing.JSplitPane splitPaneMain;
  private javax.swing.JSplitPane splitPaneTop;
  private javax.swing.JSplitPane splitPanelDown;
  private javax.swing.JTextField textFind;
  private com.igormaznitsa.jprol.easygui.TraceDialog traceEditor;

  private void setAppIcon(final Image icon) {
    try {
      Class<?> taskbarClass = Class.forName("java.awt.Taskbar");
      Object taskbarInstance = taskbarClass.getMethod("getTaskbar").invoke(null);
      taskbarClass.getMethod("setIconImage", Image.class).invoke(taskbarInstance, icon);
    } catch (Exception ex) {
      // ignore
    }

    this.setIconImage(icon);
  }

  /**
   * Creates new form MainFrame
   */
  public MainFrame(GraphicsConfiguration graphicsConfiguration) {
    super(graphicsConfiguration);
    try {
      initComponents();

      Rectangle screenBounds =
          graphicsConfiguration == null ? null : graphicsConfiguration.getBounds();
      if (screenBounds != null) {
        setSize((screenBounds.width * 10) / 12, (screenBounds.height * 10) / 12);
      }
      sourceEditor.addUndoableEditListener(this);
      sourceEditor.addDocumentListener(this);
      messageEditor.addHyperlinkListener(this);
      addWindowListener(this);
      panelProgress.setVisible(false);

      logLibrary = new LogLibrary();

      try {
        this.setAppIcon(ImageIO.read(
            this.getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/appico.png")));
      } catch (Exception ex) {
        LOG.throwing(this.getClass().getCanonicalName(), "MainFrame", ex);
      }

      fillLFMenuItem();

      loadPreferences();

      newFile();

      this.menuItemWordWrapSources.setState(sourceEditor.getEdWordWrap());

      final Action action = new AbstractAction("closeFindPanel") {
        private static final long serialVersionUID = 4377386270269629176L;

        @Override
        public void actionPerformed(ActionEvent e) {
          if (panelFindText.isVisible()) {
            panelFindText.setVisible(false);
            textFind.setText("");
          }
        }

      };

      final KeyStroke escKey = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0);
      action.putValue(Action.ACCELERATOR_KEY, escKey);
      this.buttonCloseFind.getActionMap().put("closeFind", action);
      this.buttonCloseFind.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(escKey, "closeFind");

      this.panelFindText.setVisible(false);
    } finally {
      MAIN_FRAME_INSTANCE = new WeakReference<>(this);
      if (graphicsConfiguration != null) {
        final Rectangle rect = graphicsConfiguration.getBounds();
        if (rect != null) {
          this.setBounds(rect.x + 32, rect.y + 32, Math.min(rect.width - 32, this.getSize().width),
              Math.min(rect.height - 32, this.getSize().height));
        }
      }
    }
  }

  public MainFrame(final GraphicsConfiguration config, final File initFile) {
    this(config);
    loadFile(initFile, true);
  }

  public void addErrorText(final String msg) {
    this.messageEditor.addErrorText(msg);
  }

  public void addInfoText(final String msg) {
    this.messageEditor.addInfoText(msg);
  }

  public void addWarnText(final String msg) {
    this.messageEditor.addWarningText(msg);
  }

  public File chooseFile(final File folder, final FileFilter fileFilter, final String dialogTitle,
                         final String approveButtonText) {
    final JFileChooser fileChooser = new JFileChooser(folder);
    fileChooser.setFileFilter(fileFilter);
    fileChooser.setDialogTitle(dialogTitle);
    fileChooser.setApproveButtonText(approveButtonText);
    fileChooser.setMultiSelectionEnabled(false);
    fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

    File result = null;
    if (fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
      result = fileChooser.getSelectedFile();
    }
    return result;
  }

  @Override
  public void onUndefinedPredicateWarning(final JProlContext source,
                                          final JProlChoicePoint choicePoint,
                                          final String undefinedPredicateSignature) {
    this.messageEditor.addWarningText(
        "Detected undefined predicate: " + undefinedPredicateSignature);
  }

  @Override
  public void onChoicePointTraceEvent(final JProlContext source, final JProlChoicePoint choicePoint,
                                      final TraceEvent event) {
    switch (event) {
      case CALL:
        this.traceEditor.addCallText(choicePoint.getGoalTerm().forWrite());
        break;
      case REDO:
        this.traceEditor.addRedoText(choicePoint.getGoalTerm().forWrite());
        break;
      case FAIL:
        this.traceEditor.addFailText(choicePoint.getGoalTerm().forWrite());
        break;
      case EXIT:
        this.traceEditor.addExitText(choicePoint.getGoalTerm().forWrite());
        break;
    }
  }

  private void fillLFMenuItem() {
    final LookAndFeelInfo[] plaf = UIManager.getInstalledLookAndFeels();
    this.lookAndFeelMap = new HashMap<>();
    final ButtonGroup lfGroup = new ButtonGroup();

    final ActionListener lfListener = (final ActionEvent e) -> {
      final JLFRadioButtonItem item = (JLFRadioButtonItem) e.getSource();
      setSelectedLookAndFeel(item.getLfClassName());
    };

    for (LookAndFeelInfo aPlaf : plaf) {
      lookAndFeelMap.put(aPlaf.getClassName(), aPlaf);
      JRadioButtonMenuItem menuItem = new JLFRadioButtonItem(aPlaf.getName(), aPlaf.getClassName());

      menuItem.addActionListener(lfListener);

      lfGroup.add(menuItem);
      menuLookAndFeel.add(menuItem);
    }
  }

  private void setSelectedLookAndFeel(String lookAndFeelClassName) {

    if (lookAndFeelClassName != null) {
      if (!this.lookAndFeelMap.containsKey(lookAndFeelClassName)) {
        lookAndFeelClassName = null;
      }
    }

    if (lookAndFeelClassName == null) {
      lookAndFeelClassName = UIManager.getSystemLookAndFeelClassName();
    }

    final LookAndFeelInfo feelInfo = this.lookAndFeelMap.get(lookAndFeelClassName);
    final JFrame thisFrame = this;

    for (int li = 0; li < this.menuLookAndFeel.getItemCount(); li++) {
      final JLFRadioButtonItem menuItem = (JLFRadioButtonItem) this.menuLookAndFeel.getItem(li);
      if (menuItem.getLfClassName().equals(lookAndFeelClassName) && !menuItem.isSelected()) {
        menuItem.setSelected(true);
        break;
      }
    }

    final Runnable runnable = () -> {
      try {
        UIManager.setLookAndFeel(feelInfo.getClassName());
        UIManager.put("TextPane[Enabled].backgroundPainter",
            (Painter<JComponent>) (Graphics2D g, JComponent comp, int width1, int height1) -> {
              g.setColor(comp.getBackground());
              g.fillRect(0, 0, width1, height1);
            });
      } catch (ClassNotFoundException | IllegalAccessException | InstantiationException |
               UnsupportedLookAndFeelException ex) {
        LOG.throwing(thisFrame.getClass().getCanonicalName(), "L&F", ex);
      }

      SwingUtilities.updateComponentTreeUI(this);
      this.repaint();
    };

    UiUtils.doInSwingThread(runnable);
  }

  /**
   * This method is called from within the constructor to initialize the form.
   * WARNING: Do NOT modify this code. The content of this method is always
   * regenerated by the Form Editor.
   */
  private void initComponents() {
    java.awt.GridBagConstraints gridBagConstraints;

    splitPaneMain = new javax.swing.JSplitPane();
    splitPaneTop = new javax.swing.JSplitPane();
    try {
      dialogEditor = new com.igormaznitsa.jprol.easygui.DialogEditor();
    } catch (java.io.IOException e1) {
      e1.printStackTrace();
    }
    editorPanel = new javax.swing.JPanel();
    sourceEditor = new com.igormaznitsa.jprol.easygui.PrologSourceEditor();
    panelFindText = new javax.swing.JPanel();
    labelFind = new javax.swing.JLabel();
    textFind = new javax.swing.JTextField();
    buttonCloseFind = new javax.swing.JButton();
    filler1 = new javax.swing.Box.Filler(new java.awt.Dimension(0, 0), new java.awt.Dimension(0, 0),
        new java.awt.Dimension(32767, 0));
    splitPanelDown = new javax.swing.JSplitPane();
    messageEditor = new com.igormaznitsa.jprol.easygui.MessageEditor();
    traceEditor = new com.igormaznitsa.jprol.easygui.TraceDialog();
    panelProgress = new javax.swing.JPanel();
    progressBarTask = new javax.swing.JProgressBar();
    buttonStopExecuting = new javax.swing.JButton();
    jMenuBar1 = new javax.swing.JMenuBar();
    menuFile = new javax.swing.JMenu();
    menuFileNew = new javax.swing.JMenuItem();
    menuFileOpen = new javax.swing.JMenuItem();
    menuFileSaveAs = new javax.swing.JMenuItem();
    menuFileSave = new javax.swing.JMenuItem();
    jSeparator1 = new javax.swing.JPopupMenu.Separator();
    menuFileRecentFiles = new javax.swing.JMenu();
    jSeparator4 = new javax.swing.JPopupMenu.Separator();
    menuExit = new javax.swing.JMenuItem();
    menuEdit = new javax.swing.JMenu();
    menuUndo = new javax.swing.JMenuItem();
    menuRedo = new javax.swing.JMenuItem();
    jSeparator2 = new javax.swing.JPopupMenu.Separator();
    menuClearText = new javax.swing.JMenuItem();
    menuEditCommentSelected = new javax.swing.JMenuItem();
    menuEditUncommentSelected = new javax.swing.JMenuItem();
    jSeparator3 = new javax.swing.JPopupMenu.Separator();
    menuitemFindText = new javax.swing.JMenuItem();
    menuItemWordWrapSources = new javax.swing.JCheckBoxMenuItem();
    menuItemFullScreen = new javax.swing.JMenuItem();
    jSeparator5 = new javax.swing.JPopupMenu.Separator();
    menuEditOptions = new javax.swing.JMenuItem();
    menuRun = new javax.swing.JMenu();
    menuRunScript = new javax.swing.JMenuItem();
    menuTraceScript = new javax.swing.JMenuItem();
    menuRunStop = new javax.swing.JMenuItem();
    menuView = new javax.swing.JMenu();
    menuViewKnowledgeBase = new javax.swing.JMenuItem();
    menuItemLibraryInfo = new javax.swing.JMenuItem();
    menuLookAndFeel = new javax.swing.JMenu();
    menuHelp = new javax.swing.JMenu();
    menuHelpHelp = new javax.swing.JMenuItem();
    menuAbout = new javax.swing.JMenuItem();

    setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);

    splitPaneMain.setDividerLocation(350);
    splitPaneMain.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
    splitPaneMain.setResizeWeight(0.8);
    splitPaneMain.setOneTouchExpandable(true);

    splitPaneTop.setDividerLocation(500);
    splitPaneTop.setResizeWeight(0.9);
    splitPaneTop.setOneTouchExpandable(true);

    dialogEditor.setToolTipText("The window allows to communicate with a user");
    splitPaneTop.setRightComponent(dialogEditor);

    editorPanel.setBorder(javax.swing.BorderFactory.createTitledBorder("Editor"));
    editorPanel.setLayout(new java.awt.BorderLayout());

    sourceEditor.setBorder(null);
    sourceEditor.setToolTipText("The editor allows to enter and edit text of a program");
    sourceEditor.setFont(new java.awt.Font("DejaVu Sans", Font.BOLD, 13)); // NOI18N
    editorPanel.add(sourceEditor, java.awt.BorderLayout.CENTER);

    panelFindText.setLayout(new java.awt.GridBagLayout());

    labelFind.setText("Find:");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 0;
    panelFindText.add(labelFind, gridBagConstraints);

    textFind.setToolTipText("Enter text for search (wildcard chars ? and * are supported)");
    textFind.addKeyListener(new java.awt.event.KeyAdapter() {
      @Override
      public void keyReleased(java.awt.event.KeyEvent evt) {
        textFindKeyReleased(evt);
      }
    });
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.ipadx = 300;
    panelFindText.add(textFind, gridBagConstraints);

    buttonCloseFind.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/cross.png"))); // NOI18N
    buttonCloseFind.setToolTipText("Hide the find text panel (ESC)");
    buttonCloseFind.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
    buttonCloseFind.setIconTextGap(0);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 0;
    panelFindText.add(buttonCloseFind, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.weightx = 1000.0;
    panelFindText.add(filler1, gridBagConstraints);

    editorPanel.add(panelFindText, java.awt.BorderLayout.PAGE_END);

    splitPaneTop.setLeftComponent(editorPanel);

    splitPaneMain.setTopComponent(splitPaneTop);

    splitPanelDown.setDividerLocation(500);
    splitPanelDown.setResizeWeight(0.8);
    splitPanelDown.setOneTouchExpandable(true);

    messageEditor.setToolTipText("The window shows messages during an execution of the script");
    splitPanelDown.setLeftComponent(messageEditor);

    traceEditor.setToolTipText(
        "The window shows trace information if the engine is being started at the trace mode");
    splitPanelDown.setRightComponent(traceEditor);

    splitPaneMain.setBottomComponent(splitPanelDown);

    getContentPane().add(splitPaneMain, java.awt.BorderLayout.CENTER);

    panelProgress.setBorder(javax.swing.BorderFactory.createEtchedBorder());
    panelProgress.setLayout(new java.awt.GridBagLayout());

    progressBarTask.setMaximumSize(new java.awt.Dimension(100, 20));
    progressBarTask.setPreferredSize(new java.awt.Dimension(40, 20));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 1000.0;
    panelProgress.add(progressBarTask, gridBagConstraints);

    buttonStopExecuting.setBackground(new java.awt.Color(255, 156, 156));
    buttonStopExecuting.setFont(new java.awt.Font("DejaVu Sans", Font.BOLD, 13)); // NOI18N
    buttonStopExecuting.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/flag_red.png"))); // NOI18N
    buttonStopExecuting.setText("STOP");
    buttonStopExecuting.setBorder(
        new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
    buttonStopExecuting.setMaximumSize(new java.awt.Dimension(100, 23));
    buttonStopExecuting.setMinimumSize(new java.awt.Dimension(60, 23));
    buttonStopExecuting.addActionListener(this::buttonStopExecutingActionPerformed);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    panelProgress.add(buttonStopExecuting, gridBagConstraints);

    getContentPane().add(panelProgress, java.awt.BorderLayout.SOUTH);

    menuFile.setText("File");

    menuFileNew.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/page.png"))); // NOI18N
    menuFileNew.setText("New");
    menuFileNew.setToolTipText("Create new document");
    menuFileNew.addActionListener(this::menuFileNewActionPerformed);
    menuFile.add(menuFileNew);

    menuFileOpen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_O,
        InputEvent.CTRL_DOWN_MASK));
    menuFileOpen.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/page_edit.png"))); // NOI18N
    menuFileOpen.setText("Open");
    menuFileOpen.setToolTipText("Open a saved document");
    menuFileOpen.addActionListener(this::menuFileOpenActionPerformed);
    menuFile.add(menuFileOpen);

    menuFileSaveAs.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/page_save.png"))); // NOI18N
    menuFileSaveAs.setText("Save As..");
    menuFileSaveAs.setToolTipText("Save the current document as a file");
    menuFileSaveAs.addActionListener(this::menuFileSaveAsActionPerformed);
    menuFile.add(menuFileSaveAs);

    menuFileSave.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S,
        InputEvent.CTRL_DOWN_MASK));
    menuFileSave.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/page_go.png"))); // NOI18N
    menuFileSave.setText("Save");
    menuFileSave.setToolTipText("Save the current document");
    menuFileSave.addActionListener(this::menuFileSaveActionPerformed);
    menuFile.add(menuFileSave);
    menuFile.add(jSeparator1);

    menuFileRecentFiles.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/folder.png"))); // NOI18N
    menuFileRecentFiles.setText("Recent files...");
    menuFileRecentFiles.setToolTipText("List of files opened early");
    menuFileRecentFiles.addMenuListener(new javax.swing.event.MenuListener() {
      @Override
      public void menuSelected(javax.swing.event.MenuEvent evt) {
        menuFileRecentFilesMenuSelected(evt);
      }

      @Override
      public void menuDeselected(javax.swing.event.MenuEvent evt) {
      }

      @Override
      public void menuCanceled(javax.swing.event.MenuEvent evt) {
      }
    });
    menuFile.add(menuFileRecentFiles);
    menuFile.add(jSeparator4);

    menuExit.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F4,
        InputEvent.ALT_DOWN_MASK));
    menuExit.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/door_in.png"))); // NOI18N
    menuExit.setText("Exit");
    menuExit.setToolTipText("Close the editor");
    menuExit.addActionListener(this::menuExitActionPerformed);
    menuFile.add(menuExit);

    jMenuBar1.add(menuFile);

    menuEdit.setText("Edit");

    menuUndo.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z,
        InputEvent.CTRL_DOWN_MASK));
    menuUndo.setIcon(new javax.swing.ImageIcon(getClass().getResource(
        "/com/igormaznitsa/jprol/easygui/icons/book_previous.png"))); // NOI18N
    menuUndo.setText("Undo");
    menuUndo.setToolTipText("Undo last changes in the document");
    menuUndo.setEnabled(false);
    menuUndo.addActionListener(this::menuUndoActionPerformed);
    menuEdit.add(menuUndo);

    menuRedo.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Y,
        InputEvent.CTRL_DOWN_MASK));
    menuRedo.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/book_next.png"))); // NOI18N
    menuRedo.setText("Redo");
    menuRedo.setToolTipText("Redo canceled changes in the document");
    menuRedo.setEnabled(false);
    menuRedo.addActionListener(this::menuRedoActionPerformed);
    menuEdit.add(menuRedo);
    menuEdit.add(jSeparator2);

    menuClearText.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_R,
        InputEvent.CTRL_DOWN_MASK));
    menuClearText.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/page_white.png"))); // NOI18N
    menuClearText.setText("Clear");
    menuClearText.setToolTipText("Just clear text in the current document");
    menuClearText.addActionListener(this::menuClearTextActionPerformed);
    menuEdit.add(menuClearText);

    menuEditCommentSelected.setAccelerator(
        javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_5,
            InputEvent.CTRL_DOWN_MASK));
    menuEditCommentSelected.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/comment_add.png"))); // NOI18N
    menuEditCommentSelected.setText("Comment selection");
    menuEditCommentSelected.setToolTipText(
        "Place the commenting symbol as the first one into selected lines");
    menuEditCommentSelected.addActionListener(this::menuEditCommentSelectedActionPerformed);
    menuEdit.add(menuEditCommentSelected);

    menuEditUncommentSelected.setAccelerator(
        javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_U,
            InputEvent.CTRL_DOWN_MASK));
    menuEditUncommentSelected.setIcon(new javax.swing.ImageIcon(getClass().getResource(
        "/com/igormaznitsa/jprol/easygui/icons/comment_delete.png"))); // NOI18N
    menuEditUncommentSelected.setText("Uncomment selection");
    menuEditUncommentSelected.setToolTipText(
        "Remove the first commenting symbol from selected lines");
    menuEditUncommentSelected.addActionListener(this::menuEditUncommentSelectedActionPerformed);
    menuEdit.add(menuEditUncommentSelected);
    menuEdit.add(jSeparator3);

    menuitemFindText.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F,
        InputEvent.CTRL_DOWN_MASK));
    menuitemFindText.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/zoom.png"))); // NOI18N
    menuitemFindText.setText("Find text");
    menuitemFindText.addActionListener(this::menuitemFindTextActionPerformed);
    menuEdit.add(menuitemFindText);

    menuItemWordWrapSources.setAccelerator(
        javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W,
            InputEvent.CTRL_DOWN_MASK));
    menuItemWordWrapSources.setSelected(true);
    menuItemWordWrapSources.setText("Word wrap (editor)");
    menuItemWordWrapSources.setToolTipText("Word-wrap mode for the document editor");
    menuItemWordWrapSources.setIcon(new javax.swing.ImageIcon(getClass().getResource(
        "/com/igormaznitsa/jprol/easygui/icons/text_align_justify.png"))); // NOI18N
    menuItemWordWrapSources.addActionListener(this::menuItemWordWrapSourcesActionPerformed);
    menuEdit.add(menuItemWordWrapSources);

    menuItemFullScreen.setAccelerator(
        javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F,
            java.awt.event.InputEvent.SHIFT_DOWN_MASK | java.awt.event.InputEvent.CTRL_DOWN_MASK));
    menuItemFullScreen.setIcon(new javax.swing.ImageIcon(getClass().getResource(
        "/com/igormaznitsa/jprol/easygui/icons/shape_move_forwards.png"))); // NOI18N
    menuItemFullScreen.setText("Full screen");
    menuItemFullScreen.setToolTipText(
        "Turn on the full screen mode if it is supported by the device");
    menuItemFullScreen.addActionListener(this::menuItemFullScreenActionPerformed);
    menuEdit.add(menuItemFullScreen);
    menuEdit.add(jSeparator5);

    menuEditOptions.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/cog.png"))); // NOI18N
    menuEditOptions.setText("Options");
    menuEditOptions.setToolTipText("Open editor options");
    menuEditOptions.addActionListener(this::menuEditOptionsActionPerformed);
    menuEdit.add(menuEditOptions);

    jMenuBar1.add(menuEdit);

    menuRun.setText("Run");

    menuRunScript.setAccelerator(
        javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F5, 0));
    menuRunScript.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/flag_green.png"))); // NOI18N
    menuRunScript.setText("Start");
    menuRunScript.setToolTipText("Execute the current document");
    menuRunScript.addActionListener(this::menuRunScriptActionPerformed);
    menuRun.add(menuRunScript);

    menuTraceScript.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/flag_blue.png"))); // NOI18N
    menuTraceScript.setText("Trace");
    menuTraceScript.setToolTipText("Execute the current document with tracing");
    menuTraceScript.addActionListener(this::menuTraceScriptActionPerformed);
    menuRun.add(menuTraceScript);

    menuRunStop.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/flag_red.png"))); // NOI18N
    menuRunStop.setText("Stop");
    menuRunStop.setToolTipText("Stop the current execution");
    menuRunStop.setEnabled(false);
    menuRunStop.addActionListener(this::menuRunStopActionPerformed);
    menuRun.add(menuRunStop);

    jMenuBar1.add(menuRun);

    menuView.setText("View");

    menuViewKnowledgeBase.setAccelerator(
        javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F12, 0));
    menuViewKnowledgeBase.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/eye.png"))); // NOI18N
    menuViewKnowledgeBase.setText("Show Knowledge base");
    menuViewKnowledgeBase.setToolTipText(
        "Take and show the snapshot of the current knowledge base saved in the memory");
    menuViewKnowledgeBase.setEnabled(false);
    menuViewKnowledgeBase.addActionListener(this::menuViewKnowledgeBaseActionPerformed);
    menuView.add(menuViewKnowledgeBase);

    menuItemLibraryInfo.setAccelerator(
        javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F1, 0));
    menuItemLibraryInfo.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/table.png"))); // NOI18N
    menuItemLibraryInfo.setText("Library info");
    menuItemLibraryInfo.setToolTipText("Show all predicates found in embedded libraries");
    menuItemLibraryInfo.addActionListener(this::menuItemLibraryInfoActionPerformed);
    menuView.add(menuItemLibraryInfo);

    jMenuBar1.add(menuView);

    menuLookAndFeel.setText("Look&Feel");
    jMenuBar1.add(menuLookAndFeel);

    menuHelp.setText("Help");

    menuHelpHelp.setIcon(new javax.swing.ImageIcon(
        getClass().getResource("/com/igormaznitsa/jprol/easygui/icons/information.png"))); // NOI18N
    menuHelpHelp.setText("Help");
    menuHelpHelp.setToolTipText("Show information about usage of the utility");
    menuHelpHelp.addActionListener(this::menuHelpHelpActionPerformed);
    menuHelp.add(menuHelpHelp);

    menuAbout.setIcon(new javax.swing.ImageIcon(getClass().getResource(
        "/com/igormaznitsa/jprol/easygui/icons/emoticon_smile.png"))); // NOI18N
    menuAbout.setText("About");
    menuAbout.setToolTipText("Show the information about the application and license");
    menuAbout.addActionListener(this::menuAboutActionPerformed);
    menuHelp.add(menuAbout);

    jMenuBar1.add(menuHelp);

    setJMenuBar(jMenuBar1);

    pack();
  }

  private void menuRunScriptActionPerformed(java.awt.event.ActionEvent evt) {
    startExecution(false);
  }

  private void menuUndoActionPerformed(java.awt.event.ActionEvent evt) {
    try {
      this.sourceEditor.getUndoManager().undo();
    } catch (CannotUndoException ex) {
      // ignore
    }
    UndoManager undo = sourceEditor.getUndoManager();
    this.menuUndo.setEnabled(undo.canUndo());
    this.menuRedo.setEnabled(undo.canRedo());

  }

  private void menuRedoActionPerformed(java.awt.event.ActionEvent evt) {
    try {
      this.sourceEditor.getUndoManager().redo();
    } catch (CannotRedoException ex) {
      // ignore
    }
    UndoManager undo = this.sourceEditor.getUndoManager();
    this.menuUndo.setEnabled(undo.canUndo());
    this.menuRedo.setEnabled(undo.canRedo());

  }

  private void menuExitActionPerformed(java.awt.event.ActionEvent evt) {
    windowClosing(null);
  }

  private void menuClearTextActionPerformed(java.awt.event.ActionEvent evt) {
    if (this.sourceEditor.getEditor().getDocument().getLength() > 10) {
      if (JOptionPane.showConfirmDialog(this, "Do you really want to clean?", "Confirmation",
          JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
        this.sourceEditor.getUndoManager().discardAllEdits();
        this.sourceEditor.clearText();
      }
    }
  }

  private void menuFileOpenActionPerformed(java.awt.event.ActionEvent evt) {
    loadFile(this.lastOpenedFile, false);
  }

  private void menuFileSaveActionPerformed(java.awt.event.ActionEvent evt) {
    saveFile(false);
  }

  private void menuFileSaveAsActionPerformed(java.awt.event.ActionEvent evt) {
    saveFile(true);
  }

  private void buttonStopExecutingActionPerformed(java.awt.event.ActionEvent evt) {
    final Thread executingThread = this.currentExecutedScriptThread.get();

    SwingUtilities.invokeLater(() -> {
      if (executingThread != null) {
        try {
          executingThread.interrupt();
          dialogEditor.cancelRead();
          executingThread.join();
        } catch (Throwable tr) {
          tr.printStackTrace();
        } finally {
          hideTaskControlPanel();
        }
        messageEditor.addWarningText("Execution has been canceled.");
      }
    });

  }

  private void menuAboutActionPerformed(java.awt.event.ActionEvent evt) {
    final JHtmlLabel label = new JHtmlLabel(
        "<html><body><h1>JProl Notepad</h1><b>Version:</b> " + VERSION +
            "<br><b>Project page:</b> <a href=\"https://github.com/raydac/jprol\">https://github.com/raydac/jprol</a><br><b>Author:</b> Igor Maznitsa (<a href=\"https://www.igormaznitsa.com\">https://www.igormaznitsa.com</a>)<br><br>(C)2010-2025 Igor A. Maznitsa. <a href=\"https://www.apache.org/licenses/LICENSE-2.0\">Apache 2.0 License</a><br>Icons provided by free icon set <a href=\"http://www.famfamfam.com/lab/icons/silk/\">http://www.famfamfam.com/lab/icons/silk/</a><br><br>If you like the application you could make some donation:<br><ul><li><a href=\"https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=AHWJHJFBAWGL2\">PayPal</a></li><li><a href=\"https://yoomoney.ru/to/41001158080699\">YooMoney</a></li></ul><hr>Editor with third party libraries:<ul><li><a href=\"https://github.com/bobbylight/RSyntaxTextArea\"><b>RSyntaxTextArea</b></a> <a href=\"https://raw.githubusercontent.com/bobbylight/RSyntaxTextArea/master/src/main/dist/RSyntaxTextArea.License.txt\">under modified BSD license</a></li></ul></body></html>");
    label.addLinkListener((final JHtmlLabel source, final String link) -> {
      try {
        final Desktop desktop = Desktop.isDesktopSupported() ? Desktop.getDesktop() : null;
        if (desktop != null && desktop.isSupported(Desktop.Action.BROWSE)) {
          desktop.browse(new URI(link));
        }
      } catch (Exception ex) {
        LOG.log(Level.SEVERE, "Can't open URL : " + link, ex);
      }
    });
    JOptionPane.showMessageDialog(this, label, "About", JOptionPane.INFORMATION_MESSAGE,
        new ImageIcon(this.getIconImage()));
  }

  private void menuViewKnowledgeBaseActionPerformed(java.awt.event.ActionEvent evt) {
    if (lastContext == null) {
      return;
    }

    final KnowledgeBaseSnapshotViewDialog dialog =
        new KnowledgeBaseSnapshotViewDialog(this, lastContext);

    dialog.setSize(600, 400);

    dialog.setLocationRelativeTo(this);

    dialog.setVisible(true);
  }

  private void menuRunStopActionPerformed(java.awt.event.ActionEvent evt) {
    buttonStopExecutingActionPerformed(evt);
  }

  private void menuHelpHelpActionPerformed(java.awt.event.ActionEvent evt) {
    new HelpDialog(this).setVisible(true);
  }

  private void menuEditOptionsActionPerformed(java.awt.event.ActionEvent evt) {
    OptionsDialog dialog = new OptionsDialog(this,
        new TreeModel[] {sourceEditor, dialogEditor, messageEditor, traceEditor});
    dialog.setVisible(true);
  }

  private void menuItemLibraryInfoActionPerformed(java.awt.event.ActionEvent evt) {
    final java.util.List<String> list = new ArrayList<>();
    list.add(JProlCoreLibrary.class.getCanonicalName());
    list.addAll(Arrays.asList(PROL_LIBRARIES));
    list.add(MainFrame.class.getCanonicalName() + "$LogLibrary");

    final LibraryInfoDialog infoDialog;
    try {
      infoDialog = new LibraryInfoDialog(this, list.toArray(new String[0]));
    } catch (Exception ex) {
      LOG.throwing(this.getClass().getCanonicalName(), "MenuItemLibraryInfoActionPerformed()", ex);
      this.messageEditor.addErrorText("Can't show library info dialog [" + ex.getMessage() + ']');
      return;
    }

    infoDialog.setSize(512, 480);

    infoDialog.setLocationRelativeTo(this);

    infoDialog.setVisible(true);
    infoDialog.dispose();
  }

  private void menuItemWordWrapSourcesActionPerformed(java.awt.event.ActionEvent evt) {
    this.sourceEditor.setEdWordWrap(this.menuItemWordWrapSources.isSelected());
  }

  private void menuFileNewActionPerformed(java.awt.event.ActionEvent evt) {
    final Thread executingThread = this.currentExecutedScriptThread.get();
    if (executingThread != null && executingThread.isAlive()) {
      JOptionPane.showMessageDialog(this, "Wait until current Prolog application is completed.",
          "Can't create new one", JOptionPane.WARNING_MESSAGE);
    } else {
      if (this.documentHasBeenChangedFlag) {
        if (JOptionPane.showConfirmDialog(this,
            "Document is changed and not saved. Do you really want to make new one?", "Warning",
            JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {
          newFile();
        }
      } else {
        newFile();
      }
    }
  }

  private void startExecution(final boolean tracing) {
    final Thread executingThread = this.currentExecutedScriptThread.get();

    if (executingThread != null && executingThread.isAlive()) {
      JOptionPane.showMessageDialog(this, "Prolog program is already started!.", "Can't trace",
          JOptionPane.WARNING_MESSAGE);
    } else {
      if (!this.currentExecutedScriptThread.compareAndSet(executingThread, null)) {
        return;
      }

      final Thread newThread =
          new Thread(this.executingScripts, this, tracing ? "JPROL_TRACING_EXEC" : "JPROL_EXEC");
      newThread.setDaemon(false);

      if (this.currentExecutedScriptThread.compareAndSet(null, newThread)) {
        this.startedInTracing.set(tracing);
        SwingUtilities.invokeLater(() -> {
          clearTextAtAllWindowsExcludeSource();
          dialogEditor.initBeforeSession();
          showTaskControlPanel();
          newThread.start();
        });
      }
    }
  }

  private void menuTraceScriptActionPerformed(java.awt.event.ActionEvent evt) {
    startExecution(true);
  }

  private void menuFileRecentFilesMenuSelected(javax.swing.event.MenuEvent evt) {
    JMenu menu = (JMenu) evt.getSource();
    menu.removeAll();
    for (final String path : this.recentFiles.getCollection()) {
      final JMenuItem newItem = new JMenuItem(path);
      newItem.setActionCommand(path);
      newItem.addActionListener(new ActionListener() {

        @Override
        public void actionPerformed(final ActionEvent e) {
          final String text = e.getActionCommand();
          if (text != null) {
            try {
              loadFile(new File(text), true);
            } catch (Exception ex) {

              LOG.throwing(this.getClass().getCanonicalName(), "MenuFileRecentFilesMenuSelected()",
                  ex);
            }
          }
        }
      });
      menu.add(newItem);
    }
  }

  private void menuEditCommentSelectedActionPerformed(java.awt.event.ActionEvent evt) {
    if (this.sourceEditor.commentSelectedLines()) {
      documentChanged();
    }
  }

  private void menuEditUncommentSelectedActionPerformed(java.awt.event.ActionEvent evt) {
    if (this.sourceEditor.uncommentSelectedLines()) {
      documentChanged();
    }
  }

  private void menuItemFullScreenActionPerformed(java.awt.event.ActionEvent evt) {
    final GraphicsDevice gd =
        GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
    if (gd != null && gd.isFullScreenSupported()) {
      if (gd.getFullScreenWindow() == null) {
        gd.setFullScreenWindow(this);
      } else {
        gd.setFullScreenWindow(null);
      }
    }
  }

  private void menuitemFindTextActionPerformed(java.awt.event.ActionEvent evt) {
    this.panelFindText.setVisible(true);
    this.textFind.setText("");
    this.textFind.requestFocus();
  }

  private int searchText(final String text, final Pattern pattern, final int cursorPos) {
    if (cursorPos >= text.length()) {
      return -1;
    }

    final Matcher matcher = pattern.matcher(text);
    if (matcher.find(cursorPos)) {
      return matcher.start();
    }
    return -1;
  }

  private void textFindKeyReleased(java.awt.event.KeyEvent evt) {
    if (evt.getKeyCode() == KeyEvent.VK_ENTER) {
      final Pattern patternToFind = UiUtils.makePattern(textFind.getText());
      final String text = this.sourceEditor.getText();
      int cursorPos = searchText(text, patternToFind, this.sourceEditor.getCaretPosition() + 1);

      if (cursorPos < 0) {
        cursorPos = searchText(text, patternToFind, 0);
      }
      if (cursorPos >= 0) {
        this.sourceEditor.setCaretPosition(cursorPos);
      }
    }
  }

  private void setLastContext(JProlContext context) {
    this.lastContext = context;
    this.menuViewKnowledgeBase.setEnabled(lastContext != null);
  }

  @Override
  public Reader findReader(final JProlContext context, final String id) {
    boolean successful = false;
    boolean notTraceable = false;
    try {
      if (id.equals("user")) {
        successful = true;
        notTraceable = true;
        return this.dialogEditor.getInputReader();
      } else {
        final FileReader reader = new FileReader(id);
        successful = true;
        return reader;
      }
    } catch (IOException ex) {
      throw new ProlCriticalError("Can't provide reader", ex);
    } finally {
      if (!notTraceable) {
        if (successful) {
          this.messageEditor.addInfoText(format("Reader for '%s' has been opened.", id));
        } else {
          this.messageEditor.addWarningText(format("Reader for '%s' can't be opened.", id));
        }
      }
    }
  }

  @Override
  public Writer findWriter(JProlContext context, String id, boolean append) {
    boolean successful = false;
    boolean notTraceable = false;

    try {
      if (id.equals("user")) {
        successful = true;
        notTraceable = true;
        return this.dialogEditor.getOutputWriter();
      } else {
        final Writer writer = new FileWriter(id, append);
        successful = true;
        notTraceable = false;

        return writer;
      }
    } catch (IOException ex) {
      throw new ProlCriticalError("IOException", ex);
    } finally {
      if (!notTraceable) {
        if (successful) {
          this.messageEditor.addInfoText(format("Writer for '%s' has been opened.", id));
        } else {
          this.messageEditor.addWarningText(format("Writer for '%s' can't be opened.", id));
        }
      }
    }
  }

  private static Throwable findRootCause(final Throwable throwable) {
    if (throwable == null) {
      return null;
    } else if (throwable.getCause() == null) {
      return throwable;
    } else {
      final Throwable thatError = findRootCause(throwable.getCause());
      return thatError == null ? throwable : thatError;
    }
  }

  @Override
  public void run() {
    JProlContext context = null;
    boolean successfully = false;
    boolean canceled = false;
    ProlInterruptException haltException = null;
    PrologParserException parserException = null;

    long startTime = 0;

    final Thread executing = this.currentExecutedScriptThread.get();

    try {
      this.dialogEditor.setEnabled(true);
      this.dialogEditor.requestFocus();

      this.messageEditor.addInfoText("Creating Context...");

      try {
        context = new JProlContext("prol-script").addIoResourceProvider(this);
        context.addContextListener(this);
        if (this.startedInTracing.get()) {
          context.setSystemFlag(JProlSystemFlag.DEBUG, Terms.TRUE);
        } else {
          context.setSystemFlag(JProlSystemFlag.DEBUG, Terms.FALSE);
        }

        for (final String str : PROL_LIBRARIES) {
          final AbstractJProlLibrary lib =
              (AbstractJProlLibrary) Class.forName(str).getDeclaredConstructor().newInstance();

          context.addLibrary(lib);
          this.messageEditor.addInfoText(
              format("Library '%s' has been added...", lib.getLibraryUid()));
        }

        context.addLibrary(logLibrary);
        this.messageEditor.addInfoText(
            format("Library '%s' has been added...", logLibrary.getLibraryUid()));

        setLastContext(context);
      } catch (Throwable ex) {
        LOG.log(Level.WARNING, "ExecutionThread.run()", ex);
        this.messageEditor.addErrorText(
            "Can't create context for exception [" + ex.getMessage() + ']');
        return;
      }

      this.messageEditor.addInfoText("Consult with the script... ");

      startTime = System.currentTimeMillis();

      try {
        context.consult(new StringReader(sourceEditor.getText()), this);
        // wait for async threads
        context.getContextExecutorService().shutdown();
        if (!context.getContextExecutorService().awaitTermination(15, TimeUnit.SECONDS)) {
          context.getContextExecutorService().shutdownNow();
        }
      } catch (ProlPermissionErrorException ex) {
        LOG.log(Level.WARNING, "Permission error", ex);
        this.messageEditor.addErrorText(
            "Permission error [" + ex.getMessage() + ']');
        JOptionPane.showMessageDialog(this, "Out of Memory exception  detected!", "Error",
            JOptionPane.ERROR_MESSAGE);
        return;
      } catch (PrologParserException ex) {
        LOG.log(Level.WARNING, "ExecutionThread.run()", ex);
        parserException = ex;

        final Throwable cause = findRootCause(ex);

        if (cause instanceof StackOverflowError) {
          this.messageEditor.addErrorText("Stack Overflow!");
          JOptionPane.showMessageDialog(this, "Stack overflow exception detected!", "Error",
              JOptionPane.ERROR_MESSAGE);
          return;
        } else if (cause instanceof OutOfMemoryError) {
          this.messageEditor.addErrorText("Out of Memory!");
          JOptionPane.showMessageDialog(this, "Out of Memory exception  detected!", "Error",
              JOptionPane.ERROR_MESSAGE);
          return;
        }

        if (cause instanceof ProlInterruptException) {
          haltException = (ProlInterruptException) ex.getCause();
          canceled = cause instanceof ProlChoicePointInterruptedException;
        } else if (cause instanceof InterruptedException) {
          canceled = true;
        } else {
          this.messageEditor.addText("Parser exception [" + ex.getMessage() + ']',
              MessageEditor.TYPE_ERROR, "source://" + ex.getLine() + ';' + ex.getPos(),
              "line " + ex.getLine() + ":" + ex.getPos());
          return;
        }
      } catch (ThreadDeath death) {
        canceled = true;
      } catch (Throwable ex) {
        LOG.log(Level.WARNING, "ExecutionThread.run()", ex);

        if (ex instanceof ProlInterruptException ||
            ex.getCause() instanceof ProlInterruptException) {
          if (ex instanceof ProlInterruptException) {
            haltException = (ProlInterruptException) ex;
          } else {
            haltException = (ProlInterruptException) ex.getCause();
          }
        } else {
          this.messageEditor.addErrorText(
              "Can't parse script for exception [" + ex.getMessage() + ']');
          return;
        }
      }
      successfully = true;
    } finally {
      try {
        this.messageEditor.addInfoText(
            "Total time " + ((System.currentTimeMillis() - startTime) / 1000f) + " sec.");

        if (haltException == null) {
          if (!canceled) {
            if (successfully) {
              this.messageEditor.addInfoText("Completed successfully.");
            } else {
              this.messageEditor.addErrorText("Completed with errors or not started.");
              this.dialogEditor.addText(format("%nERROR! see 'Messages' log%n"));
            }
          }
        } else {
          this.messageEditor.addText("Halted [" + haltException.getMessage() + ']',
              MessageEditor.TYPE_WARNING, parserException != null ?
                  ("source://" + parserException.getLine() + ';' + parserException.getPos()) : null,
              parserException != null ?
                  ("line " + parserException.getLine() + ":" + parserException.getPos()) : null);
          this.dialogEditor.addText(
              format("%nScript execution stopped: %s%n", haltException.getMessage()));
        }
        this.dialogEditor.setEnabled(false);
        this.currentExecutedScriptThread.compareAndSet(executing, null);
      } finally {
        try {
          if (context != null) {
            context.dispose();
          }
        } finally {
          hideTaskControlPanel();
        }
      }
    }
  }

  @Override
  public void undoableEditHappened(final UndoableEditEvent e) {
    final UndoManager undo = this.sourceEditor.getUndoManager();
    undo.addEdit(e.getEdit());
    this.menuUndo.setEnabled(undo.canUndo());
    this.menuRedo.setEnabled(undo.canRedo());
  }

  @Override
  public void windowOpened(final WindowEvent e) {
  }

  @Override
  public void windowClosing(final WindowEvent e) {
    if (this.documentHasBeenChangedFlag) {
      if (JOptionPane.showConfirmDialog(this,
          "Document is changed but not saved. Do you really want to exit?", "Confirmation",
          JOptionPane.YES_NO_OPTION) == JOptionPane.NO_OPTION) {
        return;
      }
    }

    if (this.currentExecutedScriptThread.get() != null) {
      if (JOptionPane.showConfirmDialog(this,
          "Detected active task. Do you really want to force exit?", "Confirmation",
          JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {

        this.dialogEditor.cancelRead();
        this.dialogEditor.close();

        final Thread executingThread = this.currentExecutedScriptThread.get();

        try {
          executingThread.interrupt();
          executingThread.join();
        } catch (InterruptedException thr) {
          Thread.currentThread().interrupt();
        }
      } else {
        return;
      }
    } else {
      this.dialogEditor.cancelRead();
      this.dialogEditor.close();
    }

    savePreferences();

    try {
      this.dispose();
    } catch (Exception ex) {
      LOG.log(Level.SEVERE, "Error during close", ex);
    }
  }

  @Override
  public void windowClosed(final WindowEvent e) {
  }

  @Override
  public void windowIconified(final WindowEvent e) {
  }

  @Override
  public void windowDeiconified(final WindowEvent e) {
  }

  @Override
  public void windowActivated(final WindowEvent e) {
    if (this.currentExecutedScriptThread != null) {
      this.dialogEditor.requestFocus();
    } else {
      this.sourceEditor.requestFocus();
    }

  }

  @Override
  public void windowDeactivated(final WindowEvent e) {
  }

  @Override
  public void insertUpdate(final DocumentEvent e) {
    documentChanged();
  }

  @Override
  public void removeUpdate(final DocumentEvent e) {
    documentChanged();
  }

  @Override
  public void changedUpdate(final DocumentEvent e) {
    documentChanged();
  }

  private void documentChanged() {
    if (!this.documentHasBeenChangedFlag) {
      this.documentHasBeenChangedFlag = true;
      if (this.currentOpenedFile != null) {
        this.menuFileSave.setEnabled(true);
      }
      setTitle("*" + getTitle());
    }
  }

  private void setTextToDocument(final String text) {
    this.sourceEditor.clearText();
    this.sourceEditor.getEditor().setText(text);
    this.sourceEditor.setCaretPosition(0);

    this.menuFileSave.setEnabled(this.currentOpenedFile != null);

    this.sourceEditor.getUndoManager().discardAllEdits();
    this.menuUndo.setEnabled(false);
    this.menuRedo.setEnabled(false);

    this.documentHasBeenChangedFlag = false;
  }

  private void saveFile(final boolean saveAsAction) {
    File file = this.currentOpenedFile;
    if (saveAsAction || this.currentOpenedFile == null) {
      JFileChooser fileChooser = new JFileChooser(file);
      fileChooser.addChoosableFileFilter(PROL_FILE_FILTER);
      fileChooser.setAcceptAllFileFilterUsed(true);
      fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
      fileChooser.setDragEnabled(false);
      fileChooser.setMultiSelectionEnabled(false);
      fileChooser.setFileFilter(PROL_FILE_FILTER);

      if (fileChooser.showSaveDialog(this) == JFileChooser.APPROVE_OPTION) {
        file = fileChooser.getSelectedFile();

        if (!file.exists() && fileChooser.getFileFilter().equals(PROL_FILE_FILTER)) {
          // auto extension
          if (!file.getName().toLowerCase().endsWith(PROL_EXTENSION)) {
            file = new File(file.getAbsolutePath() + PROL_EXTENSION);
          }
        }

        if (saveAsAction || !file.equals(this.currentOpenedFile)) {
          if (file.exists()) {
            if (JOptionPane.showConfirmDialog(this,
                format("File '%s' exists, to overwrite it?", file.getAbsolutePath()), "File exists",
                JOptionPane.YES_NO_OPTION) == JOptionPane.NO_OPTION) {
              return;
            }
          }
        }
      } else {
        return;
      }
    }

    final String textFromEditor = this.sourceEditor.getEditor().getText();

    try {
      Utils.writeAsUtf8(file, textFromEditor);
      this.recentFiles.put(file.getAbsolutePath());
    } catch (Throwable thr) {
      LOG.throwing(this.getClass().getCanonicalName(), "saveFile()", thr);
      JOptionPane.showMessageDialog(this, format("Can't save file for error '%s'",
          (thr.getMessage() == null ? thr.getClass().getCanonicalName() :
              thr.getLocalizedMessage())), "Can't save file", JOptionPane.ERROR_MESSAGE);
      return;
    }

    this.currentOpenedFile = file;
    this.lastOpenedFile = currentOpenedFile;
    setTitle(this.currentOpenedFile.getAbsolutePath());

    this.sourceEditor.getUndoManager().discardAllEdits();
    this.menuFileSave.setEnabled(true);
    this.documentHasBeenChangedFlag = false;
  }

  private void newFile() {
    // make new

    this.sourceEditor.clearText();

    clearTextAtAllWindowsExcludeSource();

    this.currentOpenedFile = null;
    this.documentHasBeenChangedFlag = false;

    setTitle("JProl Notepad " + VERSION);

    repaint();
  }

  private void clearTextAtAllWindowsExcludeSource() {
    this.traceEditor.clearText();
    this.dialogEditor.clearText();
    this.messageEditor.clearText();
  }

  private void loadFile(final File file, final boolean justLoadFile) {
    if (this.documentHasBeenChangedFlag) {
      if (JOptionPane.showConfirmDialog(this, "Document is changed and not saved. To load new one?",
          "Warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE) ==
          JOptionPane.NO_OPTION) {
        return;
      }
    }

    JFileChooser fileChooser = new JFileChooser(file);
    if (!justLoadFile) {
      fileChooser.addChoosableFileFilter(PROL_FILE_FILTER);
      fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
      fileChooser.setDragEnabled(false);
      fileChooser.setMultiSelectionEnabled(false);
      fileChooser.setFileFilter(PROL_FILE_FILTER);
    }

    if (justLoadFile || fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
      File fileToOpen = justLoadFile ? file : fileChooser.getSelectedFile();

      this.lastOpenedFile = fileToOpen;

      try {
        setTextToDocument(Utils.readAsUtf8(fileToOpen));
        this.currentOpenedFile = fileToOpen;
        setTitle(this.currentOpenedFile.getCanonicalPath());
        this.repaint();

        this.recentFiles.put(fileToOpen.getAbsolutePath());

      } catch (Throwable thr) {
        LOG.throwing(this.getClass().getCanonicalName(), "loadFile()", thr);
        JOptionPane.showMessageDialog(this,
            format("Can't load file %s ! [%s]", fileToOpen.getAbsolutePath(), thr.getMessage()));
        this.recentFiles.remove(fileToOpen.getAbsolutePath());
      }
    }
  }

  private void showTaskControlPanel() {
    this.panelProgress.setVisible(true);
    this.progressBarTask.setIndeterminate(true);
    this.menuRunStop.setEnabled(true);
  }

  private void hideTaskControlPanel() {
    SwingUtilities.invokeLater(() -> {
      panelProgress.setVisible(false);
      progressBarTask.setIndeterminate(false);
      menuRunStop.setEnabled(false);
    });
  }

  @Override
  public void hyperlinkUpdate(HyperlinkEvent e) {
    if (e.getEventType() != HyperlinkEvent.EventType.ACTIVATED) {
      return;
    }

    String path = e.getDescription();
    if (path.startsWith("source://")) {
      path = path.substring(9);
      String[] parsed = path.split(";");
      if (parsed.length == 2) {
        try {
          int line = Integer.parseInt(parsed[0].trim());
          int pos = Integer.parseInt(parsed[1].trim());

          this.sourceEditor.setCaretPosition(line, pos + 1);
        } catch (Exception ex) {
          ex.printStackTrace();
        }
      }
    }
  }

  private void loadPreferences() {
    final Preferences prefs = Preferences.userNodeForPackage(this.getClass());

    setSelectedLookAndFeel(prefs.get("lookandfeel", null));

    int recentFileIndex = 1;
    this.recentFiles.clear();
    while (!Thread.currentThread().isInterrupted()) {
      final String path = prefs.get("RecentFile" + recentFileIndex, null);
      if (path == null) {
        break;
      }
      this.recentFiles.add(path);
      recentFileIndex++;
    }

    if (prefs.getBoolean("maximized", false)) {
      setExtendedState(JFrame.MAXIMIZED_BOTH);
      invalidate();
      doLayout();
    } else {
      setSize(prefs.getInt("mainwidth", 640), prefs.getInt("mainheight", 600));
      setLocation(prefs.getInt("mainx", 0), prefs.getInt("mainy", 0));
    }

    this.splitPaneMain.setDividerLocation(prefs.getInt("splitpanemainpos", 400));
    this.splitPaneTop.setDividerLocation(prefs.getInt("splitpanetoppos", 300));

    final String lastFile = prefs.get("lastfile", "");
    if (!lastFile.isEmpty()) {
      this.lastOpenedFile = new File(lastFile);
    } else {
      this.lastOpenedFile = null;
    }

    this.sourceEditor.loadPreferences(prefs);
    this.messageEditor.loadPreferences(prefs);
    this.dialogEditor.loadPreferences(prefs);
    this.traceEditor.loadPreferences(prefs);
  }

  private void savePreferences() {
    final Preferences prefs = Preferences.userNodeForPackage(this.getClass());

    prefs.put("lookandfeel", UIManager.getLookAndFeel().getClass().getName());

    int recentFileIndex = 1;
    for (final String recentFile : this.recentFiles.getCollection()) {
      prefs.put("RecentFile" + recentFileIndex, recentFile);
      recentFileIndex++;
    }

    prefs.putBoolean("maximized",
        (getExtendedState() & JFrame.MAXIMIZED_BOTH) == JFrame.MAXIMIZED_BOTH);

    prefs.putInt("mainwidth", getWidth());
    prefs.putInt("mainheight", getHeight());

    prefs.putInt("mainx", getX());
    prefs.putInt("mainy", getY());

    prefs.putInt("splitpanemainpos", splitPaneMain.getDividerLocation());
    prefs.putInt("splitpanetoppos", splitPaneTop.getDividerLocation());

    prefs.put("lastfile", this.lastOpenedFile == null ? "" : this.lastOpenedFile.getAbsolutePath());

    this.sourceEditor.savePreferences(prefs);
    this.messageEditor.savePreferences(prefs);
    this.dialogEditor.savePreferences(prefs);
    this.traceEditor.savePreferences(prefs);
  }

  public MessageEditor getMessageEditor() {
    return this.messageEditor;
  }

  @Override
  public boolean onFoundInteractiveGoal(final JProlContext context, final Term goal) {
    return context.findResourceWriter("user", true).map(writer -> {
      try {
        writer.write(format("Goal: '%s'%n", goal.forWrite()));
        return true;
      } catch (IOException ex) {
        ex.printStackTrace();
        return false;
      }
    }).orElse(false);
  }

  @Override
  public boolean onSolution(final JProlContext context, final Term goal,
                            final Map<String, TermVar> varValues, final int solutionCounter) {
    final String varText = varValues.values().stream()
        .map(termVar -> termVar.getText() + '=' + termVar.getValue().forWrite())
        .collect(Collectors.joining("\n"));

    context.findResourceWriter("user", true)
        .ifPresent(writer -> {
          try {
            writer.write(format("%nYES%n%s%nNext solution?:", varText));
          } catch (IOException ex) {
            ex.printStackTrace();
          }
        });

    return context.findResourceReader("user").map(reader -> {
      try {
        final DialogEditor.NonClosableReader dialogReader = (DialogEditor.NonClosableReader) reader;
        final int chr = dialogReader.findFirstNonWhitespaceChar();
        final Optional<Writer> out = context.findResourceWriter("user", true);
        if (out.isPresent()) {
          out.get().write("\n");
        }
        return chr == 'y' || chr == 'Y' || chr == ';';
      } catch (IOException ex) {
        ex.printStackTrace();
        return false;
      }
    }).orElse(false);
  }

  @Override
  public void onFail(final JProlContext context, final Term goal, final int foundSolutionCounter) {
    context.findResourceWriter("user", true).ifPresent(writer -> {
      try {
        writer.write(format("%nNO%n"));
      } catch (IOException ex) {
        ex.printStackTrace();
      }
    });
  }

  private static final class JLFRadioButtonItem extends JRadioButtonMenuItem {

    private static final long serialVersionUID = 71348723421332L;

    private final String lfClassName;

    public JLFRadioButtonItem(final String text, final String className) {
      super(text);
      this.lfClassName = className;
    }

    public String getLfClassName() {
      return this.lfClassName;
    }
  }

  protected final class LogLibrary extends AbstractJProlLibrary {

    public LogLibrary() {
      super("JProlGuiLogger");
    }

    @JProlPredicate(determined = true, signature = "msgerror/1", reference = "The predicate allows to output information marked as error at the message window.")
    public void predicateMSGERROR(final JProlChoicePoint goal, final TermStruct struct) {
      final Term term = struct.getElement(0).findNonVarOrSame();
      final String text = term.forWrite();
      LOG.log(Level.SEVERE, "msgerror/1 : {0}", text);
      messageEditor.addErrorText(text);
    }

    @JProlPredicate(determined = true, signature = "msgwarning/1", reference = "The predicate allows to output information marked as warning at the message window.")
    public void predicateMSGWARNING(final JProlChoicePoint goal, final TermStruct struct) {
      final Term term = struct.getElement(0).findNonVarOrSame();
      final String text = term.forWrite();
      LOG.log(Level.WARNING, "msgwarning/1 : {0}", text);
      messageEditor.addWarningText(text);
    }

    @JProlPredicate(determined = true, signature = "msginfo/1", reference = "The predicate allows to output information marked as info at the message window.")
    public void predicateMSGINFO(final JProlChoicePoint goal, final TermStruct struct) {
      final Term term = struct.getElement(0).findNonVarOrSame();
      final String text = term.forWrite();
      LOG.log(Level.INFO, "msginfo/1 : {0}", text);
      messageEditor.addInfoText(text);
    }
  }
}
