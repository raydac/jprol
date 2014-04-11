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

import com.igormaznitsa.prol.annotations.Determined;
import com.igormaznitsa.prol.annotations.Predicate;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.exceptions.ParserException;
import com.igormaznitsa.prol.exceptions.ProlHaltExecutionException;
import com.igormaznitsa.prol.io.ProlStreamManager;
import com.igormaznitsa.prol.libraries.ProlAbstractLibrary;
import com.igormaznitsa.prol.libraries.ProlCoreLibrary;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import com.igormaznitsa.prol.trace.TraceListener;
import com.igormaznitsa.prol.utils.Utils;
import java.awt.Dimension;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Locale;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.prefs.Preferences;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;
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

/**
 * The class implements the main frame of the Prol Pad IDE (a small UI utility
 * to edit and run prol scripts) because it is a very specialized auxiliary
 * class, it is not described very precisely
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public final class MainFrame extends javax.swing.JFrame implements ProlStreamManager, Runnable, UndoableEditListener, WindowListener, DocumentListener, HyperlinkListener, TraceListener {

  private static final long serialVersionUID = -3816861562325125649L;

  /**
   * The version of the IDE
   */
  private final String VERSION = getString(this.getClass().getPackage().getImplementationVersion(),"<Unknown>");
  /**
   * Inside logger
   */
  private static final Logger LOG = Logger.getLogger("PROL_NOTE_PAD");

  private final ThreadGroup executingScripts = new ThreadGroup("ProlExecutingScripts");
  private final String PROL_STACK_DEPTH = "prol.stack.depth";

  @Override
  public boolean onProlGoalCall(Goal goal) {
    traceEditor.addCallText(goal.getGoalTerm().forWrite());
    return true;
  }

  @Override
  public boolean onProlGoalRedo(Goal goal) {
    traceEditor.addRedoText(goal.getGoalTerm().forWrite());
    return true;
  }

  @Override
  public void onProlGoalFail(Goal goal) {
    traceEditor.addFailText(goal.getGoalTerm().forWrite());
  }

  @Override
  public void onProlGoalExit(Goal goal) {
    traceEditor.addExitText(goal.getGoalTerm().forWrite());
  }

  private static String getString(final String str, final String def){
    return str == null ? def : str;
  }
  
  /**
   * This class is a helper Prol library to allow a user to print messages at
   * the Message dialog window (and the log too) directly from its scripts
   */
  protected final class LogLibrary extends ProlAbstractLibrary {

    public LogLibrary() {
      super("ProlNotepadLog");
    }

    @Predicate(Signature = "msgerror/1", Reference = "The predicate allows to output information marked as error at the message window.")
    @Determined
    public void predicateMSGERROR(final Goal goal, final TermStruct struct) {
      final Term term = Utils.getTermFromElement(struct.getElement(0));
      final String text = term.forWrite();
      LOG.log(Level.SEVERE, "msgerror/1 : {0}", text);
      messageEditor.addErrorText(text);
    }

    @Predicate(Signature = "msgwarning/1", Reference = "The predicate allows to output information marked as warning at the message window.")
    @Determined
    public void predicateMSGWARNING(final Goal goal, final TermStruct struct) {
      final Term term = Utils.getTermFromElement(struct.getElement(0));
      final String text = term.forWrite();
      LOG.log(Level.WARNING, "msgwarning/1 : {0}", text);
      messageEditor.addWarningText(text);
    }

    @Predicate(Signature = "msginfo/1", Reference = "The predicate allows to output information marked as info at the message window.")
    @Determined
    public void predicateMSGINFO(final Goal goal, final TermStruct struct) {
      final Term term = Utils.getTermFromElement(struct.getElement(0));
      final String text = term.forWrite();
      LOG.log(Level.INFO, "msginfo/1 : {0}", text);
      messageEditor.addInfoText(text);
    }
  }
  protected final LogLibrary logLibrary;
  protected HashMap<String, LookAndFeelInfo> lfTable;
  protected volatile Thread currentExecutedScriptThread;
  protected static final String PROL_EXTENSION = ".prl";
  protected File currentOpenedFile;
  protected File lastOpenedFile;
  protected boolean startInTraceMode;
  protected boolean documentHasBeenChangedFlag;
  static final String[] PROL_LIBRARIES = new String[]{"com.igormaznitsa.prol.libraries.ProlGraphicLibrary", "com.igormaznitsa.prol.libraries.ProlStringLibrary"};
  protected volatile ProlContext lastContext;
  private static final FileFilter prolFilter = new FileFilter() {

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
  private final RecentlyOpenedFileFixedList recentFiles = new RecentlyOpenedFileFixedList(MAX_RECENT_FILES);

  /**
   * Creates new form MainFrame
   */
  public MainFrame() {
    initComponents();

    Toolkit dt = Toolkit.getDefaultToolkit();
    Dimension scr = dt.getScreenSize();
    setSize((scr.width * 10) / 12, (scr.height * 10) / 12);

    TextLineNumber.addUndoableEditListener(this);
    TextLineNumber.addDocumentListener(this);
    messageEditor.addHyperlinkListener(this);
    addWindowListener(this);
    ExecutingPanel.setVisible(false);

    logLibrary = new LogLibrary();

    try {
      setIconImage(new ImageIcon(this.getClass().getResource("/com/igormaznitsa/prol/easygui/icons/appico.png")).getImage());
    }
    catch (Exception ex) {
      LOG.throwing(this.getClass().getCanonicalName(), "<init>()", ex);
    }

    fillLAndFeelMenu();

    loadPreferences();

    newFile();

    menuItemWordWrapSources.setState(TextLineNumber.isWordWrap());
  }

  private void fillLAndFeelMenu() {
    LookAndFeelInfo plaf[] = UIManager.getInstalledLookAndFeels();
    lfTable = new HashMap<String, LookAndFeelInfo>();
    ButtonGroup lfGroup = new ButtonGroup();

    final ActionListener lfListener = new ActionListener() {

      @Override
      public void actionPerformed(ActionEvent e) {
        JRadioButtonMenuItem item = (JRadioButtonMenuItem) e.getSource();
        setSelectedLookAndFeel(item.getText());
      }
    };

    for (int i = 0, n = plaf.length; i < n; i++) {
      String lfName = plaf[i].getName();
      lfTable.put(lfName, plaf[i]);
      JRadioButtonMenuItem menuItem = new JRadioButtonMenuItem(lfName);

      menuItem.addActionListener(lfListener);

      lfGroup.add(menuItem);
      MenuLookAndFeel.add(menuItem);
    }
  }

  private void setSelectedLookAndFeel(String name) {

    if (name != null) {
      if (!lfTable.containsKey(name)) {
        name = null;
      }
    }

    if (name == null) {
      // set the first
      name = MenuLookAndFeel.getItem(0).getText();
    }

    final LookAndFeelInfo feelInfo = lfTable.get(name);
    final JFrame thisFrame = this;

    for (int li = 0; li < MenuLookAndFeel.getItemCount(); li++) {
      JRadioButtonMenuItem menuItem = (JRadioButtonMenuItem) MenuLookAndFeel.getItem(li);
      if (menuItem.getText().equals(name)) {
        if (!menuItem.isSelected()) {
          menuItem.setSelected(true);
        }
        SwingUtilities.invokeLater(new Runnable() {

          @Override
          public void run() {
            try {
              UIManager.setLookAndFeel(feelInfo.getClassName());
              SwingUtilities.updateComponentTreeUI(thisFrame);
            }
            catch (Throwable e) {
              LOG.throwing(thisFrame.getClass().getCanonicalName(), "L&F", e);
            }
          }
        });
        break;
      }
    }
  }

  public MainFrame(final File initFile) {
    this();
    loadFile(initFile, true);
  }

  /**
   * This method is called from within the constructor to initialize the form.
   * WARNING: Do NOT modify this code. The content of this method is always
   * regenerated by the Form Editor.
   */
  @SuppressWarnings("unchecked")
  // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
  private void initComponents() {

    SplitPaneMain = new javax.swing.JSplitPane();
    SplitPaneTop = new javax.swing.JSplitPane();
    TextLineNumber = new com.wordpress.tips4java.TextLineNumber();
    try {
      dialogEditor = new com.igormaznitsa.prol.easygui.DialogEditor();
    } catch (java.io.IOException e1) {
      e1.printStackTrace();
    }
    SplitPanelDown = new javax.swing.JSplitPane();
    messageEditor = new com.igormaznitsa.prol.easygui.MessageEditor();
    traceEditor = new com.igormaznitsa.prol.easygui.TraceDialog();
    MessagePanel = new javax.swing.JPanel();
    ExecutingPanel = new javax.swing.JPanel();
    TaskProgressBar = new javax.swing.JProgressBar();
    ButtonStopExecuting = new javax.swing.JButton();
    jMenuBar1 = new javax.swing.JMenuBar();
    MenuFile = new javax.swing.JMenu();
    MenuFileNew = new javax.swing.JMenuItem();
    MenuFileOpen = new javax.swing.JMenuItem();
    MenuFileSaveAs = new javax.swing.JMenuItem();
    MenuFileSave = new javax.swing.JMenuItem();
    jSeparator1 = new javax.swing.JPopupMenu.Separator();
    MenuFileRecentFiles = new javax.swing.JMenu();
    jSeparator4 = new javax.swing.JPopupMenu.Separator();
    MenuExit = new javax.swing.JMenuItem();
    MenuEdit = new javax.swing.JMenu();
    MenuUndo = new javax.swing.JMenuItem();
    MenuRedo = new javax.swing.JMenuItem();
    jSeparator2 = new javax.swing.JPopupMenu.Separator();
    MenuClearText = new javax.swing.JMenuItem();
    MenuEditCommentSelected = new javax.swing.JMenuItem();
    MenuEditUncommentSelected = new javax.swing.JMenuItem();
    jSeparator3 = new javax.swing.JPopupMenu.Separator();
    menuItemWordWrapSources = new javax.swing.JCheckBoxMenuItem();
    menuItemFullScreen = new javax.swing.JMenuItem();
    MenuEditOptions = new javax.swing.JMenuItem();
    MenuRun = new javax.swing.JMenu();
    MenuRunScript = new javax.swing.JMenuItem();
    MenuTraceScript = new javax.swing.JMenuItem();
    MenuRunStop = new javax.swing.JMenuItem();
    MenuView = new javax.swing.JMenu();
    MenuViewKnowledgeBase = new javax.swing.JMenuItem();
    MenuItemLibraryInfo = new javax.swing.JMenuItem();
    MenuLookAndFeel = new javax.swing.JMenu();
    MenuHelp = new javax.swing.JMenu();
    MenuHelpHelp = new javax.swing.JMenuItem();
    MenuAbout = new javax.swing.JMenuItem();

    setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);

    SplitPaneMain.setDividerLocation(350);
    SplitPaneMain.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
    SplitPaneMain.setResizeWeight(0.8);
    SplitPaneMain.setOneTouchExpandable(true);

    SplitPaneTop.setDividerLocation(500);
    SplitPaneTop.setResizeWeight(0.9);
    SplitPaneTop.setOneTouchExpandable(true);

    TextLineNumber.setToolTipText("The editor allows to enter and edit text of a program");
    TextLineNumber.setFont(new java.awt.Font("DejaVu Sans", 1, 13)); // NOI18N
    SplitPaneTop.setLeftComponent(TextLineNumber);

    dialogEditor.setToolTipText("The window allows to communicate with a user");
    SplitPaneTop.setRightComponent(dialogEditor);

    SplitPaneMain.setTopComponent(SplitPaneTop);

    SplitPanelDown.setDividerLocation(500);
    SplitPanelDown.setResizeWeight(0.8);
    SplitPanelDown.setOneTouchExpandable(true);

    messageEditor.setToolTipText("The window shows messages during an execution of the script");
    SplitPanelDown.setLeftComponent(messageEditor);

    traceEditor.setToolTipText("The window shows trace information if the engine is being started at the trace mode");
    SplitPanelDown.setRightComponent(traceEditor);

    SplitPaneMain.setBottomComponent(SplitPanelDown);

    getContentPane().add(SplitPaneMain, java.awt.BorderLayout.CENTER);

    MessagePanel.setBorder(javax.swing.BorderFactory.createEtchedBorder());
    MessagePanel.setLayout(new javax.swing.BoxLayout(MessagePanel, javax.swing.BoxLayout.LINE_AXIS));

    ExecutingPanel.setLayout(new javax.swing.BoxLayout(ExecutingPanel, javax.swing.BoxLayout.LINE_AXIS));

    TaskProgressBar.setMaximumSize(new java.awt.Dimension(100, 20));
    TaskProgressBar.setPreferredSize(new java.awt.Dimension(40, 20));
    ExecutingPanel.add(TaskProgressBar);

    ButtonStopExecuting.setBackground(new java.awt.Color(255, 156, 156));
    ButtonStopExecuting.setFont(new java.awt.Font("DejaVu Sans", 1, 13)); // NOI18N
    ButtonStopExecuting.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/flag_red.png"))); // NOI18N
    ButtonStopExecuting.setText("STOP");
    ButtonStopExecuting.setToolTipText("Stop current execution");
    ButtonStopExecuting.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
    ButtonStopExecuting.setMaximumSize(new java.awt.Dimension(100, 23));
    ButtonStopExecuting.setMinimumSize(new java.awt.Dimension(60, 23));
    ButtonStopExecuting.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        ButtonStopExecutingActionPerformed(evt);
      }
    });
    ExecutingPanel.add(ButtonStopExecuting);

    MessagePanel.add(ExecutingPanel);

    getContentPane().add(MessagePanel, java.awt.BorderLayout.SOUTH);

    MenuFile.setText("File");
    MenuFile.setToolTipText("File operations");

    MenuFileNew.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/page.png"))); // NOI18N
    MenuFileNew.setText("New");
    MenuFileNew.setToolTipText("Create new document");
    MenuFileNew.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        MenuFileNewActionPerformed(evt);
      }
    });
    MenuFile.add(MenuFileNew);

    MenuFileOpen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_O, java.awt.event.InputEvent.CTRL_MASK));
    MenuFileOpen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/page_edit.png"))); // NOI18N
    MenuFileOpen.setText("Open");
    MenuFileOpen.setToolTipText("Open a saved document");
    MenuFileOpen.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        MenuFileOpenActionPerformed(evt);
      }
    });
    MenuFile.add(MenuFileOpen);

    MenuFileSaveAs.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/page_save.png"))); // NOI18N
    MenuFileSaveAs.setText("Save As..");
    MenuFileSaveAs.setToolTipText("Save the current document as a file");
    MenuFileSaveAs.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        MenuFileSaveAsActionPerformed(evt);
      }
    });
    MenuFile.add(MenuFileSaveAs);

    MenuFileSave.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, java.awt.event.InputEvent.CTRL_MASK));
    MenuFileSave.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/page_go.png"))); // NOI18N
    MenuFileSave.setText("Save");
    MenuFileSave.setToolTipText("Save the current document");
    MenuFileSave.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        MenuFileSaveActionPerformed(evt);
      }
    });
    MenuFile.add(MenuFileSave);
    MenuFile.add(jSeparator1);

    MenuFileRecentFiles.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/folder.png"))); // NOI18N
    MenuFileRecentFiles.setText("Recent files...");
    MenuFileRecentFiles.setToolTipText("List of files opened early");
    MenuFileRecentFiles.addMenuListener(new javax.swing.event.MenuListener() {
      public void menuCanceled(javax.swing.event.MenuEvent evt) {
      }
      public void menuDeselected(javax.swing.event.MenuEvent evt) {
      }
      public void menuSelected(javax.swing.event.MenuEvent evt) {
        MenuFileRecentFilesMenuSelected(evt);
      }
    });
    MenuFile.add(MenuFileRecentFiles);
    MenuFile.add(jSeparator4);

    MenuExit.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F4, java.awt.event.InputEvent.ALT_MASK));
    MenuExit.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/door_in.png"))); // NOI18N
    MenuExit.setText("Exit");
    MenuExit.setToolTipText("Close the editor");
    MenuExit.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        MenuExitActionPerformed(evt);
      }
    });
    MenuFile.add(MenuExit);

    jMenuBar1.add(MenuFile);

    MenuEdit.setText("Edit");
    MenuEdit.setToolTipText("Editing operations for the current document");

    MenuUndo.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z, java.awt.event.InputEvent.CTRL_MASK));
    MenuUndo.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/book_previous.png"))); // NOI18N
    MenuUndo.setText("Undo");
    MenuUndo.setToolTipText("Undo last changes in the document");
    MenuUndo.setEnabled(false);
    MenuUndo.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        MenuUndoActionPerformed(evt);
      }
    });
    MenuEdit.add(MenuUndo);

    MenuRedo.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Y, java.awt.event.InputEvent.CTRL_MASK));
    MenuRedo.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/book_next.png"))); // NOI18N
    MenuRedo.setText("Redo");
    MenuRedo.setToolTipText("Redo canceled changes in the document");
    MenuRedo.setEnabled(false);
    MenuRedo.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        MenuRedoActionPerformed(evt);
      }
    });
    MenuEdit.add(MenuRedo);
    MenuEdit.add(jSeparator2);

    MenuClearText.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_R, java.awt.event.InputEvent.CTRL_MASK));
    MenuClearText.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/page_white.png"))); // NOI18N
    MenuClearText.setText("Clear");
    MenuClearText.setToolTipText("Just clear text in the current document");
    MenuClearText.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        MenuClearTextActionPerformed(evt);
      }
    });
    MenuEdit.add(MenuClearText);

    MenuEditCommentSelected.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_5, java.awt.event.InputEvent.CTRL_MASK));
    MenuEditCommentSelected.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/comment_add.png"))); // NOI18N
    MenuEditCommentSelected.setText("Comment selection");
    MenuEditCommentSelected.setToolTipText("Place the commenting symbol as the first one into selected lines");
    MenuEditCommentSelected.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        MenuEditCommentSelectedActionPerformed(evt);
      }
    });
    MenuEdit.add(MenuEditCommentSelected);

    MenuEditUncommentSelected.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_U, java.awt.event.InputEvent.CTRL_MASK));
    MenuEditUncommentSelected.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/comment_delete.png"))); // NOI18N
    MenuEditUncommentSelected.setText("Uncomment selection");
    MenuEditUncommentSelected.setToolTipText("Remove the first commenting symbol from selected lines");
    MenuEditUncommentSelected.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        MenuEditUncommentSelectedActionPerformed(evt);
      }
    });
    MenuEdit.add(MenuEditUncommentSelected);
    MenuEdit.add(jSeparator3);

    menuItemWordWrapSources.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W, java.awt.event.InputEvent.CTRL_MASK));
    menuItemWordWrapSources.setSelected(true);
    menuItemWordWrapSources.setText("Word wrap");
    menuItemWordWrapSources.setToolTipText("Word-wrap mode for the document editor");
    menuItemWordWrapSources.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/text_align_justify.png"))); // NOI18N
    menuItemWordWrapSources.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        menuItemWordWrapSourcesActionPerformed(evt);
      }
    });
    MenuEdit.add(menuItemWordWrapSources);

    menuItemFullScreen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F, java.awt.event.InputEvent.SHIFT_MASK | java.awt.event.InputEvent.CTRL_MASK));
    menuItemFullScreen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/zoom.png"))); // NOI18N
    menuItemFullScreen.setText("Full screen");
    menuItemFullScreen.setToolTipText("Turn on the full screen mode if it is supported by the device");
    menuItemFullScreen.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        menuItemFullScreenActionPerformed(evt);
      }
    });
    MenuEdit.add(menuItemFullScreen);

    MenuEditOptions.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/cog.png"))); // NOI18N
    MenuEditOptions.setText("Options");
    MenuEditOptions.setToolTipText("Open editor options");
    MenuEditOptions.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        MenuEditOptionsActionPerformed(evt);
      }
    });
    MenuEdit.add(MenuEditOptions);

    jMenuBar1.add(MenuEdit);

    MenuRun.setText("Run");
    MenuRun.setToolTipText("Start/Stop operations");

    MenuRunScript.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F5, 0));
    MenuRunScript.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/flag_green.png"))); // NOI18N
    MenuRunScript.setText("Start");
    MenuRunScript.setToolTipText("Execute the current document");
    MenuRunScript.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        MenuRunScriptActionPerformed(evt);
      }
    });
    MenuRun.add(MenuRunScript);

    MenuTraceScript.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/flag_blue.png"))); // NOI18N
    MenuTraceScript.setText("Trace");
    MenuTraceScript.setToolTipText("Execute the current document with tracing");
    MenuTraceScript.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        MenuTraceScriptActionPerformed(evt);
      }
    });
    MenuRun.add(MenuTraceScript);

    MenuRunStop.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/flag_red.png"))); // NOI18N
    MenuRunStop.setText("Stop");
    MenuRunStop.setToolTipText("Stop the current execution");
    MenuRunStop.setEnabled(false);
    MenuRunStop.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        MenuRunStopActionPerformed(evt);
      }
    });
    MenuRun.add(MenuRunStop);

    jMenuBar1.add(MenuRun);

    MenuView.setText("View");
    MenuView.setToolTipText("Auxiliary operations");

    MenuViewKnowledgeBase.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F12, 0));
    MenuViewKnowledgeBase.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/eye.png"))); // NOI18N
    MenuViewKnowledgeBase.setText("See the knowledge base");
    MenuViewKnowledgeBase.setToolTipText("Take and show the snapshot of the current knowledge base saved in the memory");
    MenuViewKnowledgeBase.setEnabled(false);
    MenuViewKnowledgeBase.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        MenuViewKnowledgeBaseActionPerformed(evt);
      }
    });
    MenuView.add(MenuViewKnowledgeBase);

    MenuItemLibraryInfo.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F1, 0));
    MenuItemLibraryInfo.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/table.png"))); // NOI18N
    MenuItemLibraryInfo.setText("Library info");
    MenuItemLibraryInfo.setToolTipText("Show all predicates found in embedded libraries");
    MenuItemLibraryInfo.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        MenuItemLibraryInfoActionPerformed(evt);
      }
    });
    MenuView.add(MenuItemLibraryInfo);

    jMenuBar1.add(MenuView);

    MenuLookAndFeel.setText("Look&Feel");
    MenuLookAndFeel.setToolTipText("List of accessile L&F styles");
    jMenuBar1.add(MenuLookAndFeel);

    MenuHelp.setText("Help");
    MenuHelp.setToolTipText("Misc info");

    MenuHelpHelp.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/information.png"))); // NOI18N
    MenuHelpHelp.setText("Help");
    MenuHelpHelp.setToolTipText("Show information about usage of the utility");
    MenuHelpHelp.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        MenuHelpHelpActionPerformed(evt);
      }
    });
    MenuHelp.add(MenuHelpHelp);

    MenuAbout.setIcon(new javax.swing.ImageIcon(getClass().getResource("/com/igormaznitsa/prol/easygui/icons/emoticon_smile.png"))); // NOI18N
    MenuAbout.setText("About");
    MenuAbout.setToolTipText("Show the information about the application and license");
    MenuAbout.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        MenuAboutActionPerformed(evt);
      }
    });
    MenuHelp.add(MenuAbout);

    jMenuBar1.add(MenuHelp);

    setJMenuBar(jMenuBar1);

    pack();
  }// </editor-fold>//GEN-END:initComponents

    private void MenuRunScriptActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuRunScriptActionPerformed
      if (currentExecutedScriptThread != null && currentExecutedScriptThread.isAlive()) {
        JOptionPane.showMessageDialog(this, "Script is already executing.", "Can't start", JOptionPane.WARNING_MESSAGE);
      }
      else {
        startInTraceMode = false;

        clearTextAtAllWindowsExcludeSource();

        dialogEditor.initBeforeSession();

        final long stackSize = extractStackDepth();
        System.out.println("Execute script with the stack depth " + stackSize + " bytes");

        currentExecutedScriptThread = new Thread(executingScripts, this, "ProlScriptExecutingThread", stackSize);
        currentExecutedScriptThread.setDaemon(false);

        SwingUtilities.invokeLater(new Runnable() {

          @Override
          public void run() {
            showTaskControlPanel();
            currentExecutedScriptThread.start();
          }
        });
      }
    }//GEN-LAST:event_MenuRunScriptActionPerformed

    private void MenuUndoActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuUndoActionPerformed
      try {
        TextLineNumber.getUndoManager().undo();
      }
      catch (CannotUndoException ex) {
      }
      UndoManager undo = TextLineNumber.getUndoManager();
      MenuUndo.setEnabled(undo.canUndo());
      MenuRedo.setEnabled(undo.canRedo());

    }//GEN-LAST:event_MenuUndoActionPerformed

    private void MenuRedoActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuRedoActionPerformed
      try {
        TextLineNumber.getUndoManager().redo();
      }
      catch (CannotRedoException ex) {
      }
      UndoManager undo = TextLineNumber.getUndoManager();
      MenuUndo.setEnabled(undo.canUndo());
      MenuRedo.setEnabled(undo.canRedo());

    }//GEN-LAST:event_MenuRedoActionPerformed

    private void MenuExitActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuExitActionPerformed
      windowClosing(null);
    }//GEN-LAST:event_MenuExitActionPerformed

    private void MenuClearTextActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuClearTextActionPerformed
      if (TextLineNumber.getEditor().getDocument().getLength() > 10) {
        if (JOptionPane.showConfirmDialog(this, "Do you really want to clear the text?", "Confirmation", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
          TextLineNumber.getUndoManager().discardAllEdits();
          TextLineNumber.clearText();
        }
      }
    }//GEN-LAST:event_MenuClearTextActionPerformed

    private void MenuFileOpenActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuFileOpenActionPerformed
      loadFile(lastOpenedFile, false);
    }//GEN-LAST:event_MenuFileOpenActionPerformed

    private void MenuFileSaveActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuFileSaveActionPerformed
      saveFile(false);
    }//GEN-LAST:event_MenuFileSaveActionPerformed

    private void MenuFileSaveAsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuFileSaveAsActionPerformed
      saveFile(true);
    }//GEN-LAST:event_MenuFileSaveAsActionPerformed

    private void ButtonStopExecutingActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_ButtonStopExecutingActionPerformed
      final Thread thr = currentExecutedScriptThread;

      SwingUtilities.invokeLater(new Runnable() {

        @Override
        public void run() {
          if (thr != null) {
            try {
              thr.interrupt();
              dialogEditor.cancelRead();
              thr.join();
            }
            catch (Throwable tr) {
              tr.printStackTrace();
            }
            finally {
              hideTaskControlPanel();
            }
            messageEditor.addWarningText("The script execution has been canceled.");
          }
        }
      });

    }//GEN-LAST:event_ButtonStopExecutingActionPerformed

    private void MenuAboutActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuAboutActionPerformed
      JOptionPane.showMessageDialog(this, "The Prol Notepad utility\r\n==================================\r\nVersion: " + VERSION + "\r\nAuthor: Igor Maznitsa (http://www.igormaznitsa.com)\r\n\r\n(C)2010-2014 Igor A. Maznitsa. Apache 2.0 License\r\nIcons from the free icon set http://www.famfamfam.com/lab/icons/silk/", "About", JOptionPane.INFORMATION_MESSAGE, new ImageIcon(this.getIconImage()));
    }//GEN-LAST:event_MenuAboutActionPerformed

    private void MenuViewKnowledgeBaseActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuViewKnowledgeBaseActionPerformed
      if (lastContext == null) {
        return;
      }
      KnowledgeBaseSnapshotViewDialog dialog = new KnowledgeBaseSnapshotViewDialog(this, lastContext);
      dialog.setVisible(true);
    }//GEN-LAST:event_MenuViewKnowledgeBaseActionPerformed

    private void MenuRunStopActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuRunStopActionPerformed
      ButtonStopExecutingActionPerformed(evt);
    }//GEN-LAST:event_MenuRunStopActionPerformed

    private void MenuHelpHelpActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuHelpHelpActionPerformed
      new HelpDialog(this);
    }//GEN-LAST:event_MenuHelpHelpActionPerformed

    private void MenuEditOptionsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuEditOptionsActionPerformed
      OptionsDialog dialog = new OptionsDialog(this, new TreeModel[]{TextLineNumber, dialogEditor, messageEditor, traceEditor});
      dialog.setVisible(true);
    }//GEN-LAST:event_MenuEditOptionsActionPerformed

    private void MenuItemLibraryInfoActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuItemLibraryInfoActionPerformed
      ArrayList<String> list = new ArrayList<String>(PROL_LIBRARIES.length + 2);
      list.add(ProlCoreLibrary.class.getCanonicalName());
      list.addAll(Arrays.asList(PROL_LIBRARIES));
      list.add(MainFrame.class.getCanonicalName() + "$LogLibrary");

      LibraryInfoDialog infoDialog = null;
      try {
        infoDialog = new LibraryInfoDialog(this, list.toArray(new String[list.size()]));
      }
      catch (Exception ex) {
        LOG.throwing(this.getClass().getCanonicalName(), "MenuItemLibraryInfoActionPerformed()", ex);
        messageEditor.addErrorText("Can't show library info dialog [" + ex.getMessage() + ']');
        return;
      }
      infoDialog.setVisible(true);
      infoDialog.dispose();
    }//GEN-LAST:event_MenuItemLibraryInfoActionPerformed

    private void menuItemWordWrapSourcesActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_menuItemWordWrapSourcesActionPerformed
      TextLineNumber.setWordWrap(menuItemWordWrapSources.isSelected());
    }//GEN-LAST:event_menuItemWordWrapSourcesActionPerformed

    private void MenuFileNewActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuFileNewActionPerformed
      if (currentExecutedScriptThread != null && currentExecutedScriptThread.isAlive()) {
        JOptionPane.showMessageDialog(this, "The current script is executing.", "Can't make new", JOptionPane.ERROR_MESSAGE);
      }
      else {
        if (documentHasBeenChangedFlag) {
          if (JOptionPane.showConfirmDialog(this, "The current document changed and non-saved! Do you really want to make new one?", "Warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {
            newFile();
          }
        }
        else {
          newFile();
        }
      }
    }//GEN-LAST:event_MenuFileNewActionPerformed

    private long extractStackDepth(){
      final long MINIMAL_STACK = 5 * 1024 * 1024;

      long stackSize = MINIMAL_STACK;
      final String definedProlStackDepth = System.getProperty(PROL_STACK_DEPTH);
      if (definedProlStackDepth != null) {
        int scale = 1;
        final String trimmed = definedProlStackDepth.trim().toLowerCase(Locale.ENGLISH);
        String text = trimmed;
        if (trimmed.endsWith("m")) {
          scale = 1024 * 1024;
          text = trimmed.substring(0, trimmed.length() - 1);
        }
        else if (trimmed.endsWith("k")) {
          scale = 1024;
          text = trimmed.substring(0, trimmed.length() - 1);
        }
        try {
          stackSize = Math.max(MINIMAL_STACK, Long.parseLong(text) * scale);
        }
        catch (NumberFormatException ex) {
          ex.printStackTrace();
        }
      }
      return stackSize;
    }
    
    private void MenuTraceScriptActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuTraceScriptActionPerformed
      if (currentExecutedScriptThread != null && currentExecutedScriptThread.isAlive()) {
        JOptionPane.showMessageDialog(this, "Script is already executing.", "Can't start", JOptionPane.WARNING_MESSAGE);
      }
      else {
        startInTraceMode = true;
        clearTextAtAllWindowsExcludeSource();
        dialogEditor.initBeforeSession();

        final long stackSize = extractStackDepth();
        System.out.println("Execute script with the stack depth "+stackSize+" bytes");
        
        currentExecutedScriptThread = new Thread(executingScripts, this, "ProlScriptExecutingThread", stackSize);
        currentExecutedScriptThread.setDaemon(false);

        SwingUtilities.invokeLater(new Runnable() {

          @Override
          public void run() {
            showTaskControlPanel();
            currentExecutedScriptThread.start();
          }
        });
      }
    }//GEN-LAST:event_MenuTraceScriptActionPerformed

    private void MenuFileRecentFilesMenuSelected(javax.swing.event.MenuEvent evt) {//GEN-FIRST:event_MenuFileRecentFilesMenuSelected
      JMenu menu = (JMenu) evt.getSource();
      menu.removeAll();
      for (final String path : recentFiles.getCollection()) {
        final JMenuItem newItem = new JMenuItem(path);
        newItem.setActionCommand(path);
        newItem.addActionListener(new ActionListener() {

          @Override
          public void actionPerformed(final ActionEvent e) {
            final String text = e.getActionCommand();
            if (text != null) {
              try {
                loadFile(new File(text), true);
              }
              catch (Exception ex) {

                LOG.throwing(this.getClass().getCanonicalName(), "MenuFileRecentFilesMenuSelected()", ex);
              }
            }
          }
        });
        menu.add(newItem);
      }
    }//GEN-LAST:event_MenuFileRecentFilesMenuSelected

    private void MenuEditCommentSelectedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuEditCommentSelectedActionPerformed
      // TODO add your handling code here:
      if (TextLineNumber.commentSelectedLines()) {
        documentChanged();
      }
    }//GEN-LAST:event_MenuEditCommentSelectedActionPerformed

    private void MenuEditUncommentSelectedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuEditUncommentSelectedActionPerformed
      // TODO add your handling code here:
      if (TextLineNumber.uncommentSelectedLines()) {
        documentChanged();
      }
    }//GEN-LAST:event_MenuEditUncommentSelectedActionPerformed

  private void menuItemFullScreenActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_menuItemFullScreenActionPerformed
    final GraphicsDevice gd = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
    if (gd != null && gd.isFullScreenSupported()) {
      if (gd.getFullScreenWindow() == null) {
        gd.setFullScreenWindow(this);
      }
      else {
        gd.setFullScreenWindow(null);
      }
    }
  }//GEN-LAST:event_menuItemFullScreenActionPerformed

  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JButton ButtonStopExecuting;
  private javax.swing.JPanel ExecutingPanel;
  private javax.swing.JMenuItem MenuAbout;
  private javax.swing.JMenuItem MenuClearText;
  private javax.swing.JMenu MenuEdit;
  private javax.swing.JMenuItem MenuEditCommentSelected;
  private javax.swing.JMenuItem MenuEditOptions;
  private javax.swing.JMenuItem MenuEditUncommentSelected;
  private javax.swing.JMenuItem MenuExit;
  private javax.swing.JMenu MenuFile;
  private javax.swing.JMenuItem MenuFileNew;
  private javax.swing.JMenuItem MenuFileOpen;
  private javax.swing.JMenu MenuFileRecentFiles;
  private javax.swing.JMenuItem MenuFileSave;
  private javax.swing.JMenuItem MenuFileSaveAs;
  private javax.swing.JMenu MenuHelp;
  private javax.swing.JMenuItem MenuHelpHelp;
  private javax.swing.JMenuItem MenuItemLibraryInfo;
  private javax.swing.JMenu MenuLookAndFeel;
  private javax.swing.JMenuItem MenuRedo;
  private javax.swing.JMenu MenuRun;
  private javax.swing.JMenuItem MenuRunScript;
  private javax.swing.JMenuItem MenuRunStop;
  private javax.swing.JMenuItem MenuTraceScript;
  private javax.swing.JMenuItem MenuUndo;
  private javax.swing.JMenu MenuView;
  private javax.swing.JMenuItem MenuViewKnowledgeBase;
  private javax.swing.JPanel MessagePanel;
  private javax.swing.JSplitPane SplitPaneMain;
  private javax.swing.JSplitPane SplitPaneTop;
  private javax.swing.JSplitPane SplitPanelDown;
  private javax.swing.JProgressBar TaskProgressBar;
  private com.wordpress.tips4java.TextLineNumber TextLineNumber;
  private com.igormaznitsa.prol.easygui.DialogEditor dialogEditor;
  private javax.swing.JMenuBar jMenuBar1;
  private javax.swing.JPopupMenu.Separator jSeparator1;
  private javax.swing.JPopupMenu.Separator jSeparator2;
  private javax.swing.JPopupMenu.Separator jSeparator3;
  private javax.swing.JPopupMenu.Separator jSeparator4;
  private javax.swing.JMenuItem menuItemFullScreen;
  private javax.swing.JCheckBoxMenuItem menuItemWordWrapSources;
  private com.igormaznitsa.prol.easygui.MessageEditor messageEditor;
  private com.igormaznitsa.prol.easygui.TraceDialog traceEditor;
  // End of variables declaration//GEN-END:variables

  private void setLastContext(ProlContext context) {
    lastContext = context;
    MenuViewKnowledgeBase.setEnabled(lastContext != null);
  }

  @Override
  public Reader getReaderForResource(String resourceName) throws IOException {
    boolean successful = false;
    boolean notTraceable = false;
    try {
      if (resourceName.equals("user")) {
        successful = true;
        notTraceable = true;
        return dialogEditor.getInputReader();
      }
      else {
        final FileReader reader = new FileReader(resourceName);
        successful = true;
        notTraceable = false;
        return reader;
      }
    }
    finally {
      if (!notTraceable) {
        if (successful) {
          messageEditor.addInfoText("The reader for \'" + resourceName + "\' has been opened.");
        }
        else {
          messageEditor.addWarningText("The reader for \'" + resourceName + "\' can't be opened.");
        }
      }
    }
  }

  @Override
  public Writer getWriterForResource(String resourceName, boolean append) throws IOException {
    boolean successful = false;
    boolean notTraceable = false;

    try {
      if (resourceName.equals("user")) {
        successful = true;
        notTraceable = true;
        return dialogEditor.getOutputWriter();
      }
      else {
        final Writer writer = new FileWriter(resourceName);
        successful = true;
        notTraceable = false;

        return writer;
      }
    }
    finally {
      if (!notTraceable) {
        if (successful) {
          messageEditor.addInfoText("The writer for \'" + resourceName + "\' has been opened.");
        }
        else {
          messageEditor.addWarningText("The writer for \'" + resourceName + "\' can't be opened.");
        }
      }
    }
  }

  @Override
  public void run() {
    ProlContext context = null;
    ProlConsult consult = null;
    boolean successfully = false;
    boolean canceled = false;
    ProlHaltExecutionException halted = null;
    ParserException parserException = null;

    long startTime = 0;

    try {
      dialogEditor.setEnabled(true);
      dialogEditor.requestFocus();

      messageEditor.addInfoText("Creating the context...");

      try {
        context = new ProlContext("ProlScript", this);
        if (startInTraceMode) {
          context.setDefaultTraceListener(this);
        }

        for (final String str : PROL_LIBRARIES) {
          final ProlAbstractLibrary lib = (ProlAbstractLibrary) Class.forName(str).newInstance();

          context.addLibrary(lib);
          messageEditor.addInfoText("The library \'" + lib.getLibraryUID() + "\' has been added...");
        }

        context.addLibrary(logLibrary);
        messageEditor.addInfoText("The library \'" + logLibrary.getLibraryUID() + "\' has been added...");

        setLastContext(context);
      }
      catch (Throwable ex) {
        LOG.log(Level.WARNING, "ExecutionThread.run()", ex);
        messageEditor.addErrorText("Can't create context because there is an exception [" + ex.getMessage() + ']');
        return;
      }

      messageEditor.addInfoText("Consult with the script... ");
      consult = new ProlConsult(TextLineNumber.getText(), context);

      startTime = System.currentTimeMillis();

      try {
        consult.consult();
        // wait for async threads
        context.getContextExecutorService().shutdown();
        context.getContextExecutorService().awaitTermination(60, TimeUnit.SECONDS);
      }
      catch (ParserException ex) {
        LOG.log(Level.WARNING, "ExecutionThread.run()", ex);
        parserException = ex;

        final Throwable cause = ex.getCause();

        if (cause instanceof StackOverflowError) {
          messageEditor.addErrorText("Stack overflow detected!");
          JOptionPane.showMessageDialog(this, "Stack overflow detected...", "Error", JOptionPane.ERROR_MESSAGE);
          return;
        }
        else if (cause instanceof OutOfMemoryError) {
          messageEditor.addErrorText("Out of memory error detected!");
          JOptionPane.showMessageDialog(this, "Out of memory error detected...", "Error", JOptionPane.ERROR_MESSAGE);
          return;
        }

        if (cause instanceof ProlHaltExecutionException) {
          halted = (ProlHaltExecutionException) ex.getCause();
        }
        if (cause instanceof InterruptedException) {
          canceled = true;
        }
        else {
          messageEditor.addText("Parser exception [" + ex.getMessage() + ']', MessageEditor.TYPE_ERROR, "source://" + ex.getLine() + ';' + ex.getPos(), "line " + ex.getLine() + ":" + ex.getPos());
          return;
        }
      }
      catch (ThreadDeath death) {
        canceled = true;
      }
      catch (Throwable ex) {
        LOG.log(Level.WARNING, "ExecutionThread.run()", ex);

        if (ex instanceof ProlHaltExecutionException || ex.getCause() instanceof ProlHaltExecutionException) {
          if (ex instanceof ProlHaltExecutionException) {
            halted = (ProlHaltExecutionException) ex;
          }
          else {
            halted = (ProlHaltExecutionException) ex.getCause();
          }
        }
        else {
          messageEditor.addErrorText("Can't parse script because there is an exception [" + ex.getMessage() + ']');
          return;
        }
      }

      successfully = true;

    }
    finally {
      try {
        messageEditor.addInfoText("Total execution time " + ((System.currentTimeMillis() - startTime) / 1000f) + " sec.");

        if (halted == null) {
          if (!canceled) {
            if (successfully) {
              messageEditor.addInfoText("The script has been executed successfully.");
            }
            else {
              messageEditor.addErrorText("The script has been executed with errors or not-executed at all.");
            }
          }
        }
        else {
          messageEditor.addText("Script has been halted [" + halted.getMessage() + ']', MessageEditor.TYPE_WARNING, parserException != null ? ("source://" + parserException.getLine() + ';' + parserException.getPos()) : null, parserException != null ? ("line " + parserException.getLine() + ":" + parserException.getPos()) : null);
        }
        dialogEditor.setEnabled(false);
        currentExecutedScriptThread = null;
      }
      finally {
        if (context != null) {
          try {
            context.halt();
          }
          catch (IllegalStateException ex) {
          }
        }
        hideTaskControlPanel();
      }
    }
  }

  @Override
  public void undoableEditHappened(final UndoableEditEvent e) {
    final UndoManager undo = TextLineNumber.getUndoManager();
    undo.addEdit(e.getEdit());
    MenuUndo.setEnabled(undo.canUndo());
    MenuRedo.setEnabled(undo.canRedo());

  }

  @Override
  public void windowOpened(WindowEvent e) {
  }

  @Override
  public void windowClosing(WindowEvent e) {
    if (documentHasBeenChangedFlag) {
      if (JOptionPane.showConfirmDialog(this, "The document has been changed and non-saved. Do you really want to exit?", "Confirmation", JOptionPane.YES_NO_OPTION) == JOptionPane.NO_OPTION) {
        return;
      }
    }

    if (currentExecutedScriptThread != null) {
      if (JOptionPane.showConfirmDialog(this, "The script is being executed. Do you really want to exit?", "Confirmation", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {

        dialogEditor.cancelRead();
        dialogEditor.close();

        final Thread thrd = currentExecutedScriptThread;

        try {
          thrd.interrupt();
          thrd.join();
        }
        catch (Throwable thr) {
        }
      }
      else {
        return;
      }
    }
    else {
      dialogEditor.cancelRead();
      dialogEditor.close();
    }

    savePreferences();

    try {
      this.dispose();
    }
    catch (Exception ex) {
    }
    finally {
      System.exit(0);
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
    if (currentExecutedScriptThread != null) {
      dialogEditor.requestFocus();
    }
    else {
      TextLineNumber.requestFocus();
    }

  }

  @Override
  public void windowDeactivated(WindowEvent e) {
  }

  @Override
  public void insertUpdate(DocumentEvent e) {
    documentChanged();
  }

  @Override
  public void removeUpdate(DocumentEvent e) {
    documentChanged();
  }

  @Override
  public void changedUpdate(final DocumentEvent e) {
    documentChanged();
  }

  private void documentChanged() {
    if (!documentHasBeenChangedFlag) {
      documentHasBeenChangedFlag = true;
      if (currentOpenedFile != null) {
        MenuFileSave.setEnabled(true);
      }
      setTitle("*" + getTitle());
    }
  }

  private void setTextToDocument(final String text) {
    TextLineNumber.clearText();
    TextLineNumber.getEditor().setText(text);

    if (currentOpenedFile != null) {
      MenuFileSave.setEnabled(true);
    }
    else {
      MenuFileSave.setEnabled(false);
    }

    TextLineNumber.getUndoManager().discardAllEdits();
    MenuUndo.setEnabled(false);
    MenuRedo.setEnabled(false);

    documentHasBeenChangedFlag = false;
  }

  private void saveFile(final boolean saveAs) {
    File file = currentOpenedFile;
    if (saveAs || currentOpenedFile == null) {
      JFileChooser fileChooser = new JFileChooser(file);
      fileChooser.addChoosableFileFilter(prolFilter);
      fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
      fileChooser.setDragEnabled(false);
      fileChooser.setMultiSelectionEnabled(false);

      if (fileChooser.showSaveDialog(this) == JFileChooser.APPROVE_OPTION) {
        file = fileChooser.getSelectedFile();

        if (!file.exists() && fileChooser.getFileFilter().equals(prolFilter)) {
          // ake auto extension
          if (!file.getName().toLowerCase().endsWith(PROL_EXTENSION)) {
            file = new File(file.getAbsolutePath() + PROL_EXTENSION);
          }
        }

        if (saveAs || !file.equals(currentOpenedFile)) {
          if (file.exists()) {
            if (JOptionPane.showConfirmDialog(this, "File \'" + file.getAbsolutePath() + "\' exists, do you really want to overwrite it?", "File exists", JOptionPane.YES_NO_OPTION) == JOptionPane.NO_OPTION) {
              return;
            }
          }
        }
      }
      else {
        return;
      }
    }

    FileWriter writer = null;

    try {
      String text = TextLineNumber.getEditor().getText();
      writer = new FileWriter(file, false);
      writer.write(text);
      writer.flush();

      recentFiles.put(file.getAbsolutePath());
    }
    catch (Throwable thr) {
      LOG.throwing(this.getClass().getCanonicalName(), "saveFile()", thr);
      JOptionPane.showMessageDialog(this, "Can't save file because \'" + (thr.getMessage() == null ? thr.getClass().getCanonicalName() : thr.getLocalizedMessage()), "Can't save file", JOptionPane.ERROR_MESSAGE);
      return;
    }
    finally {
      if (writer != null) {
        try {
          writer.close();
        }
        catch (Throwable thr) {
        }
      }
    }

    currentOpenedFile = file;
    lastOpenedFile = currentOpenedFile;
    setTitle(currentOpenedFile.getAbsolutePath());

    TextLineNumber.getUndoManager().discardAllEdits();
    MenuFileSave.setEnabled(true);
    documentHasBeenChangedFlag = false;
  }

  private void newFile() {
    // make new

    TextLineNumber.clearText();

    clearTextAtAllWindowsExcludeSource();

    currentOpenedFile = null;
    documentHasBeenChangedFlag = false;

    setTitle("The Prol Notepad utility. Version: " + VERSION);

    repaint();
  }

  private void clearTextAtAllWindowsExcludeSource() {
    traceEditor.clearText();
    dialogEditor.clearText();
    messageEditor.clearText();
  }

  private void loadFile(final File file, final boolean justLoadFile) {
    if (documentHasBeenChangedFlag) {
      if (JOptionPane.showConfirmDialog(this, "The current document changed and non-saved! Do you really want to load new one?", "Warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE) == JOptionPane.NO_OPTION) {
        return;
      }
    }

    JFileChooser fileChooser = new JFileChooser(file);
    if (!justLoadFile) {
      fileChooser.addChoosableFileFilter(prolFilter);
      fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
      fileChooser.setDragEnabled(false);
      fileChooser.setMultiSelectionEnabled(false);
    }

    if (justLoadFile || fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
      File fileToOpen = justLoadFile ? file : fileChooser.getSelectedFile();

      lastOpenedFile = fileToOpen;

      FileReader reader = null;

      try {
        reader = new FileReader(fileToOpen);

        final int possiblelength = (int) fileToOpen.length();

        final StringBuilder builder = new StringBuilder(possiblelength);

        char[] buff = new char[8096];

        while (true) {
          int readlen = reader.read(buff, 0, buff.length);
          if (readlen < 0) {
            break;
          }
          builder.append(buff, 0, readlen);
        }

        setTextToDocument(builder.toString());
        currentOpenedFile = fileToOpen;
        setTitle(currentOpenedFile.getCanonicalPath());
        this.repaint();

        recentFiles.put(fileToOpen.getAbsolutePath());

      }
      catch (Throwable thr) {
        LOG.throwing(this.getClass().getCanonicalName(), "loadFile()", thr);
        JOptionPane.showMessageDialog(this, "Can't load file " + fileToOpen.getAbsolutePath() + " [" + thr.getMessage() + "]");
        recentFiles.remove(fileToOpen.getAbsolutePath());
      }
      finally {
        if (reader != null) {
          try {
            reader.close();
          }
          catch (Throwable thr) {
          }
        }
      }

    }
  }

  private void showTaskControlPanel() {
    ExecutingPanel.setVisible(true);
    TaskProgressBar.setIndeterminate(true);
    MenuRunStop.setEnabled(true);
  }

  private void hideTaskControlPanel() {
    SwingUtilities.invokeLater(new Runnable() {

      @Override
      public void run() {
        ExecutingPanel.setVisible(false);
        TaskProgressBar.setIndeterminate(false);
        MenuRunStop.setEnabled(false);
      }
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

          TextLineNumber.setCaretPosition(line, pos + 1);
        }
        catch (Exception ex) {
          ex.printStackTrace();
        }
      }
    }
  }

  private void loadPreferences() {
    Preferences prefs = Preferences.userNodeForPackage(this.getClass());

    setSelectedLookAndFeel(prefs.get("lookandfeel", MenuLookAndFeel.getItem(0).getText()));

    int recentFileIndex = 1;
    recentFiles.clear();
    while (true) {
      final String path = prefs.get("RecentFile" + recentFileIndex, null);
      if (path == null) {
        break;
      }
      recentFiles.add(path);
      recentFileIndex++;
    }

    if (prefs.getBoolean("maximized", false)) {
      setExtendedState(JFrame.MAXIMIZED_BOTH);
      invalidate();
      doLayout();
    }
    else {
      setSize(prefs.getInt("mainwidth", 640), prefs.getInt("mainheight", 600));
      setLocation(prefs.getInt("mainx", 0), prefs.getInt("mainy", 0));
    }

    SplitPaneMain.setDividerLocation(prefs.getInt("splitpanemainpos", 400));
    SplitPaneTop.setDividerLocation(prefs.getInt("splitpanetoppos", 300));

    String lastFile = prefs.get("lastfile", "");
    if (lastFile.length() > 0) {
      lastOpenedFile = new File(lastFile);
    }
    else {
      lastOpenedFile = null;
    }

    TextLineNumber.loadPreferences(prefs);
    messageEditor.loadPreferences(prefs);
    dialogEditor.loadPreferences(prefs);
    traceEditor.loadPreferences(prefs);
  }

  private void savePreferences() {
    Preferences prefs = Preferences.userNodeForPackage(this.getClass());

    prefs.put("lookandfeel", UIManager.getLookAndFeel().getName());

    int recentFileIndex = 1;
    for (final String recentFile : recentFiles.getCollection()) {
      prefs.put("RecentFile" + recentFileIndex, recentFile);
      recentFileIndex++;
    }

    prefs.putBoolean("maximized", (getExtendedState() & JFrame.MAXIMIZED_BOTH) == JFrame.MAXIMIZED_BOTH);

    prefs.putInt("mainwidth", getWidth());
    prefs.putInt("mainheight", getHeight());

    prefs.putInt("mainx", getX());
    prefs.putInt("mainy", getY());

    prefs.putInt("splitpanemainpos", SplitPaneMain.getDividerLocation());
    prefs.putInt("splitpanetoppos", SplitPaneTop.getDividerLocation());

    prefs.put("lastfile", lastOpenedFile == null ? "" : lastOpenedFile.getAbsolutePath());

    TextLineNumber.savePreferences(prefs);
    messageEditor.savePreferences(prefs);
    dialogEditor.savePreferences(prefs);
    traceEditor.savePreferences(prefs);
  }
}
