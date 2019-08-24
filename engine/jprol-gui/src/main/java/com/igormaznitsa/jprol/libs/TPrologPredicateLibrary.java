/*
 * Copyright 2017 Igor Maznitsa (http://www.igormaznitsa.com).
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

import com.igormaznitsa.jprol.annotations.Predicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.easygui.MainFrame;
import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.kbase.inmemory.InMemoryKnowledgeBase;
import com.igormaznitsa.jprol.logic.ChoicePoint;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import com.igormaznitsa.jprol.utils.Utils;

import javax.swing.filechooser.FileFilter;
import java.io.CharArrayWriter;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.logging.Logger;

import static com.igormaznitsa.jprol.data.TermType.VAR;

/**
 * The class implements some predicates to increase compatibility with Borland
 * Turbo Prolog.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public final class TPrologPredicateLibrary extends AbstractJProlLibrary {

  /**
   * Inside logger, the canonical class name is used as the logger identifier.
   */
  protected static final Logger LOG = Logger.getLogger(TPrologPredicateLibrary.class.getCanonicalName());
  private static volatile File path = new File(System.getProperty("user.home"));

  public TPrologPredicateLibrary() {
    super("TPrologPredicateLib");
  }

  @Predicate(determined = true, signature = "renamefile/2", args = {"+term,+term"}, reference = "Rename file")
  public static boolean predicateRenameFile(final ChoicePoint goal, final TermStruct predicate) {
    final Term oldpath = predicate.getElement(0).findNonVarOrSame();
    final Term newname = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNonVar(oldpath);
      ProlAssertions.assertNonVar(newname);
    }

    final File file = new File(path, oldpath.getText());
    final File newfile = new File(file.getParentFile(), newname.getText());

    return file.renameTo(newfile);
  }

  @Predicate(determined = true, signature = "file_str/2", args = {"+term,?term"}, reference = "Reads string from a file and transfers it to a variable, or creates a file and writes the string into the file.")
  public boolean predicateFileStr(final ChoicePoint goal, final TermStruct predicate) {
    final Term tfile = predicate.getElement(0).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNonVar(tfile);
    }

    final File file = new File(path, tfile.getText());
    final Term str = predicate.getElement(1).findNonVarOrSame();

    boolean result = false;

    if (str.getTermType() == VAR) {
      if (file.isFile()) {
        try {
          result = str.unifyTo(Terms.newAtom(Utils.readAsUtf8(file)));
        } catch (IOException ex) {
          MainFrame.MAIN_FRAME_INSTANCE.get().addErrorText("Can't read file '" + file + "' : " + ex.getMessage());
        }
      }
    } else {
      try {
        Utils.writeAsUtf8(file, str.getText());
        result = true;
      } catch (IOException ex) {
        MainFrame.MAIN_FRAME_INSTANCE.get().addErrorText("Can't write file '" + file + "' : " + ex.getMessage());
      }
    }

    return result;
  }

  @Predicate(determined = true, signature = "deletefile/1", args = {"+term"}, reference = "Delete file for name")
  public boolean predicateDeleteFile(final ChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      ProlAssertions.assertNonVar(arg);
    }

    final String filePath = arg.getText();
    final File file = new File(path, filePath);

    return file.delete();
  }

  @Predicate(determined = true, signature = "existfile/1", args = {"+term"}, reference = "Ceck that a file exists in current directory")
  public boolean predicateExistFile(final ChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNonVar(arg);
    }

    final String filePath = arg.getText();
    final File file = new File(path, filePath);

    return file.exists();
  }

  @Predicate(determined = true, signature = "dir/3", args = {"+term,+term,?term"}, reference = "Open directory to select file")
  public boolean predicateDir(final ChoicePoint goal, final TermStruct predicate) {
    final Term tthePath = predicate.getElement(0).findNonVarOrSame();
    final Term textension = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNonVar(tthePath);
      ProlAssertions.assertNonVar(textension);
    }

    final String thePath = tthePath.getText();
    final String extension = textension.getText();
    final Term selected = predicate.getElement(2).findNonVarOrSame();

    final File choosenFile = MainFrame.MAIN_FRAME_INSTANCE.get().chooseFile(new File(thePath), new FileFilter() {
      final String ext = '.' + extension;

      @Override
      public boolean accept(File f) {
        if (f.isDirectory()) {
          return true;
        }
        final String name = f.getName();
        return name.endsWith(this.ext);
      }

      @Override
      public String getDescription() {
        return "File filter (*" + this.ext + ')';
      }
    }, "Select file", "Select");

    boolean result = false;
    if (choosenFile != null) {
      result = selected.unifyTo(Terms.newAtom(choosenFile.getName()));
    }
    return result;
  }

  @Predicate(determined = true, signature = "disk/1", args = {"?term"}, reference = "Set or get current path")
  public boolean predicateDisk(final ChoicePoint goal, final TermStruct predicate) {
    Term thePath = predicate.getElement(0).findNonVarOrSame();

    boolean result = false;
    try {
      if (thePath.getTermType() == VAR) {
        return thePath.unifyTo(Terms.newAtom(path.getCanonicalPath()));
      } else {
        final String str = thePath.getText();
        final File file = new File(str);
        if (file.isDirectory()) {
          path = file;
          result = true;
        } else {
          MainFrame.MAIN_FRAME_INSTANCE.get().addErrorText("Can't find directory '" + file.getAbsolutePath() + '\'');
        }
      }
    } catch (IOException ex) {
      throw new ProlCriticalError("Can't process disk/1", ex);
    }
    return result;
  }

  @Predicate(determined = true, signature = "save/1", args = {"+term"}, reference = "Save current data base")
  public boolean predicateSave(final ChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      ProlAssertions.assertNonVar(arg);
    }

    final String filePath = arg.getText();

    MainFrame.MAIN_FRAME_INSTANCE.get().addInfoText("Save data base as file '" + filePath + '\'');

    final InMemoryKnowledgeBase base = (InMemoryKnowledgeBase) goal.getContext().getKnowledgeBase();
    final CharArrayWriter charArray = new CharArrayWriter(8096);
    try (PrintWriter writer = new PrintWriter(charArray, true)) {
      base.printStateAsSrc(writer);
    }
    final String dbtext = charArray.toString();

    boolean result = false;
    try {
      Utils.writeAsUtf8(path, dbtext);
      result = true;
    } catch (IOException ex) {
      MainFrame.MAIN_FRAME_INSTANCE.get().addWarnText("Can't save data base as file, error : " + ex.getMessage());
    }
    return result;
  }

}
