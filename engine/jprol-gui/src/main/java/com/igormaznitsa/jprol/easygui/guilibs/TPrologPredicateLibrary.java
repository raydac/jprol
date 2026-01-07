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

package com.igormaznitsa.jprol.easygui.guilibs;

import static com.igormaznitsa.jprol.data.TermType.VAR;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.easygui.MainFrame;
import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.kbase.inmemory.AbstractInMemoryKnowledgeBase;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.utils.ProlUtils;
import java.io.CharArrayWriter;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Objects;
import java.util.logging.Logger;
import javax.swing.filechooser.FileFilter;

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
  private static final Logger LOGGER = Logger.getLogger(TPrologPredicateLibrary.class.getName());
  private static volatile File path = new File(System.getProperty("user.home"));

  public TPrologPredicateLibrary() {
    super("TPrologPredicateLib");
  }

  @JProlPredicate(determined = true, signature = "renamefile/2", validate = {
      "+term,+term"}, reference = "Rename file")
  public static boolean predicateRenameFile(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term oldpath = predicate.getArgumentAt(0).tryGround();
    final Term newname = predicate.getArgumentAt(1).tryGround();

    final File file = new File(path, oldpath.getText());
    final File newfile = new File(file.getParentFile(), newname.getText());

    return file.renameTo(newfile);
  }

  @JProlPredicate(determined = true, signature = "file_str/2", validate = {
      "+term,?term"}, reference = "Reads string from a file and transfers it to a variable, or creates a file and writes the string into the file.")
  public boolean predicateFileStr(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term tfile = predicate.getArgumentAt(0).tryGround();

    final File file = new File(path, tfile.getText());
    final Term str = predicate.getArgumentAt(1).tryGround();

    boolean result = false;

    if (str.getTermType() == VAR) {
      if (file.isFile()) {
        try {
          result = str.unifyWith(Terms.newAtom(ProlUtils.readAsUtf8(file)));
        } catch (IOException ex) {
          Objects.requireNonNull(MainFrame.MAIN_FRAME_INSTANCE.get()).addErrorText("Can't read file '" + file + "' : " + ex.getMessage());
        }
      }
    } else {
      try {
        ProlUtils.writeAsUtf8(file, str.getText());
        result = true;
      } catch (IOException ex) {
        Objects.requireNonNull(MainFrame.MAIN_FRAME_INSTANCE.get()).addErrorText("Can't write file '" + file + "' : " + ex.getMessage());
      }
    }

    return result;
  }

  @JProlPredicate(determined = true, signature = "deletefile/1", validate = {
      "+term"}, reference = "Delete file for name")
  public boolean predicateDeleteFile(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).tryGround();

    final String filePath = arg.getText();
    final File file = new File(path, filePath);

    return file.delete();
  }

  @JProlPredicate(determined = true, signature = "existfile/1", validate = {
      "+term"}, reference = "Ceck that a file exists in current directory")
  public boolean predicateExistFile(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).tryGround();

    final String filePath = arg.getText();
    final File file = new File(path, filePath);

    return file.exists();
  }

  @JProlPredicate(determined = true, signature = "dir/3", validate = {
      "+term,+term,?term"}, reference = "Open directory to select file")
  public boolean predicateDir(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term tthePath = predicate.getArgumentAt(0).tryGround();
    final Term textension = predicate.getArgumentAt(1).tryGround();

    final String thePath = tthePath.getText();
    final String extension = textension.getText();
    final Term selected = predicate.getArgumentAt(2).tryGround();

    final File choosenFile = Objects.requireNonNull(MainFrame.MAIN_FRAME_INSTANCE.get()).chooseFile(new File(thePath), new FileFilter() {
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
      result = selected.unifyWith(Terms.newAtom(choosenFile.getName()));
    }
    return result;
  }

  @JProlPredicate(determined = true, signature = "disk/1", validate = {
      "?term"}, reference = "Set or get current path")
  public boolean predicateDisk(final JProlChoicePoint goal, final TermStruct predicate) {
    Term thePath = predicate.getArgumentAt(0).tryGround();

    boolean result = false;
    try {
      if (thePath.getTermType() == VAR) {
        return thePath.unifyWith(Terms.newAtom(path.getCanonicalPath()));
      } else {
        final String str = thePath.getText();
        final File file = new File(str);
        if (file.isDirectory()) {
          path = file;
          result = true;
        } else {
          Objects.requireNonNull(MainFrame.MAIN_FRAME_INSTANCE.get()).addErrorText("Can't find directory '" + file.getAbsolutePath() + '\'');
        }
      }
    } catch (IOException ex) {
      throw new ProlCriticalError("Can't process disk/1", ex);
    }
    return result;
  }

  @JProlPredicate(determined = true, signature = "save/1", validate = {
      "+term"}, reference = "Save current data base")
  public boolean predicateSave(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).tryGround();
    final String filePath = arg.getText();

    Objects.requireNonNull(MainFrame.MAIN_FRAME_INSTANCE.get()).addInfoText("Save data base as file '" + filePath + '\'');

    final AbstractInMemoryKnowledgeBase
        base = (AbstractInMemoryKnowledgeBase) goal.getContext().getKnowledgeBase();
    final CharArrayWriter charArray = new CharArrayWriter(8096);
    try (PrintWriter writer = new PrintWriter(charArray, true)) {
      base.printStateAsSource(writer);
    }
    final String dbtext = charArray.toString();

    boolean result = false;
    try {
      ProlUtils.writeAsUtf8(path, dbtext);
      result = true;
    } catch (IOException ex) {
      Objects.requireNonNull(MainFrame.MAIN_FRAME_INSTANCE.get()).addWarnText("Can't save data base as file, error : " + ex.getMessage());
    }
    return result;
  }

}
