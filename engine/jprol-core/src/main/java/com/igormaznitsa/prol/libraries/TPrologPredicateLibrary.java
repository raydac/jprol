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
package com.igormaznitsa.prol.libraries;

import com.igormaznitsa.prol.annotations.Determined;
import com.igormaznitsa.prol.annotations.Predicate;
import com.igormaznitsa.prol.containers.KnowledgeBase;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.exceptions.ProlCriticalError;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.utils.Utils;
import java.io.CharArrayWriter;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.logging.Logger;
import javax.swing.filechooser.FileFilter;

/**
 * The class implements some predicates to increase compatibility with Borland
 * Turbo Prolog.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public final class TPrologPredicateLibrary extends AbstractProlLibrary {

    private static volatile File path = new File(System.getProperty("user.home"));

    /**
     * Inside logger, the canonical class name is used as the logger identifier.
     */
    protected static final Logger LOG = Logger.getLogger(TPrologPredicateLibrary.class.getCanonicalName());

    @Predicate(Signature = "file_str/2", Template = {"+term,?term"}, Reference = "Reads string from a file and transfers it to a variable, or creates a file and writes the string into the file.")
    @Determined
    public boolean predicateFileStr(final Goal goal, final TermStruct predicate) {
        final File file = new File(path, Utils.getTermFromElement(predicate.getElement(0)).getText());
        final Term str = Utils.getTermFromElement(predicate.getElement(1));

        boolean result = false;

        if (str.getTermType() == Term.TYPE_VAR) {
            if (file.isFile()) {
                try {
                    result = str.Equ(new Term(Utils.readFileAsUTF8Str(file)));
                } catch (IOException ex) {
                    msgError("Can't read file '" + file + "' : " + ex.getMessage());
                }
            }
        } else {
            try {
                Utils.writeFileAsUTF8Str(file, str.getText());
                result = true;
            } catch (IOException ex) {
                msgError("Can't write file '" + file + "' : " + ex.getMessage());
            }
        }

        return result;
    }

    @Predicate(Signature = "deletefile/1", Template = {"+term"}, Reference = "Delete file for name")
    @Determined
    public boolean predicateDeleteFile(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));

        final String filePath = arg.getText();
        final File file = new File(path, filePath);

        return file.delete();
    }

    @Predicate(Signature = "existfile/1", Template = {"+term"}, Reference = "Ceck that a file exists in current directory")
    @Determined
    public boolean predicateExistFile(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));

        final String filePath = arg.getText();
        final File file = new File(path, filePath);

        return file.exists();
    }

    @Predicate(Signature = "renamefile/2", Template = {"+term,+term"}, Reference = "Rename file")
    @Determined
    public static boolean predicateRenameFile(final Goal goal, final TermStruct predicate) {
        final Term oldpath = Utils.getTermFromElement(predicate.getElement(0));
        final Term newname = Utils.getTermFromElement(predicate.getElement(1));

        final File file = new File(path, oldpath.getText());
        final File newfile = new File(file.getParentFile(), newname.getText());

        return file.renameTo(newfile);
    }

    @Predicate(Signature = "dir/3", Template = {"+term,+term,?term"}, Reference = "Open directory to select file")
    @Determined
    public boolean predicateDir(final Goal goal, final TermStruct predicate) {
        final String thePath = Utils.getTermFromElement(predicate.getElement(0)).getText();
        final String extension = Utils.getTermFromElement(predicate.getElement(1)).getText();
        final Term selected = Utils.getTermFromElement(predicate.getElement(2));

        final File choosenFile = this.getIoActionProvider().chooseFile(new File(thePath), new FileFilter() {
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
            result = selected.Equ(new Term(choosenFile.getName()));
        }
        return result;
    }

    @Predicate(Signature = "disk/1", Template = {"?term"}, Reference = "Set or get current path")
    @Determined
    public boolean predicateDisk(final Goal goal, final TermStruct predicate) {
        Term thePath = Utils.getTermFromElement(predicate.getElement(0));

        boolean result = false;
        try {
            if (thePath.getTermType() == Term.TYPE_VAR) {
                return thePath.Equ(new Term(path.getCanonicalPath()));
            } else {
                final String str = thePath.getText();
                final File file = new File(str);
                if (file.isDirectory()) {
                    path = file;
                    result = true;
                } else {
                    msgError("Can't find directory '" + file.getAbsolutePath() + '\'');
                }
            }
        } catch (IOException ex) {
            throw new ProlCriticalError("Can't process disk/1", ex);
        }
        return result;
    }

    @Predicate(Signature = "save/1", Template = {"+term"}, Reference = "Save current data base")
    @Determined
    public boolean predicateSave(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));

        final String filePath = arg.getText();

        msgInfo("Save data base as file '" + filePath + '\'');

        final KnowledgeBase base = goal.getContext().getKnowledgeBase();
        final CharArrayWriter charArray = new CharArrayWriter(8096);
        try (PrintWriter writer = new PrintWriter(charArray, true)) {
            base.write(writer);
        }
        final String dbtext = charArray.toString();

        boolean result = false;
        try {
            Utils.writeFileAsUTF8Str(path, dbtext);
            result = true;
        } catch (IOException ex) {
            msgWarn("Can't save data base as file, error : " + ex.getMessage());
        }
        return result;
    }

    public TPrologPredicateLibrary(final IoActionProvider actionProvider) {
        super("TPrologPredicateLib", actionProvider);
    }

}
