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

import javax.swing.*;
import java.io.File;
import java.io.FileNotFoundException;
import org.fife.ui.rsyntaxtextarea.AbstractTokenMakerFactory;
import org.fife.ui.rsyntaxtextarea.TokenMakerFactory;
import com.igormaznitsa.prol.easygui.tokenizer.JProlTokenMaker;

/**
 * The main class which starts the IDE
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com
 */
public class Main {

  /**
   * The main method starts the IDE.
   *
   * @param args if the array contains as minimum singe element, the elements
   * will be used as the file name to be loaded into the started IDE instance
   */
  public static void main(final String... args) {

    ((AbstractTokenMakerFactory) TokenMakerFactory.getDefaultInstance()).putMapping("text/jprol", JProlTokenMaker.class.getName());

    File fileToLoad = null;
    if (args != null && args.length > 0) {
      try {
        fileToLoad = new File(args[0]);
        if (!fileToLoad.exists() || fileToLoad.isDirectory()) {
          throw new FileNotFoundException();
        }
      } catch (FileNotFoundException thr) {
        JOptionPane.showMessageDialog(null, String.format("Can't find file \'%s\'", args[0]), "Can't load file", JOptionPane.ERROR_MESSAGE);
        fileToLoad = null;
      }
    }

    final File initFile = fileToLoad;

    SwingUtilities.invokeLater(() -> {
      if (initFile != null) {
        new MainFrame(initFile).setVisible(true);
      } else {
        new MainFrame().setVisible(true);
      }
    });
  }
}
