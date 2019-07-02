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

import com.igormaznitsa.jprol.annotations.*;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.Closeable;
import java.io.File;
import java.io.InputStream;
import java.io.PrintStream;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.Locale;
import java.util.regex.Pattern;

/**
 * Misc auxiliary methods for UI operations.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public final class UiUtils {

  private UiUtils() {

  }

  public static void closeQuetly(final Closeable closeable) {
    if (closeable != null) {
      try {
        closeable.close();
      } catch (Exception ex) {
      }
    }
  }

  public static void printPredicatesForLibrary(final PrintStream out, final Class<?> libraryClass) {
    if (!AbstractJProlLibrary.class.isAssignableFrom(libraryClass)) {
      out.println(libraryClass.getCanonicalName() + " is not an AbstractLibrary class");
      return;
    }

    final Method[] methods = libraryClass.getMethods();
    out.println(libraryClass.getCanonicalName());
    out.println("===============================================");

    final ProlOperators operators = libraryClass.getAnnotation(ProlOperators.class);
    if (operators != null) {
      // there is defined operators
      final ProlOperator[] ops = operators.Operators();
      if (ops.length > 0) {
        out.println("Operators\n-----------------------");
        for (final ProlOperator oper : ops) {
          if (oper.Priority() > 0) {
            out.println(":-op(" + oper.Priority() + "," + oper.Type().getText() + ",\'" + oper.Name() + "\').");
          }
        }
        out.println("-----------------------");
      }
    }

    for (final Method method : methods) {
      final Predicate predicate = method.getAnnotation(Predicate.class);
      if (predicate != null) {
        final boolean determined = method.getAnnotation(Determined.class) != null;
        final PredicateSynonyms predicateSynonims = method.getAnnotation(PredicateSynonyms.class);
        out.print(predicate.Signature());
        if (predicateSynonims != null) {
          out.print(" {");
          final String[] signatures = predicateSynonims.Signatures();
          for (int ls = 0; ls < signatures.length; ls++) {
            if (ls > 0) {
              out.print(", ");
            }
            out.print(signatures[ls]);
          }
          out.print("}");
        }
        if (determined) {
          out.print(" [DETERMINED]");
        }
        out.println();

        final String[] templates = predicate.Template();
        for (String template : templates) {
          out.println('[' + template + ']');
        }

        final String reference = predicate.Reference();
        if (reference != null && reference.length() > 0) {
          out.println();
          out.println(reference);
        }

        out.println("---------------------\r\n");
      }
    }
  }

  public static void doInSwingThread(final Runnable runnable) {
    if (SwingUtilities.isEventDispatchThread()) {
      runnable.run();
    } else {
      SwingUtilities.invokeLater(runnable);
    }
  }

  public static Pattern makePattern(final String str) {
    if (str.isEmpty()) {
      return null;
    }

    final StringBuilder buffer = new StringBuilder(str.length() << 1);

    for (final char c : str.toCharArray()) {
      if (Character.isLetter(c) || Character.isDigit(c)) {
        buffer.append(c);
      } else {
        if (Character.isWhitespace(c)) {
          buffer.append("\\s");
        } else {
          switch (c) {
            case '*':
              buffer.append(".*");
              break;
            case '?':
              buffer.append(".");
              break;
            default: {
              final String ucode = Integer.toHexString(c).toUpperCase(Locale.ENGLISH);
              buffer.append("\\u").append("0000".substring(4 - ucode.length())).append(ucode);
            }
            break;
          }
        }
      }
    }
    return Pattern.compile(buffer.toString(), Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE);
  }

  public static boolean browseURL(final URL url) {
    boolean result = false;
    if (Desktop.isDesktopSupported()) {
      final Desktop desktop = Desktop.getDesktop();
      if (desktop.isSupported(Desktop.Action.BROWSE)) {
        try {
          desktop.browse(url.toURI());
          result = true;
        } catch (Exception ex) {
          ex.printStackTrace();
        }
      } else if (desktop.isSupported(Desktop.Action.OPEN)) {
        try {
          desktop.open(new File(url.toURI()));
          result = true;
        } catch (Exception ex) {
          ex.printStackTrace();
        }
      }
    }
    return result;
  }

  public static ImageIcon loadIcon(final String name) {
    try {
      final Image img;
      try (InputStream inStream = UiUtils.class.getClassLoader().getResourceAsStream("com/igormaznitsa/prol/easygui/icons/" + name + ".png")) {
        img = ImageIO.read(inStream);
      }
      return new ImageIcon(img);
    } catch (Exception ex) {
      ex.printStackTrace();
      return new ImageIcon(new BufferedImage(16, 16, BufferedImage.TYPE_INT_RGB));
    }
  }

  public static void assertSwingThread() {
    if (!SwingUtilities.isEventDispatchThread()) {
      throw new Error("Must e called in Swing Dispatch Event Thread");

    }
  }
}
