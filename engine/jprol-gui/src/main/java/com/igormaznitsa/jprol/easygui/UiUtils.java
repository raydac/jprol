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

import com.igormaznitsa.jprol.annotations.JProlOperator;
import com.igormaznitsa.jprol.annotations.JProlOperators;
import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import java.awt.Color;
import java.awt.Desktop;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.InputStream;
import java.io.PrintStream;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Locale;
import java.util.Objects;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

/**
 * Misc auxiliary methods for UI operations.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public final class UiUtils {

  private UiUtils() {

  }

  public static boolean isDarkTheme() {
    final Color panelBack = UIManager.getColor("Panel.background");
    if (panelBack == null) {
      return false;
    } else {
      return calculateColorBrightness(panelBack) < 150;
    }
  }

  public static int calculateColorBrightness(final Color color) {
    return (int) Math.sqrt(
        color.getRed() * color.getRed() * .241d + color.getGreen() * color.getGreen() * .691d +
            color.getBlue() * color.getBlue() * .068d);
  }

  public static void printPredicatesForLibrary(final PrintStream out, final Class<?> libraryClass) {
    if (!AbstractJProlLibrary.class.isAssignableFrom(libraryClass)) {
      out.println(libraryClass.getCanonicalName() + " is not an AbstractLibrary class");
      return;
    }

    final Method[] methods = libraryClass.getMethods();
    out.println(libraryClass.getCanonicalName());
    out.println("===============================================");

    final JProlOperators operators = libraryClass.getAnnotation(JProlOperators.class);
    final JProlOperator operator = libraryClass.getAnnotation(JProlOperator.class);
    if (operators != null || operator != null) {
      out.println("Operators\n-----------------------");
      Stream.concat(operators == null ? Stream.empty() : Arrays.stream(operators.value()),
              operator == null ? Stream.empty() : Stream.of(operator))
          .filter(op -> op.priority() > 0)
          .sorted(Comparator.comparingInt(JProlOperator::priority))
          .forEach(op -> out.printf(":-op(%d,%s,%s).%n", op.priority(), op.type(), op.name()));
    }

    for (final Method method : methods) {
      final JProlPredicate predicate = method.getAnnotation(JProlPredicate.class);
      if (predicate != null) {
        final boolean determined = predicate.determined();
        out.print(predicate.signature());
        if (predicate.synonyms().length > 0) {
          out.print(" {");
          final String[] signatures = predicate.synonyms();
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

        final String[] templates = predicate.args();
        for (String template : templates) {
          out.println('[' + template + ']');
        }

        final String reference = predicate.reference();
        if (reference != null && !reference.isEmpty()) {
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
      try (InputStream inStream = UiUtils.class.getClassLoader().getResourceAsStream("com/igormaznitsa/jprol/easygui/icons/" + name + ".png")) {
        img = ImageIO.read(Objects.requireNonNull(inStream));
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
