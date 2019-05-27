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

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.URL;
import java.util.Locale;
import java.util.regex.Pattern;

/**
 * Misc auxiliary methods for UI operations.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public final class UIUtils {

  private UIUtils() {

  }

  public static void writeFileAsUTF8Str(final File file, final CharSequence seq) throws IOException {
    Writer writer = null;

    try {
      writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file, false), "UTF-8"));
      writer.write(seq.toString());
      writer.flush();
    }
    finally {
      closeQuetly(writer);
    }
  }

  public static String readFileAsUTF8Str(final File file) throws IOException {
    BufferedReader reader = null;
    try {
      reader = new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"));
      final StringBuilder buffer = new StringBuilder((int) file.length() < 0 ? 16384 : (int) file.length());
      while (!Thread.currentThread().isInterrupted()) {
        final int chr = reader.read();
        if (chr < 0) {
          break;
        }
        buffer.append((char) chr);
      }
      return buffer.toString();
    }
    finally {
      closeQuetly(reader);
    }
  }

  public static void closeQuetly(final Closeable closeable) {
    if (closeable != null) {
      try {
        closeable.close();
      }
      catch (Exception ex) {
      }
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
        }
        catch (Exception ex) {
          ex.printStackTrace();
        }
      } else if (desktop.isSupported(Desktop.Action.OPEN)) {
        try {
          desktop.open(new File(url.toURI()));
          result = true;
        }
        catch (Exception ex) {
          ex.printStackTrace();
        }
      }
    }
    return result;
  }

  public static ImageIcon loadIcon(final String name) {
    try {
      final Image img;
      try (InputStream inStream = UIUtils.class.getClassLoader().getResourceAsStream("com/igormaznitsa/prol/easygui/icons/" + name + ".png")) {
        img = ImageIO.read(inStream);
      }
      return new ImageIcon(img);
    }
    catch (Exception ex) {
      ex.printStackTrace();
      return new ImageIcon(new BufferedImage(16, 16, BufferedImage.TYPE_INT_RGB));
    }
  }

}
