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

import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.InputStream;
import javax.imageio.ImageIO;
import javax.swing.*;

/**
 * Misc auxiliary methods for UI operations.
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public final class UIUtils {
  private UIUtils(){
    
  }
  
  public static ImageIcon loadIcon(final String name) {
    try {
      final InputStream inStream = UIUtils.class.getClassLoader().getResourceAsStream("com/igormaznitsa/prol/easygui/icons/" + name + ".png");
      final Image img = ImageIO.read(inStream);
      inStream.close();
      return new ImageIcon(img);
    }
    catch (Exception ex) {
      ex.printStackTrace();
      return new ImageIcon(new BufferedImage(16, 16, BufferedImage.TYPE_INT_RGB));
    }
  }

}
