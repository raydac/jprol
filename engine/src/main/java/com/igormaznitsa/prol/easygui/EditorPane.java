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

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import javax.swing.JPopupMenu;
import javax.swing.JTextPane;

/**
 * The class implements the Editor pane for the IDE.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class EditorPane extends JTextPane {

  private static final long serialVersionUID = 2384993058573194751L;
  private volatile JPopupMenu popupMenu;
  private volatile EventReplacer replacer;

  private static final float SCALE_STEP = 0.5f;
  private static final float SCALE_MIN = 0.03f;
  private static final float SCALE_MAX = 10.0f;

  private float fontScale = 1.0f;
  private float fontOriginalSize;

  public EditorPane(final boolean scalable) {
    super();
    
    this.setOpaque(true);
    
    if (scalable) {
      this.addMouseWheelListener((final MouseWheelEvent e) -> {
        final int allModifiers = MouseWheelEvent.CTRL_DOWN_MASK | MouseWheelEvent.ALT_DOWN_MASK | MouseWheelEvent.META_DOWN_MASK | MouseWheelEvent.SHIFT_DOWN_MASK;
        if (!e.isConsumed() && !e.isPopupTrigger() && (e.getModifiersEx() & allModifiers) == MouseWheelEvent.CTRL_DOWN_MASK) {
          e.consume();
          this.fontScale = Math.max(SCALE_MIN, Math.min(SCALE_MAX, this.fontScale + SCALE_STEP * -e.getWheelRotation()));
          updateFontForScale();
        } else {
          this.getParent().dispatchEvent(e);
        }
      });
    }

    addMouseListener(new MouseAdapter() {

      @Override
      public void mousePressed(MouseEvent e) {
        showMenuIfPopupTrigger(e);
      }

      @Override
      public void mouseReleased(MouseEvent e) {
        showMenuIfPopupTrigger(e);
      }
    });
  }

  public Font getBaseFont(){
    return this.getFont().deriveFont(this.fontOriginalSize);
  }
  
  @Override
  public void setFont(final Font font) {
    super.setFont(font);
    this.fontScale = 1.0f;
    this.fontOriginalSize = font.getSize2D();
  }

  public void setEventReplacer(final EventReplacer replacer) {
    this.replacer = replacer;
  }

  public void setPopupMenu(final JPopupMenu menu) {
    this.popupMenu = menu;
  }

  private void showMenuIfPopupTrigger(final MouseEvent e) {
    final JPopupMenu thePopup = this.popupMenu;

    if (thePopup == null) {
      return;
    }
    if (e.isPopupTrigger()) {
      thePopup.show(this, e.getX() + 3, e.getY() + 3);
    }
  }

  private void updateFontForScale() {
    final Font newFont = this.getFont().deriveFont(this.fontScale * this.fontOriginalSize);
    if (newFont.getSize() > 0) {
      super.setFont(newFont);
    } else {
      super.setFont(this.getFont().deriveFont(1.0f));
    }
  }

  @Override
  public void paste() {
    if (replacer == null) {
      super.paste();
    } else {
      replacer.pasteEvent();
    }
  }

  public interface EventReplacer {

    void pasteEvent();
  }
}
