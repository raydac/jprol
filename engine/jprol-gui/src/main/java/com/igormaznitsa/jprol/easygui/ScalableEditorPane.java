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

import java.awt.Font;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import javax.swing.JPopupMenu;
import javax.swing.JTextPane;

/**
 * The class implements the Editor pane for the IDE.
 */
public class ScalableEditorPane extends JTextPane {
  private static final float SCALE_STEP = 0.5f;
  private static final float SCALE_MIN = 1.0f;
  private static final float SCALE_MAX = 10.0f;
  private volatile JPopupMenu popupMenu;
  private volatile EventReplacer replacer;
  private float fontScale = 1.0f;
  private Font baseFont;

  public ScalableEditorPane(final boolean scalable) {
    super();
    this.baseFont = this.getFont();
    this.setOpaque(true);

    if (scalable) {
      this.addMouseWheelListener((final MouseWheelEvent e) -> {
        final int allModifiers = MouseWheelEvent.CTRL_DOWN_MASK | MouseWheelEvent.ALT_DOWN_MASK |
            MouseWheelEvent.META_DOWN_MASK | MouseWheelEvent.SHIFT_DOWN_MASK;
        if (!e.isConsumed() && !e.isPopupTrigger() &&
            (e.getModifiersEx() & allModifiers) == MouseWheelEvent.CTRL_DOWN_MASK) {
          e.consume();
          this.fontScale = Math.max(SCALE_MIN,
              Math.min(SCALE_MAX, this.fontScale + SCALE_STEP * -e.getWheelRotation()));
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

  public Font getBaseFont() {
    return this.baseFont;
  }

  public void setBaseFont(final Font baseFont) {
    this.baseFont = baseFont;
    this.fontScale = 1.0f;
    this.updateFontForScale();
  }

  @Override
  public void setFont(final Font font) {
    this.updateFontForScale();
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
    if (this.baseFont != null) {
      final float newScaledSize = this.fontScale * this.baseFont.getSize2D();
      final Font scaledFont;
      if (newScaledSize > 1.0f) {
        scaledFont = this.baseFont.deriveFont(newScaledSize);
      } else {
        scaledFont = this.baseFont.deriveFont(1.0f);
      }
      super.setFont(scaledFont);
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
