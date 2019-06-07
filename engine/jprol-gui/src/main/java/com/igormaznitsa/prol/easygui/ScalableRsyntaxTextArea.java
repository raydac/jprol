/*
 * Copyright (C) 2019 Igor Maznitsa.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301  USA
 */

package com.igormaznitsa.prol.easygui;

import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;

import java.awt.*;
import java.awt.event.MouseWheelEvent;

public final class ScalableRsyntaxTextArea extends RSyntaxTextArea {

  private static final long serialVersionUID = 72348723421L;
  private static final float SCALE_STEP = 0.5f;
  private static final float SCALE_MIN = 1.0f;
  private static final float SCALE_MAX = 10.0f;
  private float fontScale = 1.0f;
  private float fontOriginalSize;

  public ScalableRsyntaxTextArea() {
    super();
    this.fontOriginalSize = this.getFont().getSize2D();

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

    updateFontForScale();
  }

  public Font getBaseFont() {
    return this.getFont().deriveFont(this.fontOriginalSize);
  }

  private void updateFontForScale() {
    final Font newFont = this.getFont().deriveFont(this.fontScale * this.fontOriginalSize);
    if (newFont.getSize() > 0) {
      this.setFont(newFont);
    } else {
      this.setFont(this.getFont().deriveFont(1.0f));
    }
    this.invalidate();

    final Container parent = this.getParent();
    if (parent != null) {
      parent.invalidate();
      parent.repaint();
    }

    this.repaint();
  }

  public void updateConfig() {
    this.fontOriginalSize = this.getFont().getSize2D();
    updateFontForScale();
    this.revalidate();
    this.repaint();
  }

}
