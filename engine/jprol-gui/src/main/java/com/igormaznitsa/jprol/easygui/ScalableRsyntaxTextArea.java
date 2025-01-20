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

package com.igormaznitsa.jprol.easygui;

import java.awt.Component;
import java.awt.Font;
import java.awt.event.MouseWheelEvent;
import javax.swing.JScrollPane;
import javax.swing.JViewport;
import org.fife.ui.rsyntaxtextarea.RSyntaxDocument;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.Style;
import org.fife.ui.rsyntaxtextarea.SyntaxScheme;
import org.fife.ui.rtextarea.Gutter;
import org.fife.ui.rtextarea.RTextScrollPane;

public class ScalableRsyntaxTextArea extends RSyntaxTextArea {
  private static final float SCALE_STEP = 0.5f;
  private static final float SCALE_MIN = 1.0f;
  private static final float SCALE_MAX = 10.0f;
  private float fontScale = 1.0f;
  private Font baseFont;

  public ScalableRsyntaxTextArea(final String syntaxStyle) {
    super();
    final RSyntaxDocument document = new RSyntaxDocument(syntaxStyle);
    this.setDocument(document);
    this.baseFont = this.getFont();

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

    updateFontForScale();
  }

  @Override
  public void setFont(final Font font) {
    if (this.baseFont == null) {
      super.setFont(font);
    } else {
      this.updateFontForScale();
    }
  }

  public Font getBaseFont() {
    return this.baseFont;
  }

  public void setBaseFont(final Font font) {
    this.baseFont = font;
    this.fontScale = 1.0f;
    super.setFont(this.baseFont);
    updateFontForScale();
  }

  private void updateFontForScale() {
    if (this.baseFont != null) {
      float fontSize = this.fontScale * this.baseFont.getSize2D();

      if (fontSize < 1.0f) {
        fontSize = 1.0f;
      }

      SyntaxScheme scheme = this.getSyntaxScheme();
      int count = scheme.getStyleCount();
      for (int i = 0; i < count; i++) {
        Style ss = scheme.getStyle(i);
        if (ss != null) {
          Font font = ss.font;
          if (font != null) {
            ss.font = font.deriveFont(fontSize);
          }
        }
      }

      super.setFont(this.baseFont.deriveFont(fontSize));

      this.setSyntaxScheme(scheme);

      Component parent = this.getParent();
      if (parent instanceof JViewport) {
        parent = parent.getParent();
        if (parent instanceof RTextScrollPane) {
          final Gutter gutter = ((RTextScrollPane) parent).getGutter();
          gutter.setLineNumberFont(gutter.getLineNumberFont().deriveFont(fontSize));
        }

        if (parent instanceof JScrollPane) {
          parent.repaint();
        }
      }
    }
  }


}
