package com.igormaznitsa.elife;

import com.igormaznitsa.elife.WorldModel.Cell;
import java.awt.*;
import java.awt.event.*;
import java.util.List;
import java.util.*;
import javax.swing.JComponent;

/**
 * The class implements a visual component allows to show the state of an e-life
 * world
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @version 1.00
 * @see com.igormaznitsa.elife.WorldModel
 */
public final class ElifeModelViewer extends JComponent implements MouseListener {

  private static final long serialVersionUID = -6790679443576585996L;

  private WorldModel WorldModel = null;
  private static final Color GROUND_COLOR = Color.GREEN;
  private static final Color GRID_COLOR = Color.BLACK;
  private final List<ActionListener> actionListeners = new ArrayList<ActionListener>();
  private MouseEvent lastMouseEvent;

  public ElifeModelViewer() {
    super();
    addMouseListener(this);

    setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));
  }

  public final void addActionListener(final ActionListener listener) {
    if (listener == null) {
      return;
    }
    if (actionListeners.contains(listener)) {
      return;
    }
    actionListeners.add(listener);
  }

  public final MouseEvent getLastMouseEvent() {
    return lastMouseEvent;
  }

  public final void removeActionListener(final ActionListener listener) {
    if (listener == null) {
      return;
    }
    actionListeners.remove(listener);
  }

  public final synchronized void setWorldModel(final WorldModel worldModel) {
    WorldModel p_old = this.WorldModel;
    this.WorldModel = worldModel;
    firePropertyChange(TOOL_TIP_TEXT_KEY, p_old, worldModel);
    repaint();
  }

  @Override
  protected final synchronized void paintComponent(final Graphics graphics) {
    final int viewWidth = getWidth();
    final int viewHeight = getHeight();

    if (viewWidth <= 0 || viewHeight <= 0) {
      return;
    }

    if (this.WorldModel == null) {
      graphics.setColor(Color.black);
      graphics.fillRect(WIDTH, WIDTH, viewWidth, viewHeight);
      graphics.setColor(Color.red);
      graphics.drawRect(0, 0, viewWidth, viewHeight);
      graphics.drawLine(0, 0, viewWidth, viewHeight);
      graphics.drawLine(0, viewHeight, viewWidth, 0);
    }
    else {
      final int i_cellsWidth = this.WorldModel.getColumnNumber();
      final int i_cellsHeight = this.WorldModel.getRowNumber();
      final Cell[] cellsArray = this.WorldModel.getCellsArray();

      final float deltaX = (float) viewWidth / (float) i_cellsWidth;
      final float deltaY = (float) viewHeight / (float) i_cellsHeight;

      final int cellWidth = Math.max(1, Math.round(deltaX));
      final int cellHeight = Math.max(1, Math.round(deltaY));

      graphics.setColor(GROUND_COLOR);
      graphics.fillRect(0, 0, viewWidth, viewHeight);

      final int cellLength = cellsArray.length;
      for (int li = 0; li < cellLength; li++) {
        final Cell p_cell = cellsArray[li];
        final int i_xcell = Math.round(p_cell.cellX * deltaX);
        final int i_ycell = Math.round(p_cell.cellY * deltaY);

        graphics.setColor(Color.blue);
        graphics.fillRect(i_xcell, i_ycell, cellWidth, cellHeight);
      }

      graphics.setColor(GRID_COLOR);
      for (int li = 0; li < i_cellsWidth; li++) {
        final int outX = Math.round(li * deltaX);
        graphics.drawLine(outX, 0, outX, viewHeight);
      }

      for (int li = 0; li < i_cellsHeight; li++) {
        final int outY = Math.round(li * deltaY);
        graphics.drawLine(0, outY, viewWidth, outY);
      }

      graphics.setColor(GRID_COLOR);
      graphics.drawRect(0, 0, viewWidth - 1, viewHeight - 1);

      // Toolkit.getDefaultToolkit().sync();
    }
  }

  @Override
  public void update(final Graphics g) {
    paintComponent(g);
  }

  @Override
  public void mouseClicked(MouseEvent e) {
    lastMouseEvent = e;
    for (final ActionListener listeners : actionListeners) {
      listeners.actionPerformed(new ActionEvent(this, 0, "CLICKED"));
    }
  }

  @Override
  public void mousePressed(final MouseEvent e) {
    lastMouseEvent = e;
    for (final ActionListener listeners : actionListeners) {
      listeners.actionPerformed(new ActionEvent(this, 1, "PRESSED"));
    }
  }

  @Override
  public void mouseReleased(final MouseEvent e) {
    lastMouseEvent = e;
    for (final ActionListener listeners : actionListeners) {
      listeners.actionPerformed(new ActionEvent(this, 2, "RELEASED"));
    }
  }

  public final Point coordToCell(final int coordX, final int coordY) {
    if (this.WorldModel == null) {
      return null;
    }

    final float calclatedX = (float) getWidth() / this.WorldModel.getColumnNumber();
    final float calculatedY = (float) getHeight() / this.WorldModel.getRowNumber();

    return new Point((int) (coordX / calclatedX), (int) (coordY / calculatedY));
  }

  @Override
  public void mouseEntered(MouseEvent e) {
  }

  @Override
  public void mouseExited(MouseEvent e) {
  }
}
