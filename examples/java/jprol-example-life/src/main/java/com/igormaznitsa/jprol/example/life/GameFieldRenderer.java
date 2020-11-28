package com.igormaznitsa.jprol.example.life;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;

public final class GameFieldRenderer extends JComponent {

  private static final Color GROUND_COLOR = Color.GREEN;
  private static final Color CELL_COLOR = Color.BLUE;
  private static final Color GRID_COLOR = Color.BLACK;
  private final LifeField model;
  private final List<ClickCellListener> listeners = new CopyOnWriteArrayList<>();
  private final boolean[] state = new boolean[LifeField.WIDTH * LifeField.HEIGHT];
  private boolean leftMouseButtonPressed = false;

  public GameFieldRenderer(final LifeField model) {
    super();

    final Dimension size = new Dimension(5 * LifeField.WIDTH, 5 * LifeField.HEIGHT);
    this.setPreferredSize(size);
    this.setSize(size);

    this.model = Objects.requireNonNull(model);
    this.setDoubleBuffered(false);

    this.addMouseMotionListener(new MouseMotionAdapter() {
      @Override
      public void mouseDragged(MouseEvent event) {
        final Dimension size = GameFieldRenderer.this.getSize();

        if (size.width <= 0 || size.height <= 0) {
          return;
        }

        final double dx = (double) size.width / LifeField.WIDTH;
        final double dy = (double) size.height / LifeField.HEIGHT;

        final int cellX = (int) Math.round(event.getX() / dx);
        final int cellY = (int) Math.round(event.getY() / dy);

        listeners.forEach(
            e -> e.onCellDragged(GameFieldRenderer.this, cellX, cellY, leftMouseButtonPressed));
      }
    });

    this.addMouseListener(new MouseAdapter() {
      @Override
      public void mousePressed(MouseEvent e) {
        leftMouseButtonPressed = SwingUtilities.isLeftMouseButton(e);
      }

      @Override
      public void mouseClicked(final MouseEvent event) {
        final Rectangle bounds = GameFieldRenderer.this.getBounds();

        if (bounds.width <= 0 || bounds.height <= 0) {
          return;
        }

        final double dx = (double) bounds.width / LifeField.WIDTH;
        final double dy = (double) bounds.height / LifeField.HEIGHT;

        final int cellX = (int) Math.round(event.getX() / dx);
        final int cellY = (int) Math.round(event.getY() / dy);

        listeners.forEach(
            e -> e.onCellClicked(GameFieldRenderer.this, cellX, cellY,
                SwingUtilities.isLeftMouseButton(event)));
      }
    });
  }

  public LifeField getModel() {
    return this.model;
  }

  public void loadState() {
    for (int y = 0; y < LifeField.HEIGHT; y++) {
      for (int x = 0; x < LifeField.WIDTH; x++) {
        state[y * LifeField.WIDTH + x] = this.model.get(x, y);
      }
    }
  }

  @Override
  public boolean isOpaque() {
    return true;
  }

  @Override
  public boolean isFocusable() {
    return false;
  }

  public void addClickCellListener(ClickCellListener listener) {
    this.listeners.add(listener);
  }

  public void removeClickCellListener(ClickCellListener listener) {
    this.listeners.remove(listener);
  }

  @Override
  public void paint(final Graphics g) {
    final Rectangle bounds = this.getBounds();

    if (bounds.width <= 0 || bounds.height <= 0) {
      return;
    }

    final double cellWidth = Math.max(1.0d, Math.round((double) bounds.width / LifeField.WIDTH));
    final double cellHeight =
        Math.max(1.0f, Math.round((double) bounds.height / LifeField.HEIGHT));

    g.setColor(GROUND_COLOR);
    g.fillRect(0, 0, bounds.width, bounds.height);

    final int cellWidthInt = (int) Math.round(cellWidth);
    final int cellHeightInt = (int) Math.round(cellHeight);

    for (int y = 0; y < LifeField.HEIGHT; y++) {
      final int cellY = (int) Math.round(cellHeight * y);
      for (int x = 0; x < LifeField.WIDTH; x++) {
        if (this.state[x + y * LifeField.WIDTH]) {
          g.setColor(CELL_COLOR);
          g.fillRect((int) Math.round(x * cellWidth), cellY, cellWidthInt, cellHeightInt);
        }
      }
    }

    g.setColor(GRID_COLOR);
    double y = 0;
    while (y < bounds.height) {
      final int cy = (int) Math.round(y);
      g.drawLine(0, cy, bounds.width, cy);
      y += cellHeight;
    }

    double x = 0;
    while (x < bounds.width) {
      final int cx = (int) Math.round(x);
      g.drawLine(cx, 0, cx, bounds.height);
      x += cellWidth;
    }

    g.setColor(GRID_COLOR);
    g.drawRect(0, 0, bounds.width - 1, bounds.height - 1);
  }

  public interface ClickCellListener {
    void onCellClicked(GameFieldRenderer source, int x, int y, boolean set);

    void onCellDragged(GameFieldRenderer source, int x, int y, boolean set);
  }

}
