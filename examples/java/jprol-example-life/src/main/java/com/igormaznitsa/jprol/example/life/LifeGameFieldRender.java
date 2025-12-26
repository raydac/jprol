package com.igormaznitsa.jprol.example.life;

import static java.lang.Math.max;
import static java.lang.Math.min;
import static java.lang.Math.round;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferInt;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;

public final class LifeGameFieldRender extends JComponent {

  private static final Color GROUND_COLOR = Color.BLACK;
  private static final Color CELL_COLOR = Color.ORANGE;
  private static final Color GRID_COLOR = Color.DARK_GRAY;
  private static final Color CURSOR_COLOR = Color.LIGHT_GRAY;
  private static final Stroke CURSOR_STROKE = new BasicStroke(3.0f);
  private final LifeGameField model;
  private final List<ClickCellListener> listeners = new CopyOnWriteArrayList<>();
  private boolean leftMouseButtonPressed = false;

  private Point cellUnderCursor = new Point(0, 0);

  private final BufferedImage buffer;
  private final int[] bufferData;

  public LifeGameFieldRender(final LifeGameField model) {
    super();

    this.setDoubleBuffered(true);

    final BufferedImage cursorImg = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB);
    final Cursor blankCursor = Toolkit.getDefaultToolkit().createCustomCursor(
        cursorImg, new Point(0, 0), "blank cursor");
    this.setCursor(blankCursor);
    this.setBackground(null);
    this.setOpaque(true);

    final Dimension size = new Dimension(5 * LifeGameField.WIDTH, 5 * LifeGameField.HEIGHT);
    this.setPreferredSize(size);
    this.setSize(size);

    this.buffer =
        new BufferedImage(LifeGameField.WIDTH, LifeGameField.HEIGHT, BufferedImage.TYPE_INT_RGB);
    this.bufferData = ((DataBufferInt) this.buffer.getRaster().getDataBuffer()).getData();

    this.model = Objects.requireNonNull(model);

    this.addMouseMotionListener(new MouseMotionAdapter() {

      @Override
      public void mouseDragged(final MouseEvent event) {
        final Dimension size = LifeGameFieldRender.this.getSize();

        if (size.width <= 0 || size.height <= 0) {
          return;
        }

        final double dx = (double) size.width / LifeGameField.WIDTH;
        final double dy = (double) size.height / LifeGameField.HEIGHT;

        final int cellX = (int) round(event.getX() / dx);
        final int cellY = (int) round(event.getY() / dy);

        listeners.forEach(
            e -> {
              LifeGameFieldRender.this.setCursorPos(cellX, cellY);
              e.onCellDragged(LifeGameFieldRender.this, cellX, cellY, leftMouseButtonPressed);
            });
      }

      @Override
      public void mouseMoved(final MouseEvent e) {
        updateCursorPos(e.getPoint());
      }
    });

    this.addMouseListener(new MouseAdapter() {

      @Override
      public void mousePressed(final MouseEvent evt) {
        leftMouseButtonPressed = SwingUtilities.isLeftMouseButton(evt);
        final Point cursorPos = updateCursorPos(evt.getPoint());
        listeners.forEach(
            e -> {
              LifeGameFieldRender.this.setCursorPos(cursorPos.x, cursorPos.y);
              e.onCellClicked(LifeGameFieldRender.this, cursorPos.x, cursorPos.y,
                  leftMouseButtonPressed);
            });
        refreshView();
      }

      @Override
      public void mouseEntered(final MouseEvent e) {
        updateCursorPos(e.getPoint());
      }

      @Override
      public void mouseMoved(final MouseEvent e) {
        updateCursorPos(e.getPoint());
      }
    });

  }

  private Point updateCursorPos(final Point point) {
    final Rectangle bounds = this.getBounds();
    if (bounds.width <= 0 || bounds.height <= 0) {
      return new Point(0, 0);
    }

    final double dx = (double) bounds.width / LifeGameField.WIDTH;
    final double dy = (double) bounds.height / LifeGameField.HEIGHT;

    final int cellX = (int) round(point.getX() / dx);
    final int cellY = (int) round(point.getY() / dy);

    this.setCursorPos(cellX, cellY);
    return this.getCursorPos();
  }

  public void setCursorPos(final int x, final int y) {
    this.cellUnderCursor =
        new Point(max(0, min(LifeGameField.WIDTH - 1, x)),
            max(0, min(LifeGameField.HEIGHT - 1, y)));
    this.refreshView();
  }

  private Point getCursorPos() {
    return this.cellUnderCursor;
  }

  public LifeGameField getModel() {
    return this.model;
  }

  @Override
  public boolean isFocusable() {
    return false;
  }

  public void addClickCellListener(ClickCellListener listener) {
    this.listeners.add(listener);
  }

  @SuppressWarnings("unused")
  public void removeClickCellListener(ClickCellListener listener) {
    this.listeners.remove(listener);
  }

  @Override
  public void paint(final Graphics g) {
    final Rectangle bounds = this.getBounds();

    if (bounds.width <= 0 || bounds.height <= 0) {
      return;
    }

    final Graphics2D gfx = (Graphics2D) g;

    gfx.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_DEFAULT);
    gfx.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
        RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR);

    final double cellWidth = max(1.0d, (double) bounds.width / LifeGameField.WIDTH);
    final double cellHeight = max(1.0f, (double) bounds.height / LifeGameField.HEIGHT);

    gfx.setColor(GROUND_COLOR);
    gfx.fillRect(0, 0, bounds.width, bounds.height);

    final int cellWidthInt = (int) round(cellWidth);
    final int cellHeightInt = (int) round(cellHeight);

    gfx.drawImage(this.buffer, 0, 0, bounds.width, bounds.height, null);

    gfx.setColor(GRID_COLOR);

    for (int y = 0; y < LifeGameField.HEIGHT; y++) {
      final int cy = (int) round(y * cellHeight);
      gfx.drawLine(0, cy, bounds.width, cy);
    }

    for (int x = 0; x < LifeGameField.WIDTH; x++) {
      final int cx = (int) round(x * cellWidth);
      gfx.drawLine(cx, 0, cx, bounds.height);
    }

    gfx.setColor(GRID_COLOR);
    gfx.drawRect(0, 0, bounds.width - 1, bounds.height - 1);

    gfx.setStroke(CURSOR_STROKE);
    gfx.setColor(CURSOR_COLOR);

    final int cursorX = (int) round(this.cellUnderCursor.x * cellWidth + cellWidth / 2);
    final int cursorY = (int) round(this.cellUnderCursor.y * cellHeight + cellHeight / 2);

    g.drawLine(cursorX, cursorY - cellHeightInt, cursorX, cursorY + cellHeightInt);
    g.drawLine(cursorX - cellWidthInt, cursorY, cursorX + cellWidthInt, cursorY);
    g.drawOval(cursorX - cellWidthInt / 2, cursorY - cellHeightInt / 2, cellWidthInt,
        cellHeightInt);
  }

  @Override
  public boolean isOpaque() {
    return true;
  }

  @Override
  public boolean isBackgroundSet() {
    return false;
  }

  public void refreshView() {
    final int ground = GROUND_COLOR.getRGB();
    final int set = CELL_COLOR.getRGB();
    for (int y = 0; y < LifeGameField.HEIGHT; y++) {
      for (int x = 0; x < LifeGameField.WIDTH; x++) {
        final int offset = x + y * LifeGameField.WIDTH;
        if (this.model.get(x, y)) {
          this.bufferData[offset] = set;
        } else {
          this.bufferData[offset] = ground;
        }
      }
    }
    this.paintImmediately(0,0, this.getWidth(), this.getHeight());
  }

  public interface ClickCellListener {
    void onCellClicked(LifeGameFieldRender source, int x, int y, boolean set);

    void onCellDragged(LifeGameFieldRender source, int x, int y, boolean set);
  }

}
