package com.igormaznitsa.jprol.example.life;

import static java.lang.Math.floor;
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
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferInt;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;

public final class LifeGameFieldRender extends JComponent {

  private static final int GROUND_RGB = Color.BLACK.getRGB();
  private static final int CELL_RGB = Color.ORANGE.getRGB();
  private static final Color GRID_COLOR = Color.DARK_GRAY;
  private static final Color CURSOR_COLOR = Color.LIGHT_GRAY;
  private static final Stroke CURSOR_STROKE = new BasicStroke(3.0f);
  private static final int PIXELS_PER_CELL = 5;

  private final LifeGameField model;
  private final List<ClickCellListener> listeners = new CopyOnWriteArrayList<>();
  private final BufferedImage cellImage;
  private final int[] cellPixels;

  private boolean leftMouseButtonPressed;
  private Point cellUnderCursor = new Point(0, 0);

  public LifeGameFieldRender(final LifeGameField model) {
    super();
    this.model = Objects.requireNonNull(model);

    this.cellImage =
        new BufferedImage(LifeGameField.WIDTH, LifeGameField.HEIGHT, BufferedImage.TYPE_INT_RGB);
    this.cellPixels = ((DataBufferInt) this.cellImage.getRaster().getDataBuffer()).getData();

    this.setDoubleBuffered(true);
    this.setOpaque(true);
    this.setCursor(createBlankCursor());

    final Dimension size = new Dimension(PIXELS_PER_CELL * LifeGameField.WIDTH,
        PIXELS_PER_CELL * LifeGameField.HEIGHT);
    this.setPreferredSize(size);
    this.setSize(size);

    final MouseAdapter mouseTracker = new MouseAdapter() {
      @Override
      public void mousePressed(final MouseEvent event) {
        notifyCellClicked(event);
      }

      @Override
      public void mouseDragged(final MouseEvent event) {
        notifyCellDragged(event);
      }

      @Override
      public void mouseMoved(final MouseEvent event) {
        moveCursorTo(toCell(event.getPoint()));
      }

      @Override
      public void mouseEntered(final MouseEvent event) {
        moveCursorTo(toCell(event.getPoint()));
      }
    };
    this.addMouseListener(mouseTracker);
    this.addMouseMotionListener(mouseTracker);
  }

  private static Cursor createBlankCursor() {
    return Toolkit.getDefaultToolkit().createCustomCursor(
        new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB), new Point(0, 0), "blank cursor");
  }

  private static Point clampToField(final Point cell) {
    return new Point(max(0, min(LifeGameField.WIDTH - 1, cell.x)),
        max(0, min(LifeGameField.HEIGHT - 1, cell.y)));
  }

  private static void drawGrid(final Graphics2D gfx, final int width, final int height,
                               final double cellWidth, final double cellHeight) {
    gfx.setColor(GRID_COLOR);
    for (int y = 0; y < LifeGameField.HEIGHT; y++) {
      final int lineY = (int) round(y * cellHeight);
      gfx.drawLine(0, lineY, width, lineY);
    }
    for (int x = 0; x < LifeGameField.WIDTH; x++) {
      final int lineX = (int) round(x * cellWidth);
      gfx.drawLine(lineX, 0, lineX, height);
    }
    gfx.drawRect(0, 0, width - 1, height - 1);
  }

  private void notifyCellClicked(final MouseEvent event) {
    this.leftMouseButtonPressed = SwingUtilities.isLeftMouseButton(event);
    final Point cell = clampToField(this.toCell(event.getPoint()));
    this.moveCursorTo(cell);
    this.listeners.forEach(
        listener -> listener.onCellClicked(this, cell.x, cell.y, this.leftMouseButtonPressed));
  }

  private void notifyCellDragged(final MouseEvent event) {
    final Point cell = this.toCell(event.getPoint());
    this.moveCursorTo(cell);
    this.listeners.forEach(
        listener -> listener.onCellDragged(this, cell.x, cell.y, this.leftMouseButtonPressed));
  }

  private Point toCell(final Point mousePoint) {
    if (this.getWidth() <= 0 || this.getHeight() <= 0) {
      return this.cellUnderCursor;
    }
    return new Point(
        (int) floor(mousePoint.getX() * LifeGameField.WIDTH / this.getWidth()),
        (int) floor(mousePoint.getY() * LifeGameField.HEIGHT / this.getHeight()));
  }

  public LifeGameField getModel() {
    return this.model;
  }

  @Override
  public boolean isFocusable() {
    return false;
  }

  private void moveCursorTo(final Point cell) {
    this.cellUnderCursor = clampToField(cell);
    this.repaint();
  }

  public void addClickCellListener(final ClickCellListener listener) {
    this.listeners.add(Objects.requireNonNull(listener));
  }

  @SuppressWarnings("unused")
  public void removeClickCellListener(final ClickCellListener listener) {
    this.listeners.remove(listener);
  }

  @Override
  protected void paintComponent(final Graphics graphics) {
    final int width = this.getWidth();
    final int height = this.getHeight();
    if (width <= 0 || height <= 0) {
      return;
    }

    final double cellWidth = max(1.0d, (double) width / LifeGameField.WIDTH);
    final double cellHeight = max(1.0d, (double) height / LifeGameField.HEIGHT);

    final Graphics2D gfx = (Graphics2D) graphics.create();
    try {
      gfx.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
          RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR);
      this.drawCells(gfx, width, height);
      drawGrid(gfx, width, height, cellWidth, cellHeight);
      this.drawCursor(gfx, cellWidth, cellHeight);
    } finally {
      gfx.dispose();
    }

    // X11 queues rendering commands until the next native event; without sync a frame only shows on mouse move
    Toolkit.getDefaultToolkit().sync();
  }

  private void drawCells(final Graphics2D gfx, final int width, final int height) {
    Arrays.setAll(this.cellPixels,
        offset -> this.model.get(offset % LifeGameField.WIDTH, offset / LifeGameField.WIDTH)
            ? CELL_RGB : GROUND_RGB);
    gfx.drawImage(this.cellImage, 0, 0, width, height, null);
  }

  private void drawCursor(final Graphics2D gfx, final double cellWidth, final double cellHeight) {
    final int cellWidthInt = (int) round(cellWidth);
    final int cellHeightInt = (int) round(cellHeight);
    final int centerX = (int) round(this.cellUnderCursor.x * cellWidth + cellWidth / 2);
    final int centerY = (int) round(this.cellUnderCursor.y * cellHeight + cellHeight / 2);

    gfx.setStroke(CURSOR_STROKE);
    gfx.setColor(CURSOR_COLOR);
    gfx.drawLine(centerX, centerY - cellHeightInt, centerX, centerY + cellHeightInt);
    gfx.drawLine(centerX - cellWidthInt, centerY, centerX + cellWidthInt, centerY);
    gfx.drawOval(centerX - cellWidthInt / 2, centerY - cellHeightInt / 2, cellWidthInt,
        cellHeightInt);
  }

  public interface ClickCellListener {
    void onCellClicked(LifeGameFieldRender source, int x, int y, boolean set);

    void onCellDragged(LifeGameFieldRender source, int x, int y, boolean set);
  }

}
