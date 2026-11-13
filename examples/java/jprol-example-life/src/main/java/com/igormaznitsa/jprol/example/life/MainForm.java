package com.igormaznitsa.jprol.example.life;

import com.igormaznitsa.jprol.libs.JProlCoreLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.lang.reflect.InvocationTargetException;
import java.util.Optional;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;

@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
public class MainForm extends JFrame implements LifeGameFieldRender.ClickCellListener {

  private static final Logger LOGGER = Logger.getLogger(MainForm.class.getName());
  private static final int DELAY_MS_PER_SLIDER_STEP = 5;

  private final ScheduledExecutorService generationExecutor =
      Executors.newSingleThreadScheduledExecutor(task -> {
        final Thread thread = new Thread(task, "life-generation");
        thread.setDaemon(true);
        return thread;
      });

  private final LifeGameField lifeGameField;
  private final JToggleButton startButton;
  private final JProlContext prolContext;
  private final JSlider timeSlider;
  private final LifeGameFieldRender lifeGameFieldRender;

  private Optional<ScheduledFuture<?>> currentGenerationTask = Optional.empty();

  public MainForm() {
    super("Conway's Life (a JProl engine use example)");
    this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    this.lifeGameField = new LifeGameField();

    this.lifeGameFieldRender = new LifeGameFieldRender(this.lifeGameField);

    final JPanel mainPanel = new JPanel(new BorderLayout());
    mainPanel.add(this.lifeGameFieldRender, BorderLayout.CENTER);

    this.timeSlider = new JSlider(JSlider.HORIZONTAL, 5, 100, 5);
    this.timeSlider.setSnapToTicks(true);
    this.timeSlider.setMajorTickSpacing(10);
    this.timeSlider.setMinorTickSpacing(5);
    this.timeSlider.setPaintTrack(true);
    this.timeSlider.setPaintTicks(false);

    this.timeSlider.addChangeListener(e -> this.startTimer());

    final JButton clearButton = new JButton("Clear");
    clearButton.addActionListener(x -> {
      this.stopRun();
      this.lifeGameField.clear();
      this.lifeGameFieldRender.repaint();
    });

    this.startButton = new JToggleButton("Run");
    this.startButton.addActionListener(e -> {
      if (this.startButton.isSelected()) {
        this.startRun();
      } else {
        this.stopRun();
      }
      this.lifeGameFieldRender.repaint();
    });

    final JPanel controlPanel = new JPanel(new GridBagLayout());
    final GridBagConstraints gbc =
        new GridBagConstraints(GridBagConstraints.RELATIVE, 0, 1, 1, 1, 1, GridBagConstraints.WEST,
            GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0);

    controlPanel.add(new JLabel(" FASTER"), gbc);

    gbc.weightx = 10000;
    controlPanel.add(this.timeSlider, gbc);

    gbc.weightx = 1;
    controlPanel.add(new JLabel("SLOWER "), gbc);


    controlPanel.add(clearButton, gbc);
    controlPanel.add(this.startButton, gbc);


    mainPanel.add(controlPanel, BorderLayout.SOUTH);

    this.setContentPane(mainPanel);

    this.lifeGameFieldRender.addClickCellListener(this);

    this.prolContext = new JProlContext("JProl E-life example", new JProlCoreLibrary()) {
      @Override
      public boolean isVerify() {
        return false;
      }
    };
    this.prolContext.addLibrary(new LifeLibrary(this.lifeGameField));

    this.pack();
  }

  private void startTimer() {
    if (this.startButton.isSelected()) {
      this.cancelGenerationTask();
      this.currentGenerationTask = Optional.of(this.generationExecutor.scheduleWithFixedDelay(
          this::advanceGeneration, 0, (long) this.timeSlider.getValue() * DELAY_MS_PER_SLIDER_STEP,
          TimeUnit.MILLISECONDS));
    }
  }

  private void cancelGenerationTask() {
    this.currentGenerationTask.ifPresent(task -> task.cancel(false));
    this.currentGenerationTask = Optional.empty();
  }

  private void stopRun() {
    this.startButton.setSelected(false);
    this.cancelGenerationTask();
  }

  private void startRun() {
    this.startButton.setSelected(true);
    this.startTimer();
  }

  private void advanceGeneration() {
    try {
      this.lifeGameField.resetNextGeneration();
      this.computeNextGeneration();
      this.publishNextGeneration();
    } catch (RuntimeException ex) {
      LOGGER.log(Level.SEVERE, "Life generation failed, run is stopped", ex);
      SwingUtilities.invokeLater(this::stopRun);
      throw ex;
    }
  }

  @SuppressWarnings("StatementWithEmptyBody")
  private void computeNextGeneration() {
    final JProlChoicePoint lifeGoal = this.prolContext.makeChoicePoint("life().");
    while (lifeGoal.prove() != null) {
    }
  }

  private void publishNextGeneration() {
    try {
      SwingUtilities.invokeAndWait(() -> {
        if (this.startButton.isSelected()) {
          this.lifeGameField.blinkGeneration();
          this.lifeGameFieldRender.repaint();
        }
      });
    } catch (InterruptedException ex) {
      Thread.currentThread().interrupt();
    } catch (InvocationTargetException ex) {
      throw new IllegalStateException("Can't publish next generation", ex.getCause());
    }
  }

  @Override
  public void onCellClicked(final LifeGameFieldRender source, final int x, final int y,
                            final boolean set) {
    this.editCell(source, x, y, set);
  }

  @Override
  public void onCellDragged(final LifeGameFieldRender source, final int x, final int y,
                            final boolean set) {
    this.editCell(source, x, y, set);
  }

  private void editCell(final LifeGameFieldRender source, final int x, final int y,
                        final boolean set) {
    if (x < 0 || x >= LifeGameField.WIDTH || y < 0 || y >= LifeGameField.HEIGHT) {
      return;
    }

    this.stopRun();
    source.getModel().set(x, y, set);
    source.repaint();
  }
}
