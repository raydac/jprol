package com.igormaznitsa.jprol.example.life;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.libs.JProlCoreLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.Optional;
import java.util.Timer;
import java.util.TimerTask;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;

@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
public class MainForm extends JFrame implements GameFieldRenderer.ClickCellListener {

  private final Timer timer = new Timer("task-timer", true);

  private final LifeField lifeField;
  private final JToggleButton startButton;
  private final JProlContext prolContext;
  private final JSlider timeSlider;
  private final GameFieldRenderer gameFieldRenderer;

  private Optional<TimerTask> currentTimerTask = Optional.empty();

  public MainForm() {
    super("Life");
    this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    this.lifeField = new LifeField();

    this.gameFieldRenderer = new GameFieldRenderer(this.lifeField);

    final JPanel mainPanel = new JPanel(new BorderLayout());
    mainPanel.add(this.gameFieldRenderer, BorderLayout.CENTER);

    this.timeSlider = new JSlider(JSlider.HORIZONTAL, 5, 100, 5);
    this.timeSlider.setSnapToTicks(true);
    this.timeSlider.setMajorTickSpacing(10);
    this.timeSlider.setMinorTickSpacing(5);
    this.timeSlider.setPaintTrack(true);
    this.timeSlider.setPaintTicks(false);

    timeSlider.addChangeListener(e -> this.startTimer());

    final JButton clearButton = new JButton("Clear");
    clearButton.addActionListener(x -> {
      this.stopRun();
      this.lifeField.clear();
      this.gameFieldRenderer.repaint();
    });

    this.startButton = new JToggleButton("Run");
    this.startButton.addActionListener(e -> {
      if (this.startButton.isSelected()) {
        startRun();
      } else {
        stopRun();
      }
      this.gameFieldRenderer.repaint();
    });

    final JPanel controlPanel = new JPanel(new GridBagLayout());
    final GridBagConstraints gbc =
        new GridBagConstraints(GridBagConstraints.RELATIVE, 0, 1, 1, 1, 1, GridBagConstraints.WEST,
            GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0);

    controlPanel.add(new JLabel(" FASTEST"), gbc);

    gbc.weightx = 10000;
    controlPanel.add(timeSlider, gbc);

    gbc.weightx = 1;
    controlPanel.add(new JLabel("SLOWEST "), gbc);


    controlPanel.add(clearButton, gbc);
    controlPanel.add(startButton, gbc);


    mainPanel.add(controlPanel, BorderLayout.SOUTH);

    this.setContentPane(mainPanel);

    this.gameFieldRenderer.addClickCellListener(this);

    this.prolContext = new JProlContext("life", new JProlCoreLibrary());
    this.prolContext.addLibrary(new LifeLibrary(this.lifeField));

    this.pack();
  }

  @SuppressWarnings({"StatementWithEmptyBody", "UnusedAssignment"})
  private void doIteration() {
    final JProlChoicePoint choicePoint = new JProlChoicePoint("life().", this.prolContext);
    Term term;
    while ((term = choicePoint.prove()) != null) {
      // do nothing
    }
    this.lifeField.blinkGeneration();
    SwingUtilities.invokeLater(() -> {
      this.gameFieldRenderer.revalidate();
      this.gameFieldRenderer.repaint();
    });
  }

  private void startTimer() {
    if (this.startButton.isSelected()) {
      this.currentTimerTask.ifPresent(TimerTask::cancel);
      final int delay = this.timeSlider.getValue() * 10;

      final TimerTask newTimerTask = new TimerTask() {
        @Override
        public void run() {
          doIteration();
        }
      };
      this.currentTimerTask = Optional.of(newTimerTask);
      this.timer.scheduleAtFixedRate(newTimerTask, 0, delay);
    }
  }

  private void startRun() {
    this.startButton.setSelected(true);
    startTimer();
  }

  private void stopRun() {
    this.startButton.setSelected(false);
    this.currentTimerTask.ifPresent(TimerTask::cancel);
    this.currentTimerTask = Optional.empty();
  }

  @Override
  public void onCellDragged(GameFieldRenderer source, int x, int y, boolean set) {
    if (x < 0 || x >= LifeField.WIDTH || y < 0 || y >= LifeField.HEIGHT) {
      return;
    }

    this.stopRun();
    final LifeField model = source.getModel();

    model.set(x, y, set);

    source.repaint();
  }

  @Override
  public void onCellClicked(GameFieldRenderer source, int x, int y, boolean set) {
    if (x < 0 || x >= LifeField.WIDTH || y < 0 || y >= LifeField.HEIGHT) {
      return;
    }

    this.stopRun();
    final LifeField model = source.getModel();

    model.set(x, y, set);

    source.repaint();
  }
}

