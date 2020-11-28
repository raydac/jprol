package com.igormaznitsa.jprol.example.life;

import javax.swing.SwingUtilities;

public class Main {

  public static void main(final String... args) {
    SwingUtilities.invokeLater(() -> {
      new MainForm().setVisible(true);
    });
  }
}
