import com.igormaznitsa.jprol.example.life.MainForm;
import javax.swing.SwingUtilities;

public class JProlElife {

  public static void main(final String... args) {
    SwingUtilities.invokeLater(() -> new MainForm().setVisible(true));
  }
}
