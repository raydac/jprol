package com.igormaznitsa.jprol.easygui;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import java.util.logging.Level;

public final class LogLibrary extends AbstractJProlLibrary {

  public LogLibrary() {
    super("JProlGuiLogger");
  }

  @JProlPredicate(determined = true, signature = "msgerror/1", validate = "+atom", reference = "The predicate allows to output information marked as error at the message window.")
  public void predicateMSGERROR(final JProlChoicePoint goal, final TermStruct struct) {
    final Term term = struct.getArgumentAt(0).tryGround();
    final String text = term.forWrite();

    final MainFrame mainFrame = MainFrame.MAIN_FRAME_INSTANCE.get();
    if (mainFrame != null) {
      MainFrame.LOGGER.log(Level.SEVERE, "msgerror/1 : {0}", text);
      mainFrame.messageEditor.addErrorText(text);
    }
  }

  @JProlPredicate(determined = true, signature = "msgwarning/1", validate = "+atom", reference = "The predicate allows to output information marked as warning at the message window.")
  public void predicateMSGWARNING(final JProlChoicePoint goal, final TermStruct struct) {
    final Term term = struct.getArgumentAt(0).tryGround();
    final String text = term.forWrite();

    final MainFrame mainFrame = MainFrame.MAIN_FRAME_INSTANCE.get();
    if (mainFrame != null) {
      MainFrame.LOGGER.log(Level.WARNING, "msgwarning/1 : {0}", text);
      mainFrame.messageEditor.addWarningText(text);
    }
  }

  @JProlPredicate(determined = true, signature = "msginfo/1", validate = "+atom", reference = "The predicate allows to output information marked as info at the message window.")
  public void predicateMSGINFO(final JProlChoicePoint goal, final TermStruct struct) {
    final Term term = struct.getArgumentAt(0).tryGround();
    final String text = term.forWrite();

    final MainFrame mainFrame = MainFrame.MAIN_FRAME_INSTANCE.get();
    if (mainFrame != null) {
      MainFrame.LOGGER.log(Level.INFO, "msginfo/1 : {0}", text);
      mainFrame.messageEditor.addInfoText(text);
    }
  }
}
