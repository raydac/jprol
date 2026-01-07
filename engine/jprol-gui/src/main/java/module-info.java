open module igormaznitsa.jprol.gui {

  exports com.igormaznitsa.jprol.easygui;
  exports com.igormaznitsa.jprol.easygui.tokenizer;
  exports com.igormaznitsa.jprol.easygui.guilibs;

  requires jdk.localedata;
  requires java.desktop;
  requires java.logging;
  requires igormaznitsa.jprol.extra.libs;
  requires igormaznitsa.jprol.core;
  requires java.prefs;
  requires org.fife.RSyntaxTextArea;
  requires autocomplete;
}