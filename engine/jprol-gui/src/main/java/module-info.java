open module igormaznitsa.jprol.guipad {

  exports com.igormaznitsa.jprol.easygui.guilibs;

  requires transitive jdk.localedata;
  requires transitive java.desktop;
  requires transitive java.logging;
  requires transitive java.prefs;
  requires org.fife.RSyntaxTextArea;
  requires autocomplete;

  requires transitive igormaznitsa.jprol.extra.libs;
  requires transitive igormaznitsa.jprol.core;

}