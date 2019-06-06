package com.igormaznitsa.prol.script;

import java.util.regex.Pattern;

public final class JProlScriptUtils {
  
  private static final Pattern VALID_VAR_NAME = Pattern.compile("^(_[\\w_]+|\\p{Lu}+[\\w_]*)$");
  
  private JProlScriptUtils(){
  }

  public static boolean isValidVarName(final String name) {
    return name != null && VALID_VAR_NAME.matcher(name).matches();
  }

}
