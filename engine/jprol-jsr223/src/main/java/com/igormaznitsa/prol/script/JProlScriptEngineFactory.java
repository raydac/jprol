/*
 * Copyright 2019 Igor Maznitsa.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.igormaznitsa.prol.script;

import com.igormaznitsa.prol.data.Term;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;

public class JProlScriptEngineFactory implements ScriptEngineFactory {

    @Override
    public String getEngineName() {
        return "JProl Prolog Engine";
    }

    @Override
    public String getEngineVersion() {
        return "1.1.3";
    }

    @Override
    public List<String> getExtensions() {
        return Collections.singletonList("jprol");
    }

    @Override
    public List<String> getMimeTypes() {
        return Collections.singletonList("application/jprol");
    }

    @Override
    public List<String> getNames() {
        return Collections.singletonList("jprol");
    }

    @Override
    public String getLanguageName() {
        return "Edinburgh Prolog";
    }

    @Override
    public String getLanguageVersion() {
        return "1.0.0";
    }

    @Override
    public Object getParameter(final String key) {
        switch(key){
            case ScriptEngine.ENGINE : return this.getEngineName();
            case ScriptEngine.ENGINE_VERSION : return this.getEngineVersion();
            case ScriptEngine.NAME : return this.getNames();
            case ScriptEngine.LANGUAGE : return this.getLanguageName();
            case ScriptEngine.LANGUAGE_VERSION : return this.getLanguageVersion();
            default: return null;
        }
    }

    @Override
    public String getMethodCallSyntax(String obj, String m, String... args) {
        final StringBuilder result = new StringBuilder(obj);
        result.append(":").append(m).append('(');
        String delimiter = "";
        for(final String a : args) {
            result.append(delimiter).append(a);
            delimiter = ",";
        }
        result.append(')');
        return result.toString();
    }

    @Override
    public String getOutputStatement(final String toDisplay) {
        return "?- write("+new Term(toDisplay).getSourceLikeRepresentation()+") .";
    }

    @Override
    public String getProgram(final String... statements) {
        return Arrays.stream(statements).collect(Collectors.joining(" , ", "?- ", " ."));
    }

    @Override
    public ScriptEngine getScriptEngine() {
        return new JProlScriptEngine(this);
    }
}
