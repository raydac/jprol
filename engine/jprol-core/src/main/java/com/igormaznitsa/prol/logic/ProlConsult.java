/*
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
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

package com.igormaznitsa.prol.logic;

import com.igormaznitsa.prol.containers.KnowledgeBase;
import com.igormaznitsa.prol.data.Operator;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.data.Var;
import com.igormaznitsa.prol.exceptions.ParserException;
import com.igormaznitsa.prol.exceptions.ProlHaltExecutionException;
import com.igormaznitsa.prol.exceptions.ProlKnowledgeBaseException;
import com.igormaznitsa.prol.parser.ProlReader;
import com.igormaznitsa.prol.parser.ProlTokenizer;
import com.igormaznitsa.prol.parser.ProlTreeBuilder;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import static com.igormaznitsa.prol.data.Terms.newStruct;

public class ProlConsult {

  private static final Logger LOG = Logger.getLogger(ProlConsult.class.getCanonicalName());
  private final ProlReader reader;
  private final ProlContext context;
  private final KnowledgeBase base;

  public ProlConsult(final ProlContext context) {
    this((ProlReader) null, context);
  }

  public ProlConsult(final String string, final ProlContext context) {
    this(new ProlReader(string), context);
  }

  public ProlConsult(final InputStream in, final ProlContext context) {
    this(new ProlReader(in), context);
  }

  public ProlConsult(final Reader reader, final ProlContext context) {
    this(new ProlReader(reader), context);
  }

  public ProlConsult(final ProlReader reader, final ProlContext context) {
    this.reader = reader;
    this.context = context;
    this.base = context.getKnowledgeBase();
  }

  public void consult() throws IOException {
    if (this.reader == null) {
      return;
    }

    final ProlTreeBuilder treeBuilder = new ProlTreeBuilder(context);
    final ProlTokenizer tokenizer = new ProlTokenizer();

    final Thread thisthread = Thread.currentThread();

    while (!thisthread.isInterrupted()) {

      final Term nextItem = treeBuilder.readPhraseAndMakeTree(tokenizer, reader);
      if (nextItem == null) {
        break;
      }

      final int line = tokenizer.getLastTokenLineNum();
      final int strpos = tokenizer.getLastTokenStrPos();

      try {
        switch (nextItem.getTermType()) {
          case ATOM: {
            base.assertZ(this.context, newStruct(nextItem));
          }
          break;
          case STRUCT: {
            final TermStruct struct = (TermStruct) nextItem;
            final Term functor = struct.getFunctor();

            switch (functor.getTermType()) {
              case OPERATOR: {
                final Operator op = (Operator) functor;
                final String text = op.getText();
                final OpAssoc type = op.getOperatorType();

                if (struct.isClause()) {
                  switch (type) {
                    case XFX: {
                      // new rule
                      base.assertZ(this.context, struct);
                    }
                    break;
                    case FX: {
                      // directive
                      if (!processDirective(struct.getElement(0))) {
                        throw new ProlHaltExecutionException(2);
                      }
                    }
                    break;
                  }

                } else if ("?-".equals(text)) {
                  // goal
                  final Reader userreader = context.getStreamManager().getReaderForResource("user");
                  final Writer userwriter = context.getStreamManager().getWriterForResource("user", true);

                  final Term termGoal = struct.getElement(0);

                  if (userwriter != null) {
                    userwriter.write(String.format("Goal: %s%n", termGoal.forWrite()));
                  }
                  final Map<String, Var> varmap = new HashMap<>();
                  int solutioncounter = 0;

                  final ChoicePoint thisGoal = new ChoicePoint(termGoal, context, null);

                  while (!Thread.currentThread().isInterrupted()) {
                    varmap.clear();
                    if (solveGoal(thisGoal, varmap)) {
                      solutioncounter++;
                      if (userwriter != null) {
                        userwriter.write(String.format("%nYES%n"));
                        if (!varmap.isEmpty()) {
                          for (Entry<String, Var> avar : varmap.entrySet()) {
                            final String name = avar.getKey();
                            final Var value = avar.getValue();
                            userwriter.write(String.format("%s=%s%n", name, value.isFree() ? "???" : value.forWrite()));
                            userwriter.flush();
                          }
                        }
                      }
                    } else {
                      if (userwriter != null) {
                        userwriter.write(String.format("%n%d %s%n%nNO%n", solutioncounter, (solutioncounter > 1 ? "solutions" : "solution")));
                      }
                      break;
                    }
                    if (userwriter != null && userreader != null) {
                      userwriter.append("Next solution? ");
                      final int chr = userreader.read();
                      if (!(chr == ';' || chr == 'y' || chr == 'Y')) {
                        break;
                      } else {
                        userwriter.write('\n');
                      }
                    }
                  }
                  if (userwriter != null) {
                    userwriter.flush();
                  }

                  throw new ProlHaltExecutionException("Halted because goal failed.", 1);
                } else {
                  base.assertZ(this.context, struct);
                }
              }
              break;
              default: {
                base.assertZ(this.context, struct);
              }
              break;
            }
          }
          break;
          default: {
            throw new ProlKnowledgeBaseException("Such element can't be saved at knowledge base [" + nextItem + ']');
          }
        }
      } catch (Throwable ex) {
        LOG.log(Level.SEVERE, "consult()", ex);
        if (ex instanceof ThreadDeath) {
          throw (ThreadDeath) ex;
        }
        //context.dispose();

        throw new ParserException(ex.getMessage(), line, strpos, ex);
      }
    }
  }

  public boolean processGoal(final String goal, final Map<String, Var> varTable) throws IOException, InterruptedException {
    final ProlTreeBuilder treebuilder = new ProlTreeBuilder(context);
    final Term term = treebuilder.readPhraseAndMakeTree(goal);
    return processGoal(term, varTable);
  }

  public boolean processGoal(final Term goalterm, final Map<String, Var> varTable) throws InterruptedException {
    final ChoicePoint goal = new ChoicePoint(goalterm, context, null);

    Term result = goal.next();

    if (result != null && varTable != null) {
      result.variables().forEach(e -> varTable.put(e.getText(), e));
    }

    return result != null;
  }

  private boolean solveGoal(final ChoicePoint goal, final Map<String, Var> varTable) throws InterruptedException {
    final Term result = goal.next();

    if (result != null && varTable != null) {
      result.variables().forEach(e -> varTable.put(e.getText(), e));
    }

    return result != null;
  }

  private boolean processDirective(final Term directive) throws IOException, InterruptedException {
    final ChoicePoint goal = new ChoicePoint(directive, context, null);
    return goal.next() != null;
  }
}
