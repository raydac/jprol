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
package com.igormaznitsa.prol.parser;

import com.igormaznitsa.prol.containers.KnowledgeBase;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.Operator;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.data.Var;
import com.igormaznitsa.prol.exceptions.ParserException;
import com.igormaznitsa.prol.exceptions.ProlHaltExecutionException;
import com.igormaznitsa.prol.exceptions.ProlKnowledgeBaseException;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * The class allows to input and parse a source string or resource to be placed
 * into a context. It could be called as interface of a context with an outside
 * world. Because you can enter data into context knowledge base only with the
 * consult object
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see com.igormaznitsa.prol.logic.ProlContext
 */
public class ProlConsult {

  /**
   * Inside logger for the library, as the logger name the canonical class name
   * is used (ProlConsult.class.getCanonicalName())
   */
  private static final Logger LOG = Logger.getLogger(ProlConsult.class.getCanonicalName());
  /**
   * The variable contains reader to read data from a resource
   */
  private final ProlReader reader;
  /**
   * The context bounding the consult object
   */
  private final ProlContext context;
  /**
   * The knowledge base from the context will be saved here for speed
   */
  private final KnowledgeBase base;

  /**
   * A constructor allows to create consult object without read operations
   *
   * @param context the context for the consult object, must not be null
   */
  public ProlConsult(final ProlContext context) {
    this((ProlReader) null, context);
  }

  /**
   * A constructor allows to make consulting from a String object
   *
   * @param string the string object to be consulted, must not be null
   * @param context the context for the consult object, must not be null
   */
  public ProlConsult(final String string, final ProlContext context) {
    this(new ProlReader(string), context);
  }

  /**
   * A constructor allows to make consulting from an input stream
   *
   * @param in the input stream to be the source for the consulting, must not be
   * null
   * @param context the context for the consult object, must not be null
   */
  public ProlConsult(final InputStream in, final ProlContext context) {
    this(new ProlReader(in), context);
  }

  /**
   * A constructor allows to make consulting from a reader
   *
   * @param reader the reader to be the source for the consulting, must not be
   * null
   * @param context the context for the consult object, must not be null
   */
  public ProlConsult(final Reader reader, final ProlContext context) {
    this(new ProlReader(reader), context);
  }

  /**
   * A constructor allows to make consulting from a prol reader
   *
   * @param reader the prol reader to be the source for the consulting, must not
   * be null
   * @param context the context for the consult object, must not be null
   */
  public ProlConsult(final ProlReader reader, final ProlContext context) {
    this.reader = reader;
    this.context = context;
    this.base = context.getKnowledgeBase();
  }

  /**
   * To consult for defined input source (if it presented)
   *
   * @throws IOException will be thrown if there will be any erro during a
   * transport operation
   */
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
          case Term.TYPE_ATOM: {
            base.assertZ(new TermStruct(nextItem));
          }
          break;
          case Term.TYPE_STRUCT: {
            final TermStruct struct = (TermStruct) nextItem;
            final Term functor = struct.getFunctor();

            switch (functor.getTermType()) {
              case Term.TYPE_OPERATOR: {
                final Operator op = (Operator) functor;
                final String text = op.getText();
                final int type = op.getOperatorType();

                if (struct.isFunctorLikeRuleDefinition()) {
                  switch (type) {
                    case Operator.OPTYPE_XFX: {
                      // new rule
                      base.assertZ(struct);
                    }
                    break;
                    case Operator.OPTYPE_FX: {
                      // directive
                      if (!processDirective(struct.getElement(0))) {
                        throw new ProlHaltExecutionException(2);
                      }
                    }
                    break;
                  }

                }
                else if ("?-".equals(text)) {
                  // goal
                  final Reader userreader = context.getStreamManager().getReaderForResource("user");
                  final Writer userwriter = context.getStreamManager().getWriterForResource("user", true);

                  final Term termGoal = struct.getElement(0);

                  if (userwriter != null) {
                    userwriter.write("Goal: ");
                    userwriter.write(termGoal.forWrite());
                    userwriter.write("\r\n");
                  }
                  final Map<String, Var> varmap = new HashMap<String, Var>();
                  int solutioncounter = 0;

                  final Goal thisGoal = new Goal(termGoal, context, null);

                  while (true) {
                    varmap.clear();
                    if (solveGoal(thisGoal, varmap)) {
                      solutioncounter++;
                      if (userwriter != null) {
                        userwriter.write("\r\nYES\r\n");
                        if (!varmap.isEmpty()) {
                          for (Entry<String, Var> avar : varmap.entrySet()) {
                            final String name = avar.getKey();
                            final Var value = avar.getValue();
                            userwriter.write(name);
                            userwriter.write('=');
                            if (value.isUndefined()) {
                              userwriter.write("???\r\n");
                            }
                            else {
                              userwriter.write(value.forWrite());
                              userwriter.write("\r\n");
                            }
                            userwriter.flush();
                          }
                        }
                      }
                    }
                    else {
                      if (userwriter != null) {
                        userwriter.write(solutioncounter + (solutioncounter > 1 ? " Solutions\r\n" : " Solution\r\n"));
                        userwriter.write("\r\nNO\r\n");
                      }
                      break;
                    }
                    if (userwriter != null && userreader != null) {
                      userwriter.append("Next solution? ");
                      final int chr = userreader.read();
                      if (chr < 0) {
                        if (userwriter != null) {
                          userwriter.append("    Can't get the key value, it is possible the execution has been canceled.");
                        }
                        break;
                      }
                      else {
                        if (chr == ';') {
                          if (userwriter != null) {
                            userwriter.append("\r\n");
                          }
                          continue;
                        }
                        else {

                          if (userwriter != null) {
                            userwriter.write("\r\n" + solutioncounter + (solutioncounter > 1 ? " Solutions\r\n" : " Solution\r\n"));
                            userwriter.append("Stopped by user");
                          }
                          break;
                        }
                      }
                    }
                  }
                  if (userwriter != null) {
                    userwriter.flush();
                  }

                  throw new ProlHaltExecutionException("Halted because a goal failed.", 1);
                }
                else {
                  base.assertZ(struct);
                }
              }
              break;
              default: {
                base.assertZ(struct);
              }
              break;
            }
          }
          break;
          default: {
            throw new ProlKnowledgeBaseException("Such element can't be saved at the knowledge base [" + nextItem + ']');
          }
        }
      }
      catch (Throwable ex) {
        LOG.log(Level.SEVERE, "consult()", ex);
        if (ex instanceof ThreadDeath) {
          throw (ThreadDeath) ex;
        }
        //context.halt();

        throw new ParserException(ex.getMessage(), line, strpos, ex);
      }
    }
  }

  /**
   * Solve a goal for the context and fill the table by variables of the goal,
   * it finds the first solution for the goal if the var table is null
   *
   * @param goal the goal to be solved must not be null
   * @param varTable the table to be filled by variables, can be null if you
   * don't need it
   * @return true if the goal was solved, else false because the goal was not
   * solved
   * @throws IOException it will be thrown if there will be any problem to parse
   * goal
   * @throws InterruptedException it will be thrown if the thread has been
   * interrupted
   */
  public boolean processGoal(final String goal, final Map<String, Var> varTable) throws IOException, InterruptedException {
    final ProlTreeBuilder treebuilder = new ProlTreeBuilder(context);
    final Term term = treebuilder.readPhraseAndMakeTree(goal);
    return processGoal(term, varTable);
  }

  /**
   * Solve a goal (presented as term) for the context and fill the table by
   * variables of the goal, it finds the first solution for the goal if the var
   * table is null
   *
   * @param goalterm the goal to be solved must not be null
   * @param varTable the table to be filled by variables, can be null if you
   * don't need it
   * @return true if the goal was solved, else false because the goal was not
   * solved
   * @throws InterruptedException it will be thrown if the thread has been
   * interrupted
   */
  public boolean processGoal(final Term goalterm, final Map<String, Var> varTable) throws InterruptedException {
    final Goal goal = new Goal(goalterm, context, null);

    Term result = null;

    result = goal.solve();

    if (result != null && varTable != null) {
      result.fillVarables(varTable);
    }

    return result != null;
  }

  /**
   * Inside function to get next solution of a goal and fill a hash map by its
   * variables
   *
   * @param goal the goal to get next solution, must not be null
   * @param varTable a hash map which should be filled by variables from the
   * goal, can be null
   * @return true if there is a solution, else false
   * @throws InterruptedException it will be thrown if the thread has been
   * interruped
   */
  private final boolean solveGoal(final Goal goal, final Map<String, Var> varTable) throws InterruptedException {
    final Term result = goal.solve();

    if (result != null && varTable != null) {
      result.fillVarables(varTable);
    }

    return result != null;
  }

  /**
   * process directive from consulted stream (started from ':-'), it will be
   * solved single time.
   *
   * @param directive the directive term, must not be null
   * @return true true if the goal solved successfully, false if the goal failed
   * @throws IOException it will be thrown if there is any problem with IO
   * operations
   * @throws InterruptedException it will be thrown if the goal is interrupted
   */
  private final boolean processDirective(final Term directive) throws IOException, InterruptedException {
    final Goal goal = new Goal(directive, context, null);
    return goal.solve() != null;
  }
}
