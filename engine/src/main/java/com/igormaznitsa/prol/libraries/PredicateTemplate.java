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
package com.igormaznitsa.prol.libraries;

import com.igormaznitsa.prol.data.NumericTerm;
import com.igormaznitsa.prol.data.Operator;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermInteger;
import com.igormaznitsa.prol.data.TermList;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.data.Var;
import com.igormaznitsa.prol.exceptions.ProlCriticalError;
import com.igormaznitsa.prol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.prol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.prol.utils.Utils;
import java.lang.reflect.Field;

/**
 * The class describes a template for a predicate argument. A term of a
 * predicate will be checked with the template before execution.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see com.igormaznitsa.prol.annotations.Predicate
 */
public class PredicateTemplate {

  /**
   * atom -- an atom
   */
  public static final int TYPE_ATOM = 0;
  /**
   * atom_or_atom_list -- an atom or a list of atoms
   */
  public static final int TYPE_ATOM_OR_ATOM_LIST = 1;
  /**
   * atomic -- an atomic term
   */
  public static final int TYPE_ATOMIC = 2;
  /**
   * byte -- a byte
   */
  public static final int TYPE_BYTE = 3;
  /**
   * callable_term
   */
  public static final int TYPE_CALLABLE_TERM = 4;
  /**
   * character -- a one char atom
   */
  public static final int TYPE_CHARACTER = 5;
  /**
   * character_code -- a character code
   */
  public static final int TYPE_CHARACTER_CODE = 6;
  /**
   * character_code_list -- a list of character codes
   */
  public static final int TYPE_CHARACTER_CODE_LIST = 7;
  /**
   * character_list -- a list of characters
   */
  public static final int TYPE_CHARACTER_LIST = 8;
  /**
   * clause
   */
  public static final int TYPE_CLAUSE = 9;
  /**
   * close_options -- a list of close options
   */
  public static final int TYPE_CLOSE_OPTIONS = 10;
  /**
   * compound_term
   */
  public static final int TYPE_COMPOUND_TERM = 11;
  /**
   * evaluable -- an expression
   */
  public static final int TYPE_EVALUABLE = 12;
  /**
   * flag -- a flag
   */
  public static final int TYPE_FLAG = 13;
  /**
   * head of a clause
   */
  public static final int TYPE_HEAD = 14;
  /**
   * in_byte a byte or -1
   */
  public static final int TYPE_IN_BYTE = 15;
  /**
   * in_character a one char atom or the atom end_of_file
   */
  public static final int TYPE_IN_CHARACTER = 16;
  /**
   * in_character_code -- a character code or -1
   */
  public static final int TYPE_IN_CHARACTER_CODE = 17;
  /**
   * integer
   */
  public static final int TYPE_INTEGER = 18;
  /**
   * io_mode -- an input output mode
   */
  public static final int TYPE_IO_MODE = 19;
  /**
   * list
   */
  public static final int TYPE_LIST = 20;
  /**
   * nonvar -- an atomic term or a compound term
   */
  public static final int TYPE_NONVAR = 21;
  /**
   * number
   */
  public static final int TYPE_NUMBER = 22;
  /**
   * operator_specifier -- one of the atoms xf, yf, xfx, xfy, yfx, fx, fy
   */
  public static final int TYPE_OPERATOR_SPECIFIER = 23;
  /**
   * predicate_indicator
   */
  public static final int TYPE_PREDICATE_INDICATOR = 24;
  /**
   * read_options_list a read options list
   */
  public static final int TYPE_READ_OPTIONS = 25;
  /**
   * source_sink
   */
  public static final int TYPE_SOURCE_SINK = 26;
  /**
   * stream
   */
  public static final int TYPE_STREAM = 27;
  /**
   * stream_options -- a list of stream_options
   */
  public static final int TYPE_STREAM_OPTIONS = 28;
  /**
   * stream_or_alias -- a stream or an alias
   */
  public static final int TYPE_STREAM_OR_ALIAS = 29;
  /**
   * stream_position -- a steam position
   */
  public static final int TYPE_STREAM_POSITION = 30;
  /**
   * stream_property -- a stream property
   */
  public static final int TYPE_STREAM_PROPERTY = 31;
  /**
   * term
   */
  public static final int TYPE_TERM = 32;
  /**
   * write_options_list -- a write options list
   */
  public static final int TYPE_WRITE_OPTIONS_LIST = 33;
  /**
   * not_empty_list -- a list but null-list
   */
  public static final int TYPE_NON_EMPTY_LIST = 34;
  /**
   * triggerevent -- a trigger event describer (onassert, onretract,
   * onassertretract)
   */
  public static final int TYPE_TRIGGEREVENT = 35;
  //------------------------------------------------------------
  /**
   * The argument shall be instantiated. (+)
   */
  public static final int MODIFIER_SHALL_BE_INSTANTIATED = 0;
  /**
   * The argument shall remain unaltered. Unless the argument is a compound term
   * this is the same as the mode +. (@)
   */
  public static final int MODIFIER_SHALL_REMAIN_UNALTERED = 1;
  /**
   * The argument shall be a variable that will be instantiated if the goal
   * succeeds. (-)
   */
  public static final int MODIFIER_SHALL_BE_VARIABLE = 2;
  /**
   * The argument shall be instantiated or a variable. (?) The ISO standard
   * requires that 1. if the argument is instantiated, and 2. the type of the
   * instantiated argument is not that required by the template of the predicate
   * then an error is to be raised.
   */
  public static final int MODIFIER_SHALL_BE_INSTANTIATED_OR_VARIABLE = 3;
  /**
   * The variable contains the type of the template
   */
  public final int Type;
  /**
   * The variable contains the modifier of the template
   */
  public final int Modifier;

  /**
   * A constructor allows to make the tempate for predefined modifier and type
   *
   * @param modifier the modifier index
   * @param type the type index
   */
  public PredicateTemplate(final int modifier, final int type) {
    super();
    this.Modifier = modifier;
    this.Type = type;
  }

  /**
   * A constructor allows to generate a template from a string defined as in the
   * "<modifier><template>" format
   *
   * @param string a String object contains packed string in the right format
   * @throws IllegalArgumentException if the string has a wrong format
   */
  public PredicateTemplate(final String string) {
    super();
    if (string == null || string.length() <= 3) {
      throw new IllegalArgumentException("Can\'t parse template parameter \'" + string + '\'');
    }
    switch (string.charAt(0)) {
      case '+':
        this.Modifier = MODIFIER_SHALL_BE_INSTANTIATED;
        break;
      case '@':
        this.Modifier = MODIFIER_SHALL_REMAIN_UNALTERED;
        break;
      case '-':
        this.Modifier = MODIFIER_SHALL_BE_VARIABLE;
        break;
      case '?':
        this.Modifier = MODIFIER_SHALL_BE_INSTANTIATED_OR_VARIABLE;
        break;
      default:
        throw new IllegalArgumentException("Unsupported template modifier at \'" + string + '\'');
    }
    final String template = "TYPE_" + string.substring(1).toUpperCase();
    try {
      final Field field = this.getClass().getDeclaredField(template);
      this.Type = field.getInt(null);
    }
    catch (IllegalAccessException ex) {
      throw new Error("Illegal access error", ex);
    }
    catch (NoSuchFieldException ex) {
      throw new ProlCriticalError("Unsupported template \'" + string + '\'');
    }
  }

  /**
   * Check an argument for the template
   *
   * @param term the term to be checked
   * @return true if the term must not be changed during processing and false if
   * it can be changed
   */
  @SuppressWarnings("unchecked")
  public final boolean check(final Term term) {
    final int type = term.getTermType();
    switch (Modifier) {
      case MODIFIER_SHALL_REMAIN_UNALTERED: {
        if (type == Term.TYPE_LIST || type == Term.TYPE_STRUCT) {
          checkTermForTemplate(term);
          return false;
        }
      }
      case MODIFIER_SHALL_BE_INSTANTIATED: {
        if (type == Term.TYPE_VAR && ((Var) term).isUndefined()) {
          throw new ProlInstantiationErrorException("Should be instantiated \'" + term.getSourceLikeRepresentation() + '\'', term);
        }
        checkTermForTemplate(term);
      }
      break;
      case MODIFIER_SHALL_BE_INSTANTIATED_OR_VARIABLE: {
        // any
        if (type == Term.TYPE_VAR && ((Var) term).isUndefined()) {
          return false;
        }

        checkTermForTemplate(term);
      }
      break;
      case MODIFIER_SHALL_BE_VARIABLE: {
        if (type == Term.TYPE_VAR) {
          if (!((Var) term).isUndefined()) {
            throw new ProlInstantiationErrorException("Should not be instantiated \'" + term.getSourceLikeRepresentation() + '\'', term);
          }
        }
        else {
          throw new ProlInstantiationErrorException("Should be noninstantiated variable \'" + term.getSourceLikeRepresentation() + '\'', term);
        }
        return true;
      }
      default:
        throw new ProlCriticalError("Unknown template modifier");
    }
    return false;
  }

  /**
   * An auxulary function to check a term for compatibility with the template
   *
   * @param term a term to be checked, must not be null
   */
  private final void checkTermForTemplate(final Term term) {
    final Term checkAtom = Utils.getTermFromElement(term);

    switch (Type) {
      case TYPE_ATOM: {
        boolean error = true;
        if (checkAtom != null) {
          switch (checkAtom.getTermType()) {
            case Term.TYPE_LIST: {
              error = !((TermList) checkAtom).isNullList();
            }
            break;
            case Term.TYPE_ATOM: {
              error = checkAtom instanceof NumericTerm;
            }
            break;
          }
        }
        if (error) {
          throw new ProlInstantiationErrorException("Should be atom \'" + term + '\'', term);
        }
      }
      break;
      case TYPE_ATOM_OR_ATOM_LIST: {
        if (checkAtom != null) {
          boolean error = true;
          switch (checkAtom.getTermType()) {
            case Term.TYPE_ATOM: {
              if (!(checkAtom instanceof NumericTerm)) {
                error = false;
              }
            }
            break;
            case Term.TYPE_LIST: {
              TermList lst = (TermList) checkAtom;
              error = false;
              if (lst == TermList.NULLLIST) {
                break;
              }

              while (true) {
                Term head = lst.getHead();
                if (head.getTermType() == Term.TYPE_VAR) {
                  head = ((Var) head).getValue();
                  if (head == null) {
                    error = true;
                    break;
                  }
                }
                if (head.getTermType() != Term.TYPE_ATOM) {
                  error = true;
                  break;
                }

                final Term tail = lst.getTail();
                if (tail == TermList.NULLLIST) {
                  break;
                }
                if (tail.getTermType() == Term.TYPE_LIST) {
                  lst = (TermList) tail;
                }
                else {
                  error = true;
                  break;
                }
              }
            }
            break;
          }
          if (error) {
            throw new ProlInstantiationErrorException("Should be atom or atom list \'" + term + '\'', term);
          }
        }
      }
      break;
      case TYPE_ATOMIC: {
        if (checkAtom != null) {
          boolean errorresult = false;
          switch (checkAtom.getTermType()) {
            case Term.TYPE_LIST: {
              errorresult = !((TermList) checkAtom).isNullList();
            }
            break;
            case Term.TYPE_ATOM: {
            }
            break;
            default: {
              errorresult = true;
            }
            break;
          }

          if (errorresult) {
            throw new ProlInstantiationErrorException("Should be atomic \'" + term + '\'', term);
          }
        }
      }
      break;
      case TYPE_BYTE: {
        if (checkAtom != null) {
          boolean error = true;
          if (checkAtom instanceof TermInteger) {
            final int value = ((TermInteger) checkAtom).getNumericValue().intValue();
            if ((value & 0xFF) == 0) {
              error = false;
            }
          }
          if (error) {
            throw new ProlInstantiationErrorException("Should be byte \'" + term + '\'', term);
          }
        }
      }
      break;
      case TYPE_CALLABLE_TERM: {
        if (checkAtom != null) {
          boolean error = true;

          final int typeAtom = checkAtom.getTermType();
          if (typeAtom == Term.TYPE_ATOM) {
            if (!(checkAtom instanceof NumericTerm)) {
              error = false;
            }
          }
          else if (typeAtom == Term.TYPE_STRUCT) {
            error = false;
          }
          if (error) {
            throw new ProlInstantiationErrorException("Should be callable term \'" + term + '\'', term);
          }
        }
      }
      break;
      case TYPE_CHARACTER: {
        if (checkAtom != null) {
          boolean error = true;

          if (checkAtom.getTermType() == Term.TYPE_ATOM && checkAtom.getText().length() == 1) {
            error = false;
          }

          if (error) {
            throw new ProlInstantiationErrorException("Should be character \'" + term + '\'', term);
          }
        }
      }
      break;
      case TYPE_CHARACTER_CODE: {
        if (checkAtom != null) {
          boolean error = true;
          if (checkAtom instanceof TermInteger) {
            final int value = ((TermInteger) checkAtom).getNumericValue().intValue();
            if ((value & 0xFFFF0000) == 0) {
              error = false;
            }
          }
          if (error) {
            throw new ProlInstantiationErrorException("Should be character code \'" + term + '\'', term);
          }
        }
      }
      break;
      case TYPE_CHARACTER_CODE_LIST: {
        boolean error = false;
        if (checkAtom != null) {
          if (checkAtom.getTermType() == Term.TYPE_LIST) {
            TermList lst = (TermList) checkAtom;
            error = false;
            if (lst == TermList.NULLLIST) {
              break;
            }

            while (true) {
              Term head = lst.getHead();
              if (head.getTermType() == Term.TYPE_VAR) {
                head = ((Var) head).getValue();
                if (head == null) {
                  error = true;
                  break;
                }
              }
              if (head.getTermType() == Term.TYPE_ATOM) {
                if (head instanceof TermInteger) {
                  if ((((TermInteger) head).getNumericValue().intValue() & 0xFFFF0000) != 0) {
                    error = true;
                    break;
                  }
                }
                else {
                  error = true;
                  break;
                }
              }
              else {
                error = true;
                break;
              }

              final Term tail = lst.getTail();
              if (tail == TermList.NULLLIST) {
                break;
              }
              if (tail.getTermType() == Term.TYPE_LIST) {
                lst = (TermList) tail;
              }
              else {
                error = true;
                break;
              }
            }
          }
          else {
            error = true;
          }
        }
        if (error) {
          throw new ProlInstantiationErrorException("Should be character code list \'" + term + '\'', term);
        }
      }
      break;
      case TYPE_CHARACTER_LIST: {
        boolean error = false;
        if (checkAtom != null) {
          if (checkAtom.getTermType() == Term.TYPE_LIST) {
            TermList lst = (TermList) checkAtom;
            error = false;
            if (lst == TermList.NULLLIST) {
              break;
            }

            while (true) {
              Term head = lst.getHead();
              if (head.getTermType() == Term.TYPE_VAR) {
                head = ((Var) head).getValue();
                if (head == null) {
                  error = true;
                  break;
                }
              }
              if (head.getTermType() == Term.TYPE_ATOM) {
                if (head.getText().length() != 1) {
                  error = true;
                  break;
                }
              }
              else {
                error = true;
                break;
              }

              final Term tail = lst.getTail();
              if (tail == TermList.NULLLIST) {
                break;
              }
              if (tail.getTermType() == Term.TYPE_LIST) {
                lst = (TermList) tail;
              }
              else {
                error = true;
                break;
              }
            }
          }
          else {
            error = true;
          }
        }
        if (error) {
          throw new ProlInstantiationErrorException("Should be character code list \'" + term + '\'', term);
        }
      }
      break;
      case TYPE_CLAUSE: {
        if (checkAtom != null) {
          boolean error = false;
          switch (checkAtom.getTermType()) {
            case Term.TYPE_ATOM: {
              if (checkAtom instanceof NumericTerm) {
                error = true;
              }
            }
            break;
            case Term.TYPE_STRUCT: {
              final TermStruct struct = (TermStruct) checkAtom;
              final Term functor = struct.getFunctor();

              final boolean rule = struct.isFunctorLikeRuleDefinition();
              final int functorType = functor.getTermType();

              // check left part
              if (rule) {
                final Term left = struct.getElement(0);
                switch (left.getTermType()) {
                  case Term.TYPE_ATOM: {
                    if (left instanceof NumericTerm) {
                      error = true;
                    }
                  }
                  break;
                  case Term.TYPE_LIST: {
                    error = true;
                  }
                  break;
                  case Term.TYPE_VAR: {
                    error = true;
                  }
                  break;
                }
              }
              else {
                switch (functorType) {
                  case Term.TYPE_ATOM: {
                    if (functor instanceof NumericTerm) {
                      error = true;
                    }
                  }
                  break;
                  case Term.TYPE_LIST: {
                    error = true;
                  }
                  break;
                  case Term.TYPE_VAR: {
                    error = true;
                  }
                  break;
                }
              }
            }
            break;
            default: {
              error = true;
            }
            break;
          }
          if (error) {
            throw new ProlInstantiationErrorException("Should be clause or atom \'" + term + '\'', term);
          }
        }
      }
      break;

      case TYPE_COMPOUND_TERM: {
        if (checkAtom != null) {
          switch (checkAtom.getTermType()) {
            case Term.TYPE_LIST:
            case Term.TYPE_STRUCT: {
            }
            break;
            default:
              throw new ProlInstantiationErrorException("Should be compound term \'" + term + '\'', term);
          }
        }
      }
      break;
      case TYPE_EVALUABLE: {
        if (checkAtom != null) {
          boolean error = true;
          if (checkAtom instanceof NumericTerm) {
            error = false;
          }
          else {
            if (checkAtom.getTermType() == Term.TYPE_STRUCT) {
              final TermStruct struct = (TermStruct) checkAtom;
              final PredicateProcessor processor = struct.getPredicateProcessor();
              if (processor.isEvaluable()) {
                error = false;
              }
            }
          }
          if (error) {
            throw new ProlInstantiationErrorException("Should be evaluable \'" + term + '\'', term);
          }
        }

      }
      break;
      case TYPE_HEAD: {
        if (checkAtom != null) {
          boolean error = true;
          switch (checkAtom.getTermType()) {
            case Term.TYPE_ATOM: {
              if (!(checkAtom instanceof NumericTerm)) {
                error = false;
              }
            }
            break;
            case Term.TYPE_STRUCT: {
              final Term functor = ((TermStruct) checkAtom).getFunctor();
              if (functor.getTermType() == Term.TYPE_ATOM) {
                error = false;
              }
            }
            break;
          }
          if (error) {
            throw new ProlInstantiationErrorException("Imcompatible clause head", term);
          }
        }
      }
      break;
      case TYPE_IN_BYTE: {
        boolean error = false;
        if (checkAtom != null) {
          if (checkAtom instanceof TermInteger) {
            final int val = ((TermInteger) checkAtom).getNumericValue().intValue();
            if ((val & 0xFF) != 0 && val == -1) {
              error = true;
            }
          }
          else {
            error = true;
          }
        }
        if (error) {
          throw new ProlInstantiationErrorException("Should be byte or -1 \'" + term + '\'', term);
        }
      }
      break;
      case TYPE_IN_CHARACTER: {
        boolean error = false;
        if (checkAtom != null) {
          if (checkAtom.getTermType() == Term.TYPE_ATOM) {
            final String text = checkAtom.getText();
            if (text.length() != 1 && !"end_of_file".equals(text)) {
              error = true;
            }
          }
          else {
            error = true;
          }
        }
        if (error) {
          throw new ProlInstantiationErrorException("Should be character code or -1 \'" + term + '\'', term);
        }
      }
      break;
      case TYPE_IN_CHARACTER_CODE: {
        boolean error = false;
        if (checkAtom != null) {
          if (checkAtom instanceof TermInteger) {
            final int val = ((TermInteger) checkAtom).getNumericValue().intValue();
            if ((val & 0xFFFF0000) != 0 && val != -1) {
              error = true;
            }
          }
          else {
            error = true;
          }
        }
        if (error) {
          throw new ProlInstantiationErrorException("Should be character code or -1 \'" + term + '\'', term);
        }
      }
      break;
      case TYPE_INTEGER: {
        if (checkAtom != null && !(checkAtom instanceof TermInteger)) {
          throw new ProlInstantiationErrorException("Should be integer \'" + term + '\'', term);
        }
      }
      break;
      case TYPE_IO_MODE: {
        if (checkAtom != null) {
          boolean error = true;
          if (checkAtom.getTermType() == Term.TYPE_ATOM) {
            final String text = checkAtom.getText();
            if (text.equals("read") || text.equals("write") || text.equals("append")) {
              error = false;
            }
          }
          if (error) {
            throw new ProlInstantiationErrorException("Should be 'read', 'write' or 'append' [" + term + ']', term);
          }
        }
      }
      break;
      case TYPE_LIST: {
        if (checkAtom != null) {
          if (checkAtom.getTermType() != Term.TYPE_LIST) {
            throw new ProlInstantiationErrorException("Should be list \'" + term + '\'', term);
          }
        }
      }
      break;
      case TYPE_NON_EMPTY_LIST: {
        if (checkAtom != null) {
          if (checkAtom.getTermType() != Term.TYPE_LIST) {
            throw new ProlInstantiationErrorException("Should be list \'" + term + '\'', term);
          }
          else {
            if (checkAtom == TermList.NULLLIST) {
              throw new ProlInstantiationErrorException("Should not be empty list \'" + term + '\'', term);
            }
          }
        }
      }
      break;
      case TYPE_TRIGGEREVENT: {
        if (checkAtom != null) {
          if (checkAtom.getTermType() != Term.TYPE_ATOM) {
            throw new ProlInstantiationErrorException("Should be an atom \'" + term + '\'', term);
          }
          else {
            final String value = checkAtom.getText();
            if (!"onassert".equals(value) && !"onretract".equals(value) && !"onassertretract".equals(value)) {
              throw new ProlDomainErrorException("Should be a value from the list [onassert, onretract, onassertretract] \'" + term + '\'', term);
            }
          }
        }
      }
      break;
      case TYPE_NONVAR: {
        if (checkAtom == null) {
          throw new ProlInstantiationErrorException("Should be nonvar \'" + term + '\'', term);
        }
      }
      break;
      case TYPE_NUMBER: {
        if (checkAtom != null) {
          if (!(checkAtom instanceof NumericTerm)) {
            throw new ProlInstantiationErrorException("Should be number \'" + term + '\'', term);
          }
        }
      }
      break;
      case TYPE_OPERATOR_SPECIFIER: {
        boolean error = false;
        if (checkAtom != null) {
          if (checkAtom.getTermType() == Term.TYPE_ATOM && !(checkAtom instanceof NumericTerm)) {
            final String text = checkAtom.getText();
            error = true;
            if (Operator.getTypeFromString(text) >= 0) {
              error = false;
            }
          }
          else {
            error = true;
          }
        }
        else {
          error = true;
        }
        if (error) {
          throw new ProlDomainErrorException("Should be only [xfx,yfx,xfy,xf,fx,yf,fy] but \'" + term + '\'', term);
        }
      }
      break;
      case TYPE_PREDICATE_INDICATOR: {
        if (checkAtom != null) {
          boolean error = true;
          switch (checkAtom.getTermType()) {
            case Term.TYPE_STRUCT: {
              error = Utils.extractPredicateSignatureFromStructure((TermStruct) checkAtom) == null;
            }
            break;
          }

          if (error) {
            throw new ProlInstantiationErrorException("Should be predicate indicator \'" + term + '\'', term);
          }
        }
      }
      break;
      case TYPE_TERM: {
        // any term is term
      }
      break;
      case TYPE_FLAG:
      case TYPE_CLOSE_OPTIONS:
      case TYPE_READ_OPTIONS:
      case TYPE_SOURCE_SINK:
      case TYPE_STREAM:
      case TYPE_STREAM_OPTIONS:
      case TYPE_STREAM_OR_ALIAS:
      case TYPE_STREAM_POSITION:
      case TYPE_STREAM_PROPERTY:
      case TYPE_WRITE_OPTIONS_LIST:
      default:
        throw new ProlCriticalError("Unknown or nonimplemented template type");
    }
  }
}
