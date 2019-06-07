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

import com.igormaznitsa.prol.data.*;
import com.igormaznitsa.prol.exceptions.ProlCriticalError;
import com.igormaznitsa.prol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.prol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.prol.utils.Utils;

import static com.igormaznitsa.prol.data.TermType.*;

public class PredicateTemplate {

  public final PredicateTemplateType Type;
  public final PredicateTemplateModifier Modifier;

  public PredicateTemplate(final PredicateTemplateModifier modifier, final PredicateTemplateType type) {
    super();
    this.Modifier = modifier;
    this.Type = type;
  }

  public PredicateTemplate(final String string) {
    super();
    if (string == null || string.length() <= 3) {
      throw new IllegalArgumentException("Can\'t parse template parameter \'" + string + '\'');
    }

    this.Modifier = PredicateTemplateModifier.findForName(string.charAt(0));
    if (Modifier == null) {
      throw new IllegalArgumentException("Unsupported template modifier at \'" + string + '\'');
    }
    this.Type = PredicateTemplateType.findForName(string.substring(1));
    if (this.Type == null) {
      throw new ProlCriticalError("Unsupported template \'" + string + '\'');
    }
  }

  public final boolean shouldNotBeAltered(final Term term) {
    final TermType type = term.getTermType();
    switch (Modifier) {
      case SHALL_REMAIN_UNALTERED: {
        if (type == LIST || type == STRUCT) {
          checkTermForTemplate(term);
          return false;
        }
      }
      case SHALL_BE_INSTANTIATED: {
        if (type == VAR && ((Var) term).isUndefined()) {
          throw new ProlInstantiationErrorException("Should be instantiated \'" + term.getSourceLikeRepresentation() + '\'', term);
        }
        checkTermForTemplate(term);
      }
      break;
      case SHALL_BE_INSTANTIATED_OR_VARIABLE: {
        // any
        if (type == VAR && ((Var) term).isUndefined()) {
          return false;
        }

        checkTermForTemplate(term);
      }
      break;
      case SHALL_BE_VARIABLE: {
        if (type == VAR) {
          if (!((Var) term).isUndefined()) {
            throw new ProlInstantiationErrorException("Should not be instantiated \'" + term.getSourceLikeRepresentation() + '\'', term);
          }
        } else {
          throw new ProlInstantiationErrorException("Should be noninstantiated variable \'" + term.getSourceLikeRepresentation() + '\'', term);
        }
        return true;
      }
      default:
        throw new ProlCriticalError("Unknown template modifier");
    }
    return false;
  }

  private boolean checkTermForTemplate(final Term term) {
    final Term checkAtom = Utils.getTermFromElement(term);

    switch (Type) {
      case ATOM: {
        boolean error = true;
        switch (checkAtom.getTermType()) {
          case LIST: {
            error = !((TermList) checkAtom).isNullList();
          }
          break;
          case ATOM: {
            error = checkAtom instanceof NumericTerm;
          }
          break;
        }
        if (error) {
          throw new ProlInstantiationErrorException("Should be atom \'" + term + '\'', term);
        }
      }
      break;
      case ATOM_OR_ATOM_LIST: {
        if (checkAtom != null) {
          boolean error = true;
          switch (checkAtom.getTermType()) {
            case ATOM: {
              if (!(checkAtom instanceof NumericTerm)) {
                error = false;
              }
            }
            break;
            case LIST: {
              TermList lst = (TermList) checkAtom;
              error = false;
              if (lst == TermList.NULLLIST) {
                break;
              }

              while (!Thread.currentThread().isInterrupted()) {
                Term head = lst.getHead();
                if (head.getTermType() == VAR) {
                  head = ((Var) head).getValue();
                  if (head == null) {
                    error = true;
                    break;
                  }
                }
                if (head.getTermType() != ATOM) {
                  error = true;
                  break;
                }

                final Term tail = lst.getTail();
                if (tail == TermList.NULLLIST) {
                  break;
                }
                if (tail.getTermType() == LIST) {
                  lst = (TermList) tail;
                } else {
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
      case ATOMIC: {
        if (checkAtom != null) {
          boolean errorresult = false;
          switch (checkAtom.getTermType()) {
            case LIST: {
              errorresult = !((TermList) checkAtom).isNullList();
            }
            break;
            case ATOM: {
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
      case BYTE: {
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
      case CALLABLE_TERM: {
        if (checkAtom != null) {
          boolean error = true;

          final TermType typeAtom = checkAtom.getTermType();
          if (typeAtom == ATOM) {
            if (!(checkAtom instanceof NumericTerm)) {
              error = false;
            }
          } else if (typeAtom == STRUCT) {
            error = false;
          }
          if (error) {
            throw new ProlInstantiationErrorException("Should be callable term \'" + term + '\'', term);
          }
        }
      }
      break;
      case CHARACTER: {
        if (checkAtom != null) {
          boolean error = true;

          if (checkAtom.getTermType() == ATOM && checkAtom.getText().length() == 1) {
            error = false;
          }

          if (error) {
            throw new ProlInstantiationErrorException("Should be character \'" + term + '\'', term);
          }
        }
      }
      break;
      case CHARACTER_CODE: {
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
      case CHARACTER_CODE_LIST: {
        boolean error = false;
        if (checkAtom != null) {
          if (checkAtom.getTermType() == LIST) {
            TermList lst = (TermList) checkAtom;
            error = false;
            if (lst == TermList.NULLLIST) {
              break;
            }

            while (!Thread.currentThread().isInterrupted()) {
              Term head = lst.getHead();
              if (head.getTermType() == VAR) {
                head = ((Var) head).getValue();
                if (head == null) {
                  error = true;
                  break;
                }
              }
              if (head.getTermType() == ATOM) {
                if (head instanceof TermInteger) {
                  if ((((TermInteger) head).getNumericValue().intValue() & 0xFFFF0000) != 0) {
                    error = true;
                    break;
                  }
                } else {
                  error = true;
                  break;
                }
              } else {
                error = true;
                break;
              }

              final Term tail = lst.getTail();
              if (tail == TermList.NULLLIST) {
                break;
              }
              if (tail.getTermType() == LIST) {
                lst = (TermList) tail;
              } else {
                error = true;
                break;
              }
            }
          } else {
            error = true;
          }
        }
        if (error) {
          throw new ProlInstantiationErrorException("Should be character code list \'" + term + '\'', term);
        }
      }
      break;
      case CHARACTER_LIST: {
        boolean error = false;
        if (checkAtom != null) {
          if (checkAtom.getTermType() == LIST) {
            TermList lst = (TermList) checkAtom;
            error = false;
            if (lst == TermList.NULLLIST) {
              break;
            }

            while (!Thread.currentThread().isInterrupted()) {
              Term head = lst.getHead();
              if (head.getTermType() == VAR) {
                head = ((Var) head).getValue();
                if (head == null) {
                  error = true;
                  break;
                }
              }
              if (head.getTermType() == ATOM) {
                if (head.getText().length() != 1) {
                  error = true;
                  break;
                }
              } else {
                error = true;
                break;
              }

              final Term tail = lst.getTail();
              if (tail == TermList.NULLLIST) {
                break;
              }
              if (tail.getTermType() == LIST) {
                lst = (TermList) tail;
              } else {
                error = true;
                break;
              }
            }
          } else {
            error = true;
          }
        }
        if (error) {
          throw new ProlInstantiationErrorException("Should be character code list \'" + term + '\'', term);
        }
      }
      break;
      case CLAUSE: {
        if (checkAtom != null) {
          boolean error = false;
          switch (checkAtom.getTermType()) {
            case ATOM: {
              if (checkAtom instanceof NumericTerm) {
                error = true;
              }
            }
            break;
            case STRUCT: {
              final TermStruct struct = (TermStruct) checkAtom;
              final Term functor = struct.getFunctor();

              final boolean rule = struct.isFunctorLikeRuleDefinition();
              final TermType functorType = functor.getTermType();

              // check left part
              if (rule) {
                final Term left = struct.getElement(0);
                switch (left.getTermType()) {
                  case ATOM: {
                    if (left instanceof NumericTerm) {
                      error = true;
                    }
                  }
                  break;
                  case LIST: {
                    error = true;
                  }
                  break;
                  case VAR: {
                    error = true;
                  }
                  break;
                }
              } else {
                switch (functorType) {
                  case ATOM: {
                    if (functor instanceof NumericTerm) {
                      error = true;
                    }
                  }
                  break;
                  case LIST: {
                    error = true;
                  }
                  break;
                  case VAR: {
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

      case COMPOUND_TERM: {
        if (checkAtom != null) {
          switch (checkAtom.getTermType()) {
            case LIST:
            case STRUCT: {
            }
            break;
            default:
              throw new ProlInstantiationErrorException("Should be compound term \'" + term + '\'', term);
          }
        }
      }
      break;
      case EVALUABLE: {
        if (checkAtom != null) {
          boolean error = true;
          if (checkAtom instanceof NumericTerm) {
            error = false;
          } else {
            if (checkAtom.getTermType() == STRUCT) {
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
      case HEAD: {
        if (checkAtom != null) {
          boolean error = true;
          switch (checkAtom.getTermType()) {
            case ATOM: {
              if (!(checkAtom instanceof NumericTerm)) {
                error = false;
              }
            }
            break;
            case STRUCT: {
              final Term functor = ((TermStruct) checkAtom).getFunctor();
              if (functor.getTermType() == ATOM) {
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
      case IN_BYTE: {
        boolean error = false;
        if (checkAtom != null) {
          if (checkAtom instanceof TermInteger) {
            final int val = ((TermInteger) checkAtom).getNumericValue().intValue();
            if ((val & 0xFF) != 0 && val == -1) {
              error = true;
            }
          } else {
            error = true;
          }
        }
        if (error) {
          throw new ProlInstantiationErrorException("Should be byte or -1 \'" + term + '\'', term);
        }
      }
      break;
      case IN_CHARACTER: {
        boolean error = false;
        if (checkAtom != null) {
          if (checkAtom.getTermType() == ATOM) {
            final String text = checkAtom.getText();
            if (text.length() != 1 && !"end_of_file".equals(text)) {
              error = true;
            }
          } else {
            error = true;
          }
        }
        if (error) {
          throw new ProlInstantiationErrorException("Should be character code or -1 \'" + term + '\'', term);
        }
      }
      break;
      case IN_CHARACTER_CODE: {
        boolean error = false;
        if (checkAtom != null) {
          if (checkAtom instanceof TermInteger) {
            final int val = ((TermInteger) checkAtom).getNumericValue().intValue();
            if ((val & 0xFFFF0000) != 0 && val != -1) {
              error = true;
            }
          } else {
            error = true;
          }
        }
        if (error) {
          throw new ProlInstantiationErrorException("Should be character code or -1 \'" + term + '\'', term);
        }
      }
      break;
      case INTEGER: {
        if (checkAtom != null && !(checkAtom instanceof TermInteger)) {
          throw new ProlInstantiationErrorException("Should be integer \'" + term + '\'', term);
        }
      }
      break;
      case IO_MODE: {
        if (checkAtom != null) {
          boolean error = true;
          if (checkAtom.getTermType() == ATOM) {
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
      case LIST: {
        if (checkAtom != null) {
          if (checkAtom.getTermType() != LIST) {
            throw new ProlInstantiationErrorException("Should be list \'" + term + '\'', term);
          }
        }
      }
      break;
      case NON_EMPTY_LIST: {
        if (checkAtom != null) {
          if (checkAtom.getTermType() != LIST) {
            throw new ProlInstantiationErrorException("Should be list \'" + term + '\'', term);
          } else {
            if (checkAtom == TermList.NULLLIST) {
              throw new ProlInstantiationErrorException("Should not be empty list \'" + term + '\'', term);
            }
          }
        }
      }
      break;
      case TRIGGEREVENT: {
        if (checkAtom != null) {
          if (checkAtom.getTermType() != ATOM) {
            throw new ProlInstantiationErrorException("Should be an atom \'" + term + '\'', term);
          } else {
            final String value = checkAtom.getText();
            if (!"onassert".equals(value) && !"onretract".equals(value) && !"onassertretract".equals(value)) {
              throw new ProlDomainErrorException("Should be a value from the list [onassert, onretract, onassertretract] \'" + term + '\'', term);
            }
          }
        }
      }
      break;
      case NONVAR: {
        if (checkAtom == null) {
          throw new ProlInstantiationErrorException("Should be nonvar \'" + term + '\'', term);
        }
      }
      break;
      case NUMBER: {
        if (checkAtom != null) {
          if (!(checkAtom instanceof NumericTerm)) {
            throw new ProlInstantiationErrorException("Should be number \'" + term + '\'', term);
          }
        }
      }
      break;
      case OPERATOR_SPECIFIER: {
        boolean error;
        if (checkAtom != null) {
          if (checkAtom.getTermType() == ATOM && !(checkAtom instanceof NumericTerm)) {
            final String text = checkAtom.getText();
            error = true;
            if (Operator.getTypeFromString(text) >= 0) {
              error = false;
            }
          } else {
            error = true;
          }
        } else {
          error = true;
        }
        if (error) {
          throw new ProlDomainErrorException("Should be only [xfx,yfx,xfy,xf,fx,yf,fy] but \'" + term + '\'', term);
        }
      }
      break;
      case PREDICATE_INDICATOR: {
        if (checkAtom != null) {
          boolean error = true;
          switch (checkAtom.getTermType()) {
            case STRUCT: {
              error = Utils.extractPredicateSignatureFromStructure(checkAtom) == null;
            }
            break;
          }

          if (error) {
            throw new ProlInstantiationErrorException("Should be predicate indicator \'" + term + '\'', term);
          }
        }
      }
      break;
      case TERM: {
        // any term is term
      }
      break;
      default:
        throw new ProlCriticalError("Unknown or nonimplemented template type");
    }
    return true;
  }
}
