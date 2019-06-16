package com.igormaznitsa.prol.data;

import com.igormaznitsa.prol.libraries.PredicateProcessor;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prologparser.terms.*;
import com.igormaznitsa.prologparser.tokenizer.Op;

import java.util.HashMap;
import java.util.Map;

public final class Terms {
  public static final TermLong INT_ONE = new TermLong(1L);
  public static final TermLong INT_ZERO = new TermLong(0L);
  public static final TermLong INT_MINUS_ONE = new TermLong(-1L);
  public static final Term LIST_FUNCTOR = new Term(".");
  public static final TermList NULL_LIST = new TermList();

  public static Term newAtom(final String text) {
    if (".".equals(text)) {
      return LIST_FUNCTOR;
    } else {
      return new Term(text);
    }
  }

  public static TermDouble newDouble(final String text) {
    return new TermDouble(text);
  }

  public static TermDouble newDouble(final double value) {
    return new TermDouble(value);
  }

  public static TermLong newLong(final String text) {
    return new TermLong(text);
  }

  public static TermLong newLong(final long value) {
    if (value == 0L) {
      return INT_ZERO;
    } else if (value == 1L) {
      return INT_ONE;
    } else if (value == -1L) {
      return INT_MINUS_ONE;
    } else {
      return new TermLong(value);
    }
  }

  public static TermList newList(final Term term) {
    return new TermList(term);
  }

  public static TermList newList(final Term head, final Term tail) {
    return new TermList(head, tail);
  }

  public static TermVar newVar(final String name) {
    return new TermVar(name);
  }

  public static TermVar newVar() {
    return new TermVar();
  }

  public static TermStruct newStruct(final Term functor) {
    return new TermStruct(functor);
  }

  public static TermStruct newStruct(final String functor, final Term[] elements) {
    return new TermStruct(functor, elements);
  }

  public static TermStruct newStruct(final Term functor, final Term[] elements) {
    return new TermStruct(functor, elements);
  }

  public static TermStruct newStruct(final Term functor, final Term[] elements, final PredicateProcessor processor) {
    return new TermStruct(functor, elements, processor);
  }

  private static Term convert(final ProlContext context, final PrologTerm term, final Map<String, TermVar> vars) {
    switch (term.getType()) {
      case ATOM: {
        if (term instanceof PrologNumeric) {
          if (term instanceof PrologFloat) {
            return newDouble(((PrologFloat) term).getFloatValue().doubleValue());
          } else {
            return newLong(((PrologInt) term).getIntValue().longValue());
          }
        } else {
          final String text = term.getText();
          if (context.hasZeroArityPredicateForName(text)) {
            final TermStruct result = newStruct(text, new Term[0]);
            result.setPredicateProcessor(context.findProcessor(result));
            return result;
          } else {
            return newAtom(text);
          }
        }
      }
      case OPERATOR: {
        return context.findOperatorForName(term.getText()).getForTypePrecisely(((Op) term).getAssoc());
      }
      case LIST: {
        final PrologList list = (PrologList) term;
        return list.isEmpty() ? NULL_LIST : newList(convert(context, list.getHead(), vars), convert(context, list.getTail(), vars));
      }
      case STRUCT: {
        final PrologStruct struct = (PrologStruct) term;

        if (struct.isBlock()) {
          return convert(context, struct.getTermAt(0), vars);
        } else {
          final TermStruct result;
          final int arity = struct.getArity();
          if (arity == 0) {
            result = newStruct(convert(context, struct.getFunctor(), vars));
          } else {
            final Term[] terms = new Term[arity];
            for (int i = 0; i < arity; i++) {
              terms[i] = convert(context, struct.getTermAt(i), vars);
            }
            result = newStruct(convert(context, struct.getFunctor(), vars), terms);
          }
          result.setPredicateProcessor(context.findProcessor(result));
          return result;
        }
      }
      case VAR: {
        if (((PrologVar) term).isAnonymous()) {
          return newVar();
        } else {
          return vars.computeIfAbsent(term.getText(), Terms::newVar);
        }
      }
      default:
        throw new IllegalArgumentException("Unexpected parsed prolog term: " + term);
    }
  }

  public static Term fromParsed(final ProlContext context, final PrologTerm term) {
    if (term instanceof PrologAtom || term instanceof SpecServiceCompound) {
      return convert(context, term, null);
    } else {
      try {
        return convert(context, term, new HashMap<>());
      } catch (RuntimeException ex) {
        throw ex;
      }
    }
  }

}
