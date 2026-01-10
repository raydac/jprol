package com.igormaznitsa.jprol.data;

import static com.igormaznitsa.jprol.data.TermList.NULL_LIST;
import static com.igormaznitsa.jprol.data.TermStruct.EMPTY_ARRAY;

import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.PredicateInvoker;
import com.igormaznitsa.jprol.utils.ProlUtils;
import com.igormaznitsa.jprol.utils.lazy.LazyMap;
import com.igormaznitsa.prologparser.terms.PrologFloat;
import com.igormaznitsa.prologparser.terms.PrologInt;
import com.igormaznitsa.prologparser.terms.PrologList;
import com.igormaznitsa.prologparser.terms.PrologNumeric;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.PrologVar;
import com.igormaznitsa.prologparser.terms.Quotation;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.tokenizer.Op;
import java.util.List;
import java.util.Map;

public final class Terms {
  public static final Term EMPTY_ATOM = new Term("", SourcePosition.UNKNOWN);
  public static final Term EMPTY_LIST_ATOM = new Term("[]", SourcePosition.UNKNOWN);
  public static final Term TRUE = new Term("true", SourcePosition.UNKNOWN);
  public static final Term FALSE = new Term("false", SourcePosition.UNKNOWN);
  public static final Term LIST_FUNCTOR = new Term(".", SourcePosition.UNKNOWN);

  private static final long MIN_CACHED = -16L;
  private static final long MAX_CACHED = 16L;

  private static final TermLong[] CACHED_LONGS = new TermLong[(int) (MAX_CACHED - MIN_CACHED + 1)];

  static {
    for (int i = 0; i < CACHED_LONGS.length; i++) {
      CACHED_LONGS[i] = new TermLong(i + MIN_CACHED, SourcePosition.UNKNOWN);
    }
  }

  public static Term newAtom(final String text) {
    return newAtom(text, SourcePosition.UNKNOWN);
  }

  public static Term newAtom(final String text,
                             final SourcePosition sourcePosition) {
    if (".".equals(text) && sourcePosition.isUnknown()) {
      return LIST_FUNCTOR;
    } else {
      if (sourcePosition == SourcePosition.UNKNOWN) {
        if ("false".equals(text)) {
          return FALSE;
        }
        if ("true".equals(text)) {
          return TRUE;
        }
      }
      return new Term(text, sourcePosition);
    }
  }

  public static TermDouble newDouble(final String text,
                                     final SourcePosition sourcePosition) {
    return new TermDouble(text, sourcePosition);
  }

  public static TermDouble newDouble(final String text) {
    return new TermDouble(text, SourcePosition.UNKNOWN);
  }

  public static TermDouble newDouble(final double value) {
    return new TermDouble(value, SourcePosition.UNKNOWN);
  }

  public static TermDouble newDouble(final double value, final SourcePosition sourcePosition) {
    return new TermDouble(value, sourcePosition);
  }

  public static TermLong newLong(final String text, final SourcePosition sourcePosition) {
    return new TermLong(text, sourcePosition);
  }

  public static TermLong newLong(final String text) {
    return new TermLong(text, SourcePosition.UNKNOWN);
  }

  public static TermLong newLong(final long value) {
    return newLong(value, SourcePosition.UNKNOWN);
  }

  public static TermLong newLong(final long value,
                                 final SourcePosition sourcePosition) {
    TermLong result;
    if (sourcePosition.isUnknown()) {
      if (value >= MIN_CACHED && value <= MAX_CACHED) {
        return CACHED_LONGS[(int) value - (int) MIN_CACHED];
      }
      return new TermLong(value, sourcePosition);
    } else {
      result = new TermLong(value, sourcePosition);
    }
    return result;
  }

  public static TermList newList() {
    return NULL_LIST;
  }

  public static TermList newList(final Term term) {
    return new TermList(term, term.getSourcePosition());
  }

  public static TermList newList(final SourcePosition sourcePosition) {
    if (sourcePosition == null || sourcePosition == SourcePosition.UNKNOWN) {
      return NULL_LIST;
    } else {
      return new TermList(sourcePosition);
    }
  }

  public static TermList newList(final Term term, final SourcePosition sourcePosition) {
    return new TermList(term, sourcePosition);
  }

  public static TermList newList(final Term head, final Term tail) {
    return newList(head, tail, SourcePosition.UNKNOWN);
  }

  public static TermList newList(final Term head,
                                 final Term tail,
                                 final SourcePosition sourcePosition) {
    return new TermList(head, tail, sourcePosition);
  }

  public static TermVar newVar(final String name, final SourcePosition sourcePosition) {
    return new TermVar(name, sourcePosition);
  }

  public static TermVar newVar(final String name) {
    return new TermVar(name, SourcePosition.UNKNOWN);
  }

  public static TermVar newAnonymousVar(final SourcePosition sourcePosition) {
    return new TermVar((Term) null, sourcePosition);
  }

  public static TermVar newAnonymousVar() {
    return new TermVar((Term) null, SourcePosition.UNKNOWN);
  }

  public static TermStruct newStruct(final Term functor) {
    return new TermStruct(functor, functor.getSourcePosition());
  }

  public static TermStruct newStruct(final Term functor, final SourcePosition sourcePosition) {
    return new TermStruct(functor, sourcePosition);
  }

  public static TermStruct newStruct(final String functor,
                                     final Term[] elements,
                                     final SourcePosition sourcePosition) {
    return new TermStruct(functor, elements, sourcePosition);
  }

  public static TermStruct newStruct(final Term functor, final Term... elements) {
    return new TermStruct(functor, elements, functor.getSourcePosition());
  }

  public static TermStruct newStruct(final Term functor,
                                     final Term[] elements,
                                     final PredicateInvoker processor
  ) {
    return new TermStruct(functor, elements, functor.getSourcePosition(), processor);
  }

  public static TermStruct newStruct(final Term functor,
                                     final Term[] elements,
                                     final SourcePosition sourcePosition
  ) {
    return new TermStruct(functor, elements, sourcePosition);
  }

  public static TermStruct newStruct(final Term functor,
                                     final List<Term> elements,
                                     final SourcePosition sourcePosition
  ) {
    return new TermStruct(functor, elements.toArray(EMPTY_ARRAY), sourcePosition);
  }

  public static TermStruct newStruct(final Term functor,
                                     final Term[] elements,
                                     final PredicateInvoker processor,
                                     final SourcePosition sourcePosition) {
    return new TermStruct(functor, elements, sourcePosition, processor);
  }


  private static Term convert(final JProlContext context, final PrologTerm term,
                              final Map<String, TermVar> vars) {
    final SourcePosition sourcePosition = SourcePosition.positionOf(term.getLine(), term.getPos());

    switch (term.getType()) {
      case ATOM: {
        if (term instanceof PrologNumeric) {
          if (term instanceof PrologFloat) {
            return newDouble(((PrologFloat) term).getFloatValue().doubleValue(), sourcePosition);
          } else {
            return newLong(((PrologInt) term).getIntValue().longValue(), sourcePosition);
          }
        } else {
          if (term.getQuotation() == Quotation.DOUBLE) {
            return ProlUtils.toCharCodeList(term.getText(), sourcePosition);
          } else {
            final String text = term.getText();
            if (context.hasZeroArityPredicateForName(text)) {
              final TermStruct result = newStruct(text, EMPTY_ARRAY, sourcePosition);
              result.setPredicateProcessor(context.findProcessor(result));
              return result;
            } else {
              return newAtom(text, sourcePosition);
            }
          }
        }
      }
      case OPERATOR: {
        return context.getKnowledgeBase().findOperatorForName(context, term.getText())
            .getForExactType(((Op) term).getAssoc());
      }
      case LIST: {
        final PrologList list = (PrologList) term;
        return list.isEmpty() ? NULL_LIST :
            newList(convert(context, list.getHead(), vars), convert(context, list.getTail(), vars));
      }
      case STRUCT: {
        final PrologStruct struct = (PrologStruct) term;

        if (struct.isBlock()) {
          return convert(context, struct.getTermAt(0), vars);
        } else {
          final TermStruct result;
          final int arity = struct.getArity();
          if (arity == 0) {
            result = newStruct(convert(context, struct.getFunctor(), vars), sourcePosition);
          } else {
            final PrologTerm functor = struct.getFunctor();
            if (arity == 2 && functor.getType() == TermType.ATOM && functor.getText().equals(".")) {
              // list
              result = Terms.newList(convert(context, struct.getTermAt(0), vars),
                  convert(context, struct.getTermAt(1), vars), sourcePosition);
            } else {
              final Term[] terms = new Term[arity];
              for (int i = 0; i < arity; i++) {
                terms[i] = convert(context, struct.getTermAt(i), vars);
              }
              result =
                  newStruct(convert(context, struct.getFunctor(), vars), terms, sourcePosition);
            }
          }
          result.setPredicateProcessor(context.findProcessor(result));
          return result;
        }
      }
      case VAR: {
        if (((PrologVar) term).isAnonymous()) {
          return newAnonymousVar(sourcePosition);
        } else {
          return vars.computeIfAbsent(term.getText(), name -> new TermVar(name, sourcePosition));
        }
      }
      default:
        throw new IllegalArgumentException("Unexpected parsed prolog term: " + term);
    }
  }

  public static Term fromParsed(final JProlContext context, final PrologTerm term) {
    if (term.getType() == com.igormaznitsa.prologparser.terms.TermType.ATOM
        || term.getType() == com.igormaznitsa.prologparser.terms.TermType.OPERATOR) {
      return convert(context, term, null);
    } else {
      return convert(context, term, new LazyMap<>());
    }
  }

  public static NumericTerm newNumeric(final String string) {
    try {
      return Terms.newLong(Long.parseLong(string));
    } catch (NumberFormatException ex) {
      return Terms.newDouble(Double.parseDouble(string));
    }
  }
}
