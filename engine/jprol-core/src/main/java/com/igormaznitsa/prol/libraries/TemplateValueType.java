package com.igormaznitsa.prol.libraries;

import com.igormaznitsa.prol.data.*;
import com.igormaznitsa.prol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.prol.exceptions.ProlInstantiationErrorException;

import java.util.Collections;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.function.Consumer;

import static com.igormaznitsa.prol.data.TermType.STRUCT;
import static com.igormaznitsa.prol.data.TermType.VAR;

public enum TemplateValueType {
  ATOM(t -> {
    boolean error = true;
    switch (t.getTermType()) {
      case LIST: {
        error = !((TermList) t).isNullList();
      }
      break;
      case ATOM: {
        error = t instanceof NumericTerm;
      }
      break;
    }
    if (error) {
      throw new ProlInstantiationErrorException("Should be atom \'" + t + '\'', t);
    }
  }),
  ATOM_OR_ATOM_LIST(t -> {
    boolean error = true;
    switch (t.getTermType()) {
      case ATOM: {
        if (!(t instanceof NumericTerm)) {
          error = false;
        }
      }
      break;
      case LIST: {
        TermList lst = (TermList) t;
        error = false;
        if (lst.isNullList()) {
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
          if (head.getTermType() != TermType.ATOM) {
            error = true;
            break;
          }

          final Term tail = lst.getTail();
          if (tail == Terms.NULL_LIST) {
            break;
          }
          if (tail.getTermType() == TermType.LIST) {
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
      throw new ProlInstantiationErrorException("Should be atom or atom list \'" + t + '\'', t);
    }
  }),
  ATOMIC(t -> {
    boolean errorresult = false;
    switch (t.getTermType()) {
      case LIST: {
        errorresult = !((TermList) t).isNullList();
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
      throw new ProlInstantiationErrorException("Should be atomic \'" + t + '\'', t);
    }
  }),
  BYTE(t -> {
    boolean error = true;
    if (t instanceof TermLong) {
      final int value = t.toNumber().intValue();
      if ((value & 0xFF) == 0) {
        error = false;
      }
    }
    if (error) {
      throw new ProlInstantiationErrorException("Should be byte \'" + t + '\'', t);
    }
  }),
  CALLABLE_TERM(t -> {
    boolean error = true;

    final TermType typeAtom = t.getTermType();
    if (typeAtom == TermType.ATOM) {
      if (!(t instanceof NumericTerm)) {
        error = false;
      }
    } else if (typeAtom == STRUCT) {
      error = false;
    }
    if (error) {
      throw new ProlInstantiationErrorException("Should be callable term \'" + t + '\'', t);
    }
  }),
  CHARACTER(t -> {
    boolean error = true;

    if (t.getTermType() == TermType.ATOM && t.getText().length() == 1) {
      error = false;
    }

    if (error) {
      throw new ProlInstantiationErrorException("Should be character \'" + t + '\'', t);
    }
  }),
  CHARACTER_CODE(t -> {
    boolean error = true;
    if (t instanceof TermLong) {
      final int value = t.toNumber().intValue();
      if ((value & 0xFFFF0000) == 0) {
        error = false;
      }
    }
    if (error) {
      throw new ProlInstantiationErrorException("Should be character code \'" + t + '\'', t);
    }
  }),
  CHARACTER_CODE_LIST(t -> {
    boolean error;
    if (t.getTermType() == TermType.LIST) {
      TermList lst = (TermList) t;
      error = false;
      if (!lst.isNullList()) {
        while (!Thread.currentThread().isInterrupted()) {
          Term head = lst.getHead();
          if (head.getTermType() == TermType.VAR) {
            head = ((Var) head).getValue();
            if (head == null) {
              error = true;
              break;
            }
          }
          if (head.getTermType() == TermType.ATOM) {
            if (head instanceof TermLong) {
              if ((head.toNumber().intValue() & 0xFFFF0000) != 0) {
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
          if (tail == Terms.NULL_LIST) {
            break;
          }
          if (tail.getTermType() == TermType.LIST) {
            lst = (TermList) tail;
          } else {
            error = true;
            break;
          }
        }
      }
    } else {
      error = true;
    }
    if (error) {
      throw new ProlInstantiationErrorException("Should be character code list \'" + t + '\'', t);
    }
  }),
  CHARACTER_LIST(t -> {
    boolean error;
    if (t.getTermType() == TermType.LIST) {
      TermList lst = (TermList) t;
      error = false;
      if (!lst.isNullList()) {

        while (!Thread.currentThread().isInterrupted()) {
          Term head = lst.getHead();
          if (head.getTermType() == TermType.VAR) {
            head = ((Var) head).getValue();
            if (head == null) {
              error = true;
              break;
            }
          }
          if (head.getTermType() == TermType.ATOM) {
            if (head.getText().length() != 1) {
              error = true;
              break;
            }
          } else {
            error = true;
            break;
          }

          final Term tail = lst.getTail();
          if (tail == Terms.NULL_LIST) {
            break;
          }
          if (tail.getTermType() == TermType.LIST) {
            lst = (TermList) tail;
          } else {
            error = true;
            break;
          }
        }
      }
    } else {
      error = true;
    }
    if (error) {
      throw new ProlInstantiationErrorException("Should be character code list \'" + t + '\'', t);
    }
  }),
  CLAUSE(t -> {
    boolean error = false;
    switch (t.getTermType()) {
      case ATOM: {
        if (t instanceof NumericTerm) {
          error = true;
        }
      }
      break;
      case STRUCT: {
        final TermStruct struct = (TermStruct) t;
        final Term functor = struct.getFunctor();

        final TermType functorType = functor.getTermType();

        // check left part
        if (struct.isClause()) {
          final Term left = struct.getElement(0);
          switch (left.getTermType()) {
            case ATOM: {
              if (left instanceof NumericTerm) {
                error = true;
              }
            }
            break;
            case LIST:
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
            case LIST:
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
      throw new ProlInstantiationErrorException("Should be clause or atom \'" + t + '\'', t);
    }
  }),
  COMPOUND_TERM(t -> {
    switch (t.getTermType()) {
      case LIST:
      case STRUCT: {
      }
      break;
      default:
        throw new ProlInstantiationErrorException("Should be compound term \'" + t + '\'', t);
    }
  }),
  EVALUABLE(t -> {
    boolean error = true;
    if (t instanceof NumericTerm) {
      error = false;
    } else {
      if (t.getTermType() == STRUCT) {
        final TermStruct struct = (TermStruct) t;
        final PredicateProcessor processor = struct.getPredicateProcessor();
        if (processor.isEvaluable()) {
          error = false;
        }
      }
    }
    if (error) {
      throw new ProlInstantiationErrorException("Should be evaluable \'" + t + '\'', t);
    }
  }),
  HEAD(t -> {
    boolean error = true;
    switch (t.getTermType()) {
      case ATOM: {
        if (!(t instanceof NumericTerm)) {
          error = false;
        }
      }
      break;
      case STRUCT: {
        final Term functor = ((TermStruct) t).getFunctor();
        if (functor.getTermType() == TermType.ATOM) {
          error = false;
        }
      }
      break;
    }
    if (error) {
      throw new ProlInstantiationErrorException("Imcompatible clause head", t);
    }
  }),
  IN_BYTE(t -> {
    boolean error = false;
    if (t instanceof TermLong) {
      final int val = t.toNumber().intValue();
      if ((val & 0xFF) != 0 && val == -1) {
        error = true;
      }
    } else {
      error = true;
    }
    if (error) {
      throw new ProlInstantiationErrorException("Should be byte or -1 \'" + t + '\'', t);
    }
  }),
  IN_CHARACTER(t -> {
    boolean error = false;
    if (t.getTermType() == TermType.ATOM) {
      final String text = t.getText();
      if (text.length() != 1 && !"end_of_file".equals(text)) {
        error = true;
      }
    } else {
      error = true;
    }
    if (error) {
      throw new ProlInstantiationErrorException("Should be character code or -1 \'" + t + '\'', t);
    }
  }),
  IN_CHARACTER_CODE(t -> {
    boolean error = false;
    if (t instanceof TermLong) {
      final int val = t.toNumber().intValue();
      if ((val & 0xFFFF0000) != 0 && val != -1) {
        error = true;
      }
    } else {
      error = true;
    }
    if (error) {
      throw new ProlInstantiationErrorException("Should be character code or -1 \'" + t + '\'', t);
    }
  }),
  INTEGER(t -> {
    if (!(t instanceof TermLong)) {
      throw new ProlInstantiationErrorException("Should be integer \'" + t + '\'', t);
    }
  }),
  IO_MODE(t -> {
    boolean error = true;
    if (t.getTermType() == TermType.ATOM) {
      final String text = t.getText();
      if (text.equals("read") || text.equals("write") || text.equals("append")) {
        error = false;
      }
    }
    if (error) {
      throw new ProlInstantiationErrorException("Should be 'read', 'write' or 'append' [" + t + ']', t);
    }
  }),
  LIST(t -> {
    if (t.getTermType() != TermType.LIST) {
      throw new ProlInstantiationErrorException("Should be list \'" + t + '\'', t);
    }
  }),
  NONVAR(t -> {
    if (t.getTermType() == VAR) {
      throw new ProlInstantiationErrorException("Should be nonvar \'" + t + '\'', t);
    }
  }),
  NUMBER(t -> {
    if (!(t instanceof NumericTerm)) {
      throw new ProlInstantiationErrorException("Should be number \'" + t + '\'', t);
    }
  }),
  OPERATOR_SPECIFIER(t -> {
    boolean error;
    if (t.getTermType() == TermType.ATOM && !(t instanceof NumericTerm)) {
      final String text = t.getText();
      error = Operator.getTypeFromString(text) < 0;
    } else {
      error = true;
    }
    if (error) {
      throw new ProlDomainErrorException("Should be only [xfx,yfx,xfy,xf,fx,yf,fy] but \'" + t + '\'', t);
    }
  }),
  PREDICATE_INDICATOR(t -> {
    boolean error = true;
    if (t.getTermType() == STRUCT) {
      final TermStruct struct = (TermStruct) t;
      if (struct.getArity() == 2 && "/".equals(struct.getFunctor().getText())) {
        final Term left = struct.getElement(0);
        final Term right = struct.getElement(1);

        final boolean leftIsCorrect = (left.getTermType() == TermType.ATOM && left.getClass() == Term.class)
            || (left.getTermType() == STRUCT && ((TermStruct) left).getArity() == 0)
            || (left.getTermType() == VAR && !left.isGround());

        final boolean rightIsCorrect = (right.getTermType() == TermType.ATOM && right.getClass() == TermLong.class)
            || (right.getTermType() == VAR && !right.isGround());

        error = !(leftIsCorrect && rightIsCorrect);
      }
    }

    if (error) {
      throw new ProlInstantiationErrorException("Should be predicate indicator \'" + t + '\'', t);
    }
  }),
  TERM(t -> {
  }),
  NON_EMPTY_LIST(t -> {
    if (t.getTermType() != TermType.LIST) {
      throw new ProlInstantiationErrorException("Should be list \'" + t + '\'', t);
    } else {
      if (t == Terms.NULL_LIST) {
        throw new ProlInstantiationErrorException("Should not be empty list \'" + t + '\'', t);
      }
    }
  }),
  TRIGGEREVENT(t -> {
    if (t.getTermType() != TermType.ATOM) {
      throw new ProlInstantiationErrorException("Should be an atom \'" + t + '\'', t);
    } else {
      final String value = t.getText();
      if (!"onassert".equals(value) && !"onretract".equals(value) && !"onassertretract".equals(value)) {
        throw new ProlDomainErrorException("Should be a value from the list [onassert, onretract, onassertretract] \'" + t + '\'', t);
      }
    }
  });

  private static final Map<String, TemplateValueType> map;

  static {
    final Map<String, TemplateValueType> temp = new HashMap<>();
    for (final TemplateValueType t : TemplateValueType.values()) {
      temp.put(t.name().toLowerCase(Locale.ENGLISH), t);
    }
    map = Collections.unmodifiableMap(temp);
  }

  private final Consumer<Term> checker;

  TemplateValueType(final Consumer<Term> checker) {
    this.checker = checker;
  }

  public static TemplateValueType findForName(final String name) {
    return map.get(name);
  }

  public void check(final Term t) {
    this.checker.accept(t);
  }

}
