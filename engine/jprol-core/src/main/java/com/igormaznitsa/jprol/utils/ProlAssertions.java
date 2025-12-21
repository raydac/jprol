package com.igormaznitsa.jprol.utils;

import static com.igormaznitsa.jprol.data.TermType.STRUCT;
import static com.igormaznitsa.jprol.data.TermType.VAR;

import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermDouble;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlRepresentationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.logic.PredicateInvoker;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;

public final class ProlAssertions {
  private ProlAssertions() {
  }

  public static void assertStruct(final Term term) {
    assertNonVar(term);
    if (term.getTermType() != STRUCT) {
      throw new ProlTypeErrorException("struct", "Expected struct: " + term, term);
    }
  }

  public static void assertArity(final Term term) {
    assertNonVar(term);
    if (term instanceof TermLong) {
      if (term.toNumber().longValue() < 0) {
        throw new ProlDomainErrorException("integer", "Expected zero or positive: " + term, term);
      }
    } else {
      throw new ProlTypeErrorException("integer", "Expected arity value: " + term, term);
    }
  }

  public static boolean isAtom(final Term t) {
    final boolean result;
    switch (t.getTermType()) {
      case LIST: {
        result = t.isNullList();
      }
      break;
      case ATOM: {
        result = !(t instanceof NumericTerm);
      }
      break;
      default:
        result = false;
        break;
    }
    return result;
  }

  public static void assertAtom(final Term t) {
    assertNonVar(t);
    if (!isAtom(t)) {
      throw new ProlTypeErrorException("atom", "Atom expected: " + t, t);
    }
  }

  public static void assertAtomOrAtomList(final Term t) {
    assertNonVar(t);
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
        if (lst.isNullList()) {
          break;
        }

        error = false;
        while (true) {
          Term head = lst.getHead();
          if (head.getTermType() == VAR) {
            head = ((TermVar) head).getValue();
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
          if (tail.isNullList()) {
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
      throw new ProlInstantiationErrorException("Should be atom or atom list '" + t + '\'', t);
    }
  }

  public static void assertAtomic(final Term t) {
    assertNonVar(t);
    boolean errorType = false;
    switch (t.getTermType()) {
      case LIST: {
        errorType = !t.isNullList();
      }
      break;
      case ATOM: {
      }
      break;
      default: {
        errorType = true;
      }
      break;
    }

    if (errorType) {
      throw new ProlTypeErrorException("atomic", "Atomic type expected: " + t, t);
    }
  }

  public static void assertByte(final Term t) {
    assertNonVar(t);
    boolean error = true;
    if (t instanceof TermLong) {
      final int value = t.toNumber().intValue();
      if ((value & 0xFF) == 0) {
        error = false;
      }
    }
    if (error) {
      throw new ProlInstantiationErrorException("Should be byte '" + t + '\'', t);
    }
  }

  public static void assertCallable(final Term t) {
    final boolean nonCallable;
    switch (t.getTermType()) {
      case STRUCT: {
        nonCallable = false;
      }
      break;
      case ATOM: {
        nonCallable = t instanceof NumericTerm;
      }
      break;
      case VAR: {
        Term value = t.findNonVarOrSame();
        if (value.getTermType() == VAR) {
          throw new ProlInstantiationErrorException("Expected instantiated callable: " + t, t);
        } else {
          assertCallable(value);
        }
      }
      default:
        nonCallable = true;
        break;
    }
    if (nonCallable) {
      throw new ProlTypeErrorException("callable", "Callable term expected: " + t, t);
    }
  }

  public static void assertCharacter(final Term t) {
    assertNonVar(t);
    boolean error = t.getTermType() != TermType.ATOM || t.getText().length() != 1;
    if (error) {
      throw new ProlTypeErrorException("'character' expected, found an atom: " + t, t);
    }
  }

  public static void assertCharacterCode(final Term t) {
    assertNonVar(t);
    if (t instanceof TermLong) {
      final int value = t.toNumber().intValue();
      if ((value & 0xFFFF0000) != 0) {
        throw new ProlRepresentationErrorException("character_code",
            "Incompatible character code: " + t, t);
      }
    } else {
      throw new ProlTypeErrorException("character", "Char code expected: " + t, t);
    }
  }

  public static void assertCharacterCodeList(final Term t) {
    int errorCode = 0;
    if (t.getTermType() == TermType.LIST) {
      TermList lst = (TermList) t;
      while (!lst.isNullList() && errorCode == 0) {
        final Term value = lst.getHead().findNonVarOrSame();
        if (value.getTermType() == TermType.ATOM) {
          if (value instanceof TermLong) {
            if ((value.toNumber().intValue() & 0xFFFF0000) != 0) {
              errorCode = 2;
            }
          } else {
            errorCode = 2;
          }
        } else {
          errorCode = value.getTermType() == VAR ? 3 : 1;
        }
        final Term tail = lst.getTail().findNonVarOrSame();
        if (tail.getTermType() == TermType.LIST) {
          lst = (TermList) tail;
        } else {
          errorCode = tail.getTermType() == VAR ? 0 : 1;
          break;
        }
      }
    } else if (t.getTermType() == VAR) {
      errorCode = 3;
    } else {
      errorCode = 1;
    }
    switch (errorCode) {
      case 1:
        throw new ProlTypeErrorException("character_code_list",
            "Character code list expected: " + t, t);
      case 2:
        throw new ProlRepresentationErrorException("character_code",
            "Incompatible character code in list: " + t, t);
      case 3:
        throw new ProlInstantiationErrorException("Must be instantiated: " + t, t);
    }
  }

  public static void assertCharacterList(final Term t) {
    int errorCode = 0;
    if (t.getTermType() == VAR) {
      errorCode = 3;
    } else if (t.getTermType() == TermType.LIST) {
      TermList lst = (TermList) t;
      while (!lst.isNullList() && errorCode == 0) {
        final Term value = lst.getHead().findNonVarOrSame();
        if (value.getTermType() == TermType.ATOM) {
          if (value instanceof NumericTerm) {
            errorCode = 1;
          } else {
            if (value.getText().length() != 1) {
              errorCode = 2;
            }
          }
        } else {
          errorCode = value.getTermType() == VAR ? 3 : 1;
        }
        final Term tail = lst.getTail().findNonVarOrSame();
        if (tail.getTermType() == TermType.LIST) {
          lst = (TermList) tail;
        } else {
          errorCode = tail.getTermType() == VAR ? 0 : 1;
          break;
        }
      }
    } else {
      errorCode = 1;
    }
    switch (errorCode) {
      case 1:
        throw new ProlTypeErrorException("character_list", "Character list expected: " + t, t);
      case 2:
        throw new ProlRepresentationErrorException("character", "Character expected: " + t, t);
      case 3:
        throw new ProlInstantiationErrorException("Must be instantiated: " + t, t);
    }
  }

  public static void assertCompoundTerm(final Term t) {
    switch (t.getTermType()) {
      case LIST:
      case STRUCT: {
      }
      break;
      case VAR: {
        throw new ProlInstantiationErrorException("Expected instantiated compound term: " + t, t);
      }
      default:
        throw new ProlTypeErrorException("compound", "Expected compound: " + t, t);
    }
  }

  public static void assertEvaluable(final Term t) {
    assertNonVar(t);
    boolean error = true;
    if (t instanceof NumericTerm) {
      error = false;
    } else {
      if (t.getTermType() == STRUCT) {
        final TermStruct struct = (TermStruct) t;
        final PredicateInvoker processor = struct.getPredicateProcessor();
        if (processor != null && processor.isEvaluable()) {
          error = false;
        }
      }
    }
    if (error) {
      throw new ProlTypeErrorException("evaluable", "Expected evaluable: " + t, t);
    }
  }

  public static void assertHead(final Term t) {
    assertNonVar(t);
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
      throw new ProlTypeErrorException("Unexpected head: " + t, t);
    }
  }

  public static void assertInByte(final Term t) {
    assertNonVar(t);
    boolean error = false;
    if (t instanceof TermLong) {
      final int val = t.toNumber().intValue();
      if (val == -1) {
        error = true;
      }
    } else {
      error = true;
    }
    if (error) {
      throw new ProlInstantiationErrorException("Should be byte or -1 '" + t + '\'', t);
    }
  }

  public static void assertInCharacter(final Term t) {
    assertNonVar(t);
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
      throw new ProlInstantiationErrorException("Should be character code or -1 '" + t + '\'', t);
    }
  }

  public static void assertInCharacterCode(final Term t) {
    assertNonVar(t);
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
      throw new ProlInstantiationErrorException("Should be character code or -1 '" + t + '\'', t);
    }
  }

  public static TermLong assertInteger(final Term t) {
    assertNonVar(t);
    if (!(t instanceof TermLong)) {
      throw new ProlTypeErrorException("integer", "Integer expected: " + t, t);
    }
    return (TermLong) t;
  }

  public static void assertIoMode(final Term t) {
    assertNonVar(t);
    boolean error = true;
    if (t.getTermType() == TermType.ATOM) {
      final String text = t.getText();
      if (text.equals("read") || text.equals("write") || text.equals("append")) {
        error = false;
      }
    }
    if (error) {
      throw new ProlDomainErrorException("[read,write,append]", "Wrong io mode: " + t, t);
    }
  }

  public static TermList assertList(final Term t) {
    assertNonVar(t);
    if (t.getTermType() != TermType.LIST) {
      throw new ProlTypeErrorException("list", "List expected: " + t, t);
    }
    return (TermList) t;
  }

  public static TermVar assertVar(final Term t) {
    if (t.getTermType() != VAR) {
      throw new ProlTypeErrorException("var", "Unbound variable expected: " + t, t);
    }
    return (TermVar) t;
  }

  public static void assertNonVar(final Term t) {
    if (t.getTermType() == VAR) {
      throw new ProlInstantiationErrorException("Grounded value expected: " + t, t);
    }
  }

  public static NumericTerm assertNumber(final Term t) {
    assertNonVar(t);
    if (!(t instanceof NumericTerm)) {
      throw new ProlTypeErrorException("number", "Number expected: " + t, t);
    }
    return (NumericTerm) t;
  }

  public static void assertOperatorSpecifier(final Term t) {
    assertNonVar(t);
    boolean error;
    if (t.getTermType() == TermType.ATOM && !(t instanceof NumericTerm)) {
      final String text = t.getText();
      error = !OpAssoc.findForName(text).isPresent(); // don't change for Android API compatibility
    } else {
      error = true;
    }
    if (error) {
      throw new ProlDomainErrorException(
          "Should be only [xfx,yfx,xfy,xf,fx,yf,fy] but '" + t + '\'', t);
    }
  }

  public static TermStruct assertIndicator(final Term t) {
    assertNonVar(t);
    int errorCode = 1;
    if (t.getTermType() == STRUCT) {
      final TermStruct struct = (TermStruct) t;
      if (struct.getArity() == 2 && "/".equals(struct.getFunctor().getText())) {
        final Term left = struct.getElement(0).findNonVarOrSame();
        final Term right = struct.getElement(1).findNonVarOrSame();

        final boolean leftOk =
            (left.getTermType() == TermType.ATOM && left.getClass() == Term.class)
                || (left.getTermType() == STRUCT && ((TermStruct) left).getArity() == 0)
                || (left.getTermType() == VAR && !left.isGround());

        final boolean rightOk =
            (right.getTermType() == TermType.ATOM && right.getClass() == TermLong.class)
                || (right.getTermType() == VAR && !right.isGround());

        if (leftOk && rightOk) {
          if (right instanceof NumericTerm) {
            if (right instanceof TermDouble || right.toNumber().longValue() < 0L) {
              errorCode = 2;
            } else if (right.toNumber().longValue() > Integer.MAX_VALUE) {
              errorCode = 3;
            } else {
              errorCode = 0;
            }
          } else {
            errorCode = 0;
          }
        }
      }
    }

    switch (errorCode) {
      case 1:
        throw new ProlTypeErrorException("predicate_indicator",
            "Predicate indicator expected: " + t, t);
      case 2:
        throw new ProlDomainErrorException("integer", "Predicate indicator expected: " + t, t);
      case 3:
        throw new ProlRepresentationErrorException("max_arity", "Wrong arity: " + t, t);
    }

    return (TermStruct) t;
  }

  public static void assertNonEmptyList(final Term t) {
    assertNonVar(t);
    if (t.getTermType() != TermType.LIST) {
      throw new ProlTypeErrorException("list", "Expected list: " + t, t);
    } else {
      if (t.isNullList()) {
        throw new ProlDomainErrorException("[]", "Expected non-empty list: " + t, t);
      }
    }
  }

  @SuppressWarnings("SpellCheckingInspection")
  public void assertTriggerEvent(final Term t) {
    assertNonVar(t);
    if (t.getTermType() != TermType.ATOM) {
      throw new ProlInstantiationErrorException("Should be an atom '" + t + '\'', t);
    } else {
      final String value = t.getText();
      if (!"onassert".equals(value) && !"onretract".equals(value) &&
          !"onassertretract".equals(value)) {
        throw new ProlDomainErrorException(
            "Should be a value from the list [onassert, onretract, onassertretract] '" + t + '\'',
            t);
      }
    }
  }
}
