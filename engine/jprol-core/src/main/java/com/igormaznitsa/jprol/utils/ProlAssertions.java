package com.igormaznitsa.jprol.utils;

import static com.igormaznitsa.jprol.data.TermType.STRUCT;
import static com.igormaznitsa.jprol.data.TermType.VAR;

import com.igormaznitsa.jprol.data.CompoundTerm;
import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermDouble;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.exceptions.ProlException;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlRepresentationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.logic.PredicateInvoker;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;

public final class ProlAssertions {
  private ProlAssertions() {
  }

  public static ProlException checkMeta(final Term term) {
    Term that = term.tryGround();
    final boolean flag =
        (that.getTermType() == TermType.ATOM && !(that instanceof NumericTerm)) ||
            that.getTermType() == TermType.STRUCT;
    if (flag) {
      return null;
    } else {
      if (that.getTermType() == TermType.VAR) {
        return new ProlInstantiationErrorException("Expected instantiated: " + that, that);
      } else {
        return new ProlTypeErrorException("ground", that);
      }
    }
  }

  public static Term assertMeta(final Term term) {
    final ProlException exception = checkMeta(term);
    if (exception == null) {
      return term;
    }
    throw exception;
  }

  public static Term assertNonVar(final Term term) {
    final ProlException exception = checkNonVar(term);
    if (exception == null) {
      return term;
    }
    throw exception;
  }

  public static ProlException checkNonVar(final Term term) {
    if (term.tryGround().getTermType() == VAR) {
      return new ProlInstantiationErrorException("Expected non-var value", term);
    } else {
      return null;
    }
  }

  public static Term assertGround(final Term term) {
    final ProlException exception = checkGround(term);
    if (exception == null) {
      return term;
    }
    throw exception;
  }

  public static ProlException checkGround(final Term term) {
    if (term.isGround()) {
      return null;
    }
    return new ProlInstantiationErrorException("Expected ground value", term);
  }

  public static Term assertUnbound(final Term term) {
    final ProlException exception = checkUnbound(term);
    if (exception == null) {
      return term;
    }
    throw exception;
  }

  public static ProlException checkUnbound(final Term term) {
    if (term.isUnground()) {
      return null;
    }
    return new ProlTypeErrorException("var", "Unbound variable expected: " + term, term);
  }

  public static TermStruct assertStruct(final Term term) {
    final ProlException exception = checkStruct(term);
    if (exception != null) {
      throw exception;
    }
    return term.tryGround();
  }

  public static ProlException checkStruct(final Term term) {
    final Term that = term.tryGround();
    if (that.getTermType() != STRUCT) {
      return new ProlTypeErrorException("struct", "Expected struct: " + term, term);
    }
    return null;
  }

  public static TermLong assertArity(final Term term) {
    final ProlException exception = checkArity(term);
    if (exception != null) {
      throw exception;
    }
    return term.tryGround();
  }

  public static ProlException checkArity(final Term term) {
    final Term that = term.tryGround();
    if (that instanceof TermLong) {
      final long value = that.toNumber().longValue();
      if (value < 0) {
        return new ProlDomainErrorException("integer", "Expected zero or positive: " + term, term);
      }
      if (value > Integer.MAX_VALUE) {
        return new ProlDomainErrorException("integer", "Too big arity: " + term, term);
      }
      return null;
    } else {
      return new ProlTypeErrorException("integer", "Expected arity value: " + term, term);
    }
  }

  public static boolean isAtom(final Term t) {
    if (t == null) {
      return false;
    }
    final Term term = t.tryGround();
    final boolean result;
    if (term.getTermType() == TermType.ATOM) {
      result = !(term instanceof NumericTerm);
    } else {
      result = false;
    }
    return result;
  }

  public static ProlException checkAtom(final Term term) {
    ProlException result = checkNonVar(term);
    if (result == null) {
      if (!isAtom(term)) {
        return new ProlTypeErrorException("atom", "Atom expected: " + term.toSrcString(), term);
      }
    }
    return result;
  }

  public static Term assertAtom(final Term term) {
    final ProlException exception = checkAtom(term);
    if (exception == null) {
      return term;
    }
    throw exception;
  }

  public static Term assertAtomOrAtomList(final Term term) {
    final ProlException exception = checkAtomOrAtomList(term);
    if (exception != null) {
      throw exception;
    }
    return term;
  }

  public static ProlException checkAtomOrAtomList(final Term term) {
    final Term that = term.tryGround();
    boolean error = true;
    switch (that.getTermType()) {
      case ATOM: {
        if (!(that instanceof NumericTerm)) {
          error = false;
        }
      }
      break;
      case LIST: {
        TermList lst = (TermList) that;
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
      return new ProlInstantiationErrorException("Should be atom or atom list '" + that + '\'',
          that);
    }
    return null;
  }

  public static Term assertAtomic(final Term term) {
    final ProlException exception = checkAtomic(term);
    if (exception != null) {
      throw exception;
    }
    return term;
  }

  public static ProlException checkAtomic(final Term term) {
    final Term t = term.tryGround();
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
      return new ProlTypeErrorException("atomic", "Atomic type expected: " + t, t);
    }
    return null;
  }

  public static TermLong assertByte(final Term term) {
    final ProlException exception = checkByte(term);
    if (exception != null) {
      throw exception;
    }
    return term.tryGround();
  }

  public static ProlException checkByte(final Term term) {
    final Term t = term.tryGround();
    boolean error = true;
    if (t instanceof TermLong) {
      final int value = t.toNumber().intValue();
      if ((value & 0xFF) == 0) {
        error = false;
      }
    }
    if (error) {
      return new ProlInstantiationErrorException("Should be byte '" + t + '\'', t);
    }
    return null;
  }

  public static Term assertName(final Term term) {
    final ProlException exception = checkName(term);
    if (exception != null) {
      throw exception;
    }
    return term;
  }

  public static ProlException checkName(final Term term) {
    final Term that = term.tryGround();
    if (that.isGround() && (that.isNullList() || !(that instanceof CompoundTerm))) {
      return null;
    }
    return new ProlTypeErrorException("name", that);
  }

  public static TermList assertString(final Term term) {
    final ProlException exception = checkString(term);
    if (exception != null) {
      throw exception;
    }
    return term.tryGround();
  }

  public static ProlException checkString(final Term term) {
    final Term that = term.tryGround();
    if (that.getTermType() == TermType.LIST) {
      final TermList termList = (TermList) that;
      if (termList.isNullList()) {
        return null;
      }
      if (!termList.doesContainOnlyCharCodes()) {
        return new ProlDomainErrorException("string",
            "List describing list must contain only char codes", term);
      }
      return null;
    } else {
      return new ProlTypeErrorException("list", "String must be a list of char codes", term);
    }
  }

  public static Term assertBody(final Term term) {
    final ProlException exception = checkBody(term);
    if (exception != null) {
      throw exception;
    }
    return term;
  }

  public static ProlException checkBody(final Term term) {
    final Term that = term.tryGround();
    if ((that.getTermType() == TermType.ATOM && !(that instanceof NumericTerm)) ||
        that.getTermType() == TermType.STRUCT) {
      return null;
    } else {
      throw new ProlTypeErrorException("body", "Expected body term: " + that, that);
    }
  }

  public static Term assertCallable(final Term term) {
    final ProlException exception = checkCallable(term);
    if (exception != null) {
      throw exception;
    }
    return term;
  }

  public static ProlException checkCallable(final Term term) {
    final Term t = term.tryGround();
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
        Term value = t.tryGround();
        if (value.getTermType() == VAR) {
          return new ProlInstantiationErrorException("Expected instantiated callable: " + t, t);
        } else {
          return checkCallable(value);
        }
      }
      default:
        nonCallable = true;
        break;
    }
    if (nonCallable) {
      return new ProlTypeErrorException("callable", "Callable term expected: " + t, t);
    }
    return null;
  }

  public static Term assertChar(final Term term) {
    final ProlException exception = checkChar(term);
    if (exception != null) {
      throw exception;
    }
    return term;
  }

  public static ProlException checkChar(final Term term) {
    final Term t = term.tryGround();
    boolean error = t.getTermType() != TermType.ATOM || t.getText().length() != 1;
    if (error) {
      return new ProlTypeErrorException("'character' expected, found an atom: " + t, t);
    }
    return null;
  }

  public static Term assertCharacterCode(final Term term) {
    final ProlException exception = checkCharCode(term);
    if (exception != null) {
      throw exception;
    }
    return term;
  }

  public static ProlException checkCharCode(final Term term) {
    final Term t = term.tryGround();
    if (t instanceof TermLong) {
      final int value = t.toNumber().intValue();
      if ((value & 0xFFFF0000) != 0) {
        return new ProlRepresentationErrorException("character_code",
            "Incompatible character code: " + t, t);
      }
    } else {
      return new ProlTypeErrorException("character", "Char code expected: " + t, t);
    }
    return null;
  }

  public static TermList assertCharCodeList(final Term term) {
    final ProlException exception = checkCharCodeList(term);
    if (exception != null) {
      throw exception;
    }
    return term.tryGround();
  }

  public static ProlException checkCharCodeList(final Term term) {
    final Term t = term.tryGround();
    int errorCode = 0;
    if (t.getTermType() == TermType.LIST) {
      TermList lst = (TermList) t;
      while (!lst.isNullList() && errorCode == 0) {
        final Term value = lst.getHead().tryGround();
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
        final Term tail = lst.getTail().tryGround();
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
        return new ProlTypeErrorException("character_code_list",
            "Character code list expected: " + t, t);
      case 2:
        return new ProlRepresentationErrorException("character_code",
            "Incompatible character code in list: " + t, t);
      case 3:
        return new ProlInstantiationErrorException("Must be instantiated: " + t, t);
      default:
        return null;
    }
  }

  public static TermList assertCharList(final Term term) {
    final ProlException exception = checkCharList(term);
    if (exception != null) {
      throw exception;
    }
    return term.tryGround();
  }

  public static ProlException checkCharList(final Term term) {
    final Term t = term.tryGround();
    int errorCode = 0;
    if (t.getTermType() == VAR) {
      errorCode = 3;
    } else if (t.getTermType() == TermType.LIST) {
      TermList lst = (TermList) t;
      while (!lst.isNullList() && errorCode == 0) {
        final Term value = lst.getHead().tryGround();
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
        final Term tail = lst.getTail().tryGround();
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
        return new ProlTypeErrorException("character_list", "Character list expected: " + t, t);
      case 2:
        return new ProlRepresentationErrorException("character", "Character expected: " + t, t);
      case 3:
        return new ProlInstantiationErrorException("Must be instantiated: " + t, t);
      default:
        return null;
    }
  }

  public static Term assertCompound(final Term t) {
    final ProlException exception = checkCompound(t);
    if (exception != null) {
      throw exception;
    }
    return t;
  }

  public static ProlException checkCompound(final Term t) {
    final Term that = t.tryGround();
    switch (that.getTermType()) {
      case LIST:
      case STRUCT: {
        return null;
      }
      case VAR: {
        return new ProlInstantiationErrorException("Expected instantiated compound term: " + that,
            that);
      }
      default:
        return new ProlTypeErrorException("compound", "Expected compound: " + that, that);
    }
  }

  public static Term assertEvaluable(final Term t) {
    final ProlException exception = checkEvaluable(t);
    if (exception != null) {
      throw exception;
    }
    return t.tryGround();
  }

  public static ProlException checkEvaluable(final Term term) {
    final Term t = term.tryGround();
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
      return new ProlTypeErrorException("evaluable", "Expected evaluable: " + t, t);
    }
    return null;
  }

  public static Term assertHead(final Term t) {
    final ProlException exception = checkHead(t);
    if (exception != null) {
      throw exception;
    }
    return t;
  }

  public static ProlException checkHead(final Term term) {
    final Term t = term.tryGround();
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
      return new ProlTypeErrorException("Unexpected head: " + t, t);
    }
    return null;
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

  public static ProlException checkInteger(final Term term) {
    final Term that = term.tryGround();
    if (!(that instanceof TermLong)) {
      return new ProlTypeErrorException("integer", "Integer expected: " + that, that);
    }
    return null;
  }

  public static TermLong assertInteger(final Term t) {
    final ProlException exception = checkInteger(t);
    if (exception == null) {
      return t.tryGround();
    }
    throw exception;
  }

  public static ProlException checkFloat(final Term term) {
    final Term that = term.tryGround();
    if (!(that instanceof TermDouble)) {
      return new ProlTypeErrorException("float", "Float expected: " + that, that);
    }
    return null;
  }

  public static TermDouble assertFloat(final Term t) {
    final ProlException exception = checkFloat(t);
    if (exception == null) {
      return t.tryGround();
    }
    throw exception;
  }

  public static ProlException checkNumber(final Term term) {
    final Term that = term.tryGround();
    if (!(that instanceof NumericTerm)) {
      return new ProlTypeErrorException("number", "Numeric term expected: " + that, that);
    }
    return null;
  }

  public static Term assertOperatorSpecifier(final Term term) {
    final ProlException exception = checkOperatorSpecifier(term);
    if (exception != null) {
      throw exception;
    }
    return term;
  }

  public static ProlException checkOperatorSpecifier(final Term term) {
    final Term that = term.tryGround();
    boolean error;
    if (that.getTermType() == TermType.ATOM && !(that instanceof NumericTerm)) {
      final String text = that.getText();
      error = !OpAssoc.findForName(text).isPresent(); // don't change for Android API compatibility
    } else {
      error = true;
    }
    if (error) {
      return new ProlDomainErrorException(
          "Should be only [xfx,yfx,xfy,xf,fx,yf,fy] but '" + that + '\'', that);
    } else {
      return null;
    }

  }

  public static NumericTerm assertNumber(final Term t) {
    final ProlException exception = checkNumber(t);
    if (exception == null) {
      return t.tryGround();
    }
    throw exception;
  }

  public static Term assertIoMode(final Term term) {
    final ProlException exception = checkIoMode(term);
    if (exception != null) {
      throw exception;
    }
    return term;
  }

  public static ProlException checkIoMode(final Term term) {
    final Term t = term.tryGround();
    boolean error = true;
    if (t.getTermType() == TermType.ATOM) {
      final String text = t.getText();
      if (text.equals("read") || text.equals("write") || text.equals("append")) {
        error = false;
      }
    }
    if (error) {
      return new ProlDomainErrorException("[read,write,append]", "Wrong io mode: " + t, t);
    }
    return null;
  }

  public static Term assertGoal(final Term t) {
    final ProlException exception = checkGoal(t);
    if (exception != null) {
      throw exception;
    }
    return t.tryGround();
  }

  public static ProlException checkGoal(final Term term) {
    final Term that = term.tryGround();
    if ((that.getTermType() == TermType.ATOM && !(that instanceof NumericTerm)) ||
        that.getTermType() == TermType.STRUCT) {
      return null;
    } else {
      return new ProlTypeErrorException("goal",
          "Expected term which can be called as a goal: " + that, that);
    }
  }

  public static TermList assertList(final Term t) {
    final ProlException exception = checkList(t);
    if (exception != null) {
      throw exception;
    }
    return t.tryGround();
  }

  public static ProlException checkList(final Term term) {
    final Term t = term.tryGround();
    if (t.getTermType() != TermType.LIST) {
      return new ProlTypeErrorException("list", "List expected: " + t, t);
    }
    return null;
  }

  public static ProlException checkVar(final Term t) {
    if (t.isUnground() && t.getTermType() != VAR) {
      return new ProlTypeErrorException("var", "Unbound variable expected: " + t, t);
    }
    return null;
  }

  public static TermVar assertVar(final Term t) {
    final ProlException exception = checkVar(t);
    if (exception != null) {
      throw exception;
    }
    return t.tryGround();
  }

  public static TermStruct assertIndicator(final Term t) {
    final ProlException prolException = checkIndicator(t);
    if (prolException == null) {
      return t.tryGround();
    }
    throw prolException;
  }

  public static ProlException checkIndicator(final Term t) {
    assertNonVar(t);
    int errorCode = 1;
    if (t.getTermType() == STRUCT) {
      final TermStruct struct = (TermStruct) t;
      if (struct.getArity() == 2 && "/".equals(struct.getFunctor().getText())) {
        final Term left = struct.getArgumentAt(0).tryGround();
        final Term right = struct.getArgumentAt(1).tryGround();

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
        return new ProlTypeErrorException("predicate_indicator",
            "Predicate indicator expected: " + t, t);
      case 2:
        return new ProlDomainErrorException("integer", "Predicate indicator expected: " + t, t);
      case 3:
        return new ProlRepresentationErrorException("max_arity", "Wrong arity: " + t, t);
    }

    return null;
  }

  public static Term assertNonEmptyList(final Term term) {
    final ProlException exception = checkNonEmptyList(term);
    if (exception != null) {
      throw exception;
    }
    return term;
  }

  public static ProlException checkNonEmptyList(final Term term) {
    final Term t = term.tryGround();
    if (t.getTermType() != TermType.LIST) {
      return new ProlTypeErrorException("list", "Expected list: " + t, t);
    } else {
      if (t.isNullList()) {
        return new ProlDomainErrorException("[]", "Expected non-empty list: " + t, t);
      }
    }
    return null;
  }

  @SuppressWarnings("SpellCheckingInspection")
  public static ProlException checkTriggerEvent(final Term term) {
    final Term t = term.tryGround();
    if (t.getTermType() != TermType.ATOM) {
      return new ProlInstantiationErrorException("Should be an atom '" + t + '\'', t);
    } else {
      final String value = t.getText();
      if (!"onassert".equals(value) && !"onretract".equals(value) &&
          !"onassertretract".equals(value)) {
        return new ProlDomainErrorException(
            "Should be a value from the list [onassert, onretract, onassertretract] '" + t + '\'',
            t);
      }
    }
    return null;
  }

  public Term assertTriggerEvent(final Term term) {
    final ProlException exception = checkTriggerEvent(term);
    if (exception != null) {
      throw exception;
    }
    return term;
  }
}
