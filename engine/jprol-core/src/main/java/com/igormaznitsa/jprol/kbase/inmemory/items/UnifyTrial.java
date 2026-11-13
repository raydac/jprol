package com.igormaznitsa.jprol.kbase.inmemory.items;

import static com.igormaznitsa.jprol.data.TermType.ATOM;
import static com.igormaznitsa.jprol.data.TermType.LIST;
import static com.igormaznitsa.jprol.data.TermType.OPERATOR;
import static com.igormaznitsa.jprol.data.TermType.STRUCT;
import static com.igormaznitsa.jprol.data.TermType.VAR;

import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermOperator;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermVar;
import java.util.Arrays;

final class UnifyTrial {

  private static final int SIDE_LEFT = 0;
  private static final int SIDE_RIGHT = 1;
  private static final int INITIAL_CAPACITY = 8;
  private static final int WATCHDOG = 8192;

  private TermVar[] boundVars = new TermVar[INITIAL_CAPACITY];
  private byte[] boundSides = new byte[INITIAL_CAPACITY];
  private Term[] targetTerms = new Term[INITIAL_CAPACITY];
  private byte[] targetSides = new byte[INITIAL_CAPACITY];
  private int size;

  private int derefSide;
  private Term derefTerm;

  static boolean canUnify(final Term left, final Term right) {
    return new UnifyTrial().unify(SIDE_LEFT, left, SIDE_RIGHT, right);
  }

  private void dereference(final int startSide, final Term startTerm) {
    int side = startSide;
    Term term = startTerm;

    for (int step = 0; step < WATCHDOG; step++) {
      while (term.getTermType() == VAR) {
        final Term next = ((TermVar) term).getImmediateValue();
        if (next == null) {
          break;
        }
        term = next;
      }

      if (term.getTermType() != VAR) {
        this.derefSide = side;
        this.derefTerm = term;
        return;
      }

      final int bindingIndex = this.findBinding(side, (TermVar) term);
      if (bindingIndex < 0) {
        this.derefSide = side;
        this.derefTerm = term;
        return;
      }

      side = this.targetSides[bindingIndex];
      term = this.targetTerms[bindingIndex];
    }

    throw new IllegalStateException("Too deep variable chain detected in unify trial");
  }

  private int findBinding(final int side, final TermVar variable) {
    for (int i = 0; i < this.size; i++) {
      if (this.boundVars[i] == variable && this.boundSides[i] == side) {
        return i;
      }
    }
    return -1;
  }

  private void bind(
      final int varSide,
      final TermVar variable,
      final int targetSide,
      final Term target
  ) {
    if (this.size == this.boundVars.length) {
      final int grown = this.boundVars.length << 1;
      this.boundVars = Arrays.copyOf(this.boundVars, grown);
      this.boundSides = Arrays.copyOf(this.boundSides, grown);
      this.targetTerms = Arrays.copyOf(this.targetTerms, grown);
      this.targetSides = Arrays.copyOf(this.targetSides, grown);
    }
    final int idx = this.size++;
    this.boundVars[idx] = variable;
    this.boundSides[idx] = (byte) varSide;
    this.targetTerms[idx] = target;
    this.targetSides[idx] = (byte) targetSide;
  }

  private boolean unify(
      final int leftSide,
      final Term rawLeft,
      final int rightSide,
      final Term rawRight
  ) {
    int curLeftSide = leftSide;
    Term curLeft = rawLeft;
    int curRightSide = rightSide;
    Term curRight = rawRight;

    while (true) {
      if (curLeftSide == curRightSide && curLeft == curRight) {
        return true;
      }

      this.dereference(curLeftSide, curLeft);
      final int sL = this.derefSide;
      final Term tL = this.derefTerm;

      this.dereference(curRightSide, curRight);
      final int sR = this.derefSide;
      final Term tR = this.derefTerm;

      if (sL == sR && tL == tR) {
        return true;
      }
      if (tL.getTermType() == VAR) {
        this.bind(sL, (TermVar) tL, sR, tR);
        return true;
      }
      if (tR.getTermType() == VAR) {
        this.bind(sR, (TermVar) tR, sL, tL);
        return true;
      }
      if (tL instanceof NumericTerm || tR instanceof NumericTerm) {
        return tL.getClass() == tR.getClass()
            && ((NumericTerm) tL).compare((NumericTerm) tR) == 0;
      }

      switch (tL.getTermType()) {
        case LIST: {
          if (tR.getTermType() != LIST) {
            return false;
          }
          final TermList listL = (TermList) tL;
          final TermList listR = (TermList) tR;
          if (listL.isNullList()) {
            return listR.isNullList();
          }
          if (listR.isNullList()) {
            return false;
          }
          if (!this.unify(sL, listL.getHead(), sR, listR.getHead())) {
            return false;
          }
          curLeftSide = sL;
          curLeft = listL.getTail();
          curRightSide = sR;
          curRight = listR.getTail();
          continue;
        }
        case STRUCT: {
          final TermStruct structL = (TermStruct) tL;
          if (tR.getTermType() == STRUCT) {
            final TermStruct structR = (TermStruct) tR;
            final int arity = structL.getArity();
            if (arity != structR.getArity()
                || !this.unify(sL, structL.getFunctor(), sR, structR.getFunctor())) {
              return false;
            }
            for (int i = 0; i < arity; i++) {
              if (!this.unify(sL, structL.getArgumentAt(i), sR, structR.getArgumentAt(i))) {
                return false;
              }
            }
            return true;
          }
          return tR.getTermType() == ATOM
              && structL.getArity() == 0
              && structL.getFunctor().getText().equals(tR.getText());
        }
        case OPERATOR: {
          if (tR.getTermType() == OPERATOR) {
            return ((TermOperator) tL).getType() == ((TermOperator) tR).getType()
                && tL.getText().equals(tR.getText());
          }
          return tR.getTermType() == ATOM && tL.getText().equals(tR.getText());
        }
        case ATOM: {
          if (tR.getTermType() == ATOM || tR.getTermType() == OPERATOR) {
            return tL.getText().equals(tR.getText());
          }
          if (tR.getTermType() == STRUCT) {
            final TermStruct structR = (TermStruct) tR;
            return structR.getArity() == 0
                && tL.getText().equals(structR.getFunctor().getText());
          }
          return false;
        }
        default:
          return false;
      }
    }
  }
}
