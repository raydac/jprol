package com.igormaznitsa.jprol.example.life;

import com.igormaznitsa.jprol.annotations.JProlConsultText;
import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import java.util.Objects;

@SuppressWarnings("unused")
@JProlConsultText({
    "process_cell(X,Y,live) :- count_neighbors(X,Y,N), ((N < 2 ; N > 3), !, reset_cell(X,Y) ; set_cell(X,Y)),!.",
    "process_cell(X,Y,dead) :- count_neighbors(X,Y,N), N = 3, set_cell(X,Y),!.",
    "life() :- field_width(W), field_height(H), MaxW is W - 1, MaxH is H - 1, for(Y,0,MaxH), for(X,0,MaxW), cell_state(X,Y,State), process_cell(X,Y,State), fail."
})
public class LifeLibrary extends AbstractJProlLibrary {

  private static final TermLong WIDTH = Terms.newLong(LifeGameField.WIDTH);
  private static final TermLong HEIGHT = Terms.newLong(LifeGameField.HEIGHT);
  private static final Term CELL_LIVE = Terms.newAtom("live");
  private static final Term CELL_DEAD = Terms.newAtom("dead");
  private final LifeGameField field;

  public LifeLibrary(final LifeGameField field) {
    super("jprol-examples-life");
    this.field = Objects.requireNonNull(field);
  }

  @JProlPredicate(signature = "field_width/1", validate = "?number", determined = true)
  public boolean fieldWidth(final JProlChoicePoint choicePoint, final TermStruct struct) {
    final Term term = struct.getArgumentAt(0).tryGround();
    return term.unifyWith(WIDTH);
  }

  @JProlPredicate(signature = "field_height/1", validate = "?number", determined = true)
  public boolean fieldHeight(final JProlChoicePoint choicePoint, final TermStruct struct) {
    final Term term = struct.getArgumentAt(0).tryGround();
    return term.unifyWith(HEIGHT);
  }

  @JProlPredicate(signature = "cell_state/3", validate = "+number,+number,?atom", determined = true)
  public boolean isCellSet(final JProlChoicePoint choicePoint, final TermStruct struct) {
    final int x = struct.getArgumentAt(0).tryGround().toNumber().intValue();
    final int y = struct.getArgumentAt(1).tryGround().toNumber().intValue();
    final Term state = struct.getArgumentAt(2).tryGround();
    return state.unifyWith(this.field.get(x, y) ? CELL_LIVE : CELL_DEAD);
  }

  @JProlPredicate(signature = "count_neighbors/3", validate = "+number,+number,?number", determined = true)
  public boolean countNeighbors(final JProlChoicePoint choicePoint, final TermStruct struct) {
    final int x = struct.getArgumentAt(0).tryGround().toNumber().intValue();
    final int y = struct.getArgumentAt(1).tryGround().toNumber().intValue();
    final Term n = struct.getArgumentAt(2).tryGround();

    int counter = 0;
    for (int dy = -1; dy <= 1; dy++) {
      for (int dx = -1; dx <= 1; dx++) {
        counter += dx == 0 && dy == 0 ? 0 : this.field.get(x + dx, y + dy) ? 1 : 0;
      }
    }

    final TermLong counterTerm = Terms.newLong(counter);

    return n.unifyWith(counterTerm);
  }

  @JProlPredicate(signature = "set_cell/2", validate = "+number,+number", determined = true)
  public void setCell(final JProlChoicePoint choicePoint, final TermStruct struct) {
    final int x = struct.getArgumentAt(0).tryGround().toNumber().intValue();
    final int y = struct.getArgumentAt(1).tryGround().toNumber().intValue();
    this.field.setNext(x, y, true);
  }

  @JProlPredicate(signature = "reset_cell/2", validate = "+number,+number", determined = true)
  public void resetCell(final JProlChoicePoint choicePoint, final TermStruct struct) {
    final int x = struct.getArgumentAt(0).tryGround().toNumber().intValue();
    final int y = struct.getArgumentAt(1).tryGround().toNumber().intValue();
    this.field.setNext(x, y, false);
  }

}
