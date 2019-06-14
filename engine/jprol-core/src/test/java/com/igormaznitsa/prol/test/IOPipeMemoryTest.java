package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.io.ProlMemoryPipe;
import com.igormaznitsa.prol.logic.ChoicePoint;
import com.igormaznitsa.prol.logic.ProlConsult;
import com.igormaznitsa.prol.logic.ProlContext;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.fail;

class IOPipeMemoryTest extends AbstractProlTest {

  @Test
  void testIOMemory() throws Exception {
    final ProlContext context = new ProlContext("IO Memory test", DefaultProlStreamManagerImpl.getInstance());
    final ProlConsult consult = new ProlConsult("save_to_memory([]):-!.save_to_memory([X|L]):-write(X),write(\'. \'),save_to_memory(L). all_from_memory :- next_from_memory(X),write(X),nl,(all_from_memory,!;true). next_from_memory(X):-read(X),X\\==end_of_file.", context);
    consult.consult();

    final ChoicePoint goal = new ChoicePoint("tell(\'+buff\'),save_to_memory([1,2,3,4,5,6,7,8,9,10]),told,see(\'+buff\'),tell(\'+buff2\'),all_from_memory,seen,told.", context);

    Term result;
    while ((result = goal.next()) != null) {
      final ProlMemoryPipe pipe = context.findMemPipe("+buff2");
      int next = 1;
      Term term;
      while ((term = pipe.readToken()) != ProlMemoryPipe.END_OF_FILE) {
        final String str = term.forWrite();
        if (!str.equals(Integer.toString(next))) {
          fail("Wrong value!");
        }
        next++;
      }
      if (next < 10) {
        fail("Wrong number of integers!");
      }
    }
  }
}
