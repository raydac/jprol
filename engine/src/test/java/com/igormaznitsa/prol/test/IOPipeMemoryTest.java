package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.io.ProlMemoryPipe;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import junit.framework.TestCase;
import org.junit.Test;
import static org.junit.Assert.*;

public class IOPipeMemoryTest extends TestCase {

  @Test
  public void testIOMemory() {
    try {
      final ProlContext context = new ProlContext("IO Memory test", DefaultProlStreamManagerImpl.getInstance());
      final ProlConsult consult = new ProlConsult("save_to_memory([]):-!.save_to_memory([X|L]):-write(X),write(\'. \'),save_to_memory(L). all_from_memory :- next_from_memory(X),write(X),nl,(all_from_memory,!;true). next_from_memory(X):-read(X),X\\==end_of_file.", context);
      consult.consult();

      final Goal goal = new Goal("tell(\'+buff\'),save_to_memory([1,2,3,4,5,6,7,8,9,10]),told,see(\'+buff\'),tell(\'+buff2\'),all_from_memory,seen,told.", context);

      while (true) {
        final Term result = goal.solve();
        if (result == null) {
          break;
        }

        final ProlMemoryPipe pipe = context.getMemoryPipeForName("+buff2");
        int next = 1;
        while (true) {
          final Term term = pipe.readToken();
          if (term == ProlMemoryPipe.END_OF_FILE) {
            break;
          }
          final String str = term.forWrite();

          if (!str.equals(Integer.toString(next))) {
            fail("Wrong value!");
            return;
          }
          next++;
        }
        if (next < 10) {
          fail("Wrong number of integers!");
          return;
        }
      }

      assertTrue(true);
    }
    catch (Throwable thr) {
      thr.printStackTrace();
      fail("Exception during operation");
    }
  }
}
