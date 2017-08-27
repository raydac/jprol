package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import junit.framework.TestCase;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;

public class EightQueens extends TestCase {

    @Test
    public void testEightQueens() throws Throwable {
        final String[] etal = new String[]{"[4,2,7,3,6,8,5,1]", "[5,2,4,7,3,8,6,1]", "[3,5,2,8,6,4,7,1]", "[3,6,4,2,8,5,7,1]", "[5,7,1,3,8,6,4,2]",
                "[4,6,8,3,1,7,5,2]",
                "[3,6,8,1,4,7,5,2]",
                "[5,3,8,4,7,1,6,2]",
                "[5,7,4,1,3,8,6,2]",
                "[4,1,5,8,6,3,7,2]",
                "[3,6,4,1,8,5,7,2]",
                "[4,7,5,3,1,6,8,2]",
                "[6,4,2,8,5,7,1,3]",
                "[6,4,7,1,8,2,5,3]",
                "[1,7,4,6,8,2,5,3]",
                "[6,8,2,4,1,7,5,3]",
                "[6,2,7,1,4,8,5,3]",
                "[4,7,1,8,5,2,6,3]",
                "[5,8,4,1,7,2,6,3]",
                "[4,8,1,5,7,2,6,3]",
                "[2,7,5,8,1,4,6,3]",
                "[1,7,5,8,2,4,6,3]",
                "[2,5,7,4,1,8,6,3]",
                "[4,2,7,5,1,8,6,3]",
                "[5,7,1,4,2,8,6,3]",
                "[6,4,1,5,8,2,7,3]",
                "[5,1,4,6,8,2,7,3]",
                "[5,2,6,1,7,4,8,3]",
                "[6,3,7,2,8,5,1,4]",
                "[2,7,3,6,8,5,1,4]",
                "[7,3,1,6,8,5,2,4]",
                "[5,1,8,6,3,7,2,4]",
                "[1,5,8,6,3,7,2,4]",
                "[3,6,8,1,5,7,2,4]",
                "[6,3,1,7,5,8,2,4]",
                "[7,5,3,1,6,8,2,4]",
                "[7,3,8,2,5,1,6,4]",
                "[5,3,1,7,2,8,6,4]",
                "[2,5,7,1,3,8,6,4]",
                "[3,6,2,5,8,1,7,4]",
                "[6,1,5,2,8,3,7,4]",
                "[8,3,1,6,2,5,7,4]",
                "[2,8,6,1,3,5,7,4]",
                "[5,7,2,6,3,1,8,4]",
                "[3,6,2,7,5,1,8,4]",
                "[6,2,7,1,3,5,8,4]",
                "[3,7,2,8,6,4,1,5]",
                "[6,3,7,2,4,8,1,5]",
                "[4,2,7,3,6,8,1,5]",
                "[7,1,3,8,6,4,2,5]",
                "[1,6,8,3,7,4,2,5]",
                "[3,8,4,7,1,6,2,5]",
                "[6,3,7,4,1,8,2,5]",
                "[7,4,2,8,6,1,3,5]",
                "[4,6,8,2,7,1,3,5]",
                "[2,6,1,7,4,8,3,5]",
                "[2,4,6,8,3,1,7,5]",
                "[3,6,8,2,4,1,7,5]",
                "[6,3,1,8,4,2,7,5]",
                "[8,4,1,3,6,2,7,5]",
                "[4,8,1,3,6,2,7,5]",
                "[2,6,8,3,1,4,7,5]",
                "[7,2,6,3,1,4,8,5]",
                "[3,6,2,7,1,4,8,5]",
                "[4,7,3,8,2,5,1,6]",
                "[4,8,5,3,1,7,2,6]",
                "[3,5,8,4,1,7,2,6]",
                "[4,2,8,5,7,1,3,6]",
                "[5,7,2,4,8,1,3,6]",
                "[7,4,2,5,8,1,3,6]",
                "[8,2,4,1,7,5,3,6]",
                "[7,2,4,1,8,5,3,6]",
                "[5,1,8,4,2,7,3,6]",
                "[4,1,5,8,2,7,3,6]",
                "[5,2,8,1,4,7,3,6]",
                "[3,7,2,8,5,1,4,6]",
                "[3,1,7,5,8,2,4,6]",
                "[8,2,5,3,1,7,4,6]",
                "[3,5,2,8,1,7,4,6]",
                "[3,5,7,1,4,2,8,6]",
                "[5,2,4,6,8,3,1,7]",
                "[6,3,5,8,1,4,2,7]",
                "[5,8,4,1,3,6,2,7]",
                "[4,2,5,8,6,1,3,7]",
                "[4,6,1,5,2,8,3,7]",
                "[6,3,1,8,5,2,4,7]",
                "[5,3,1,6,8,2,4,7]",
                "[4,2,8,6,1,3,5,7]",
                "[6,3,5,7,1,4,2,8]",
                "[6,4,7,1,3,5,2,8]",
                "[4,7,5,2,6,1,3,8]",
                "[5,7,2,6,3,1,4,8]"};

        // check the knowledge base export data process
        //--
        final ProlContext context0 = new ProlContext("test", DefaultProlStreamManagerImpl.getInstance());
        final ProlConsult consult0 = new ProlConsult("solution([]). solution([X/Y|Others]):-solution(Others),member(Y,[1,2,3,4,5,6,7,8]),notattack(X/Y,Others). notattack(_,[]). notattack(X/Y,[X1/Y1 | Others]):- Y=\\=Y1, Y1-Y=\\=X1-X, Y1-Y=\\=X-X1, notattack(X/Y,Others). member(Item,[Item|Rest]). member(Item,[First|Rest]):-member(Item,Rest). template([1/Y1,2/Y2,3/Y3,4/Y4,5/Y5,6/Y6,7/Y7,8/Y8]).", context0);
        consult0.consult();
        ByteArrayOutputStream buffer = new ByteArrayOutputStream(1024);
        PrintWriter src = new PrintWriter(buffer);
        context0.getKnowledgeBase().write(src);
        src.flush();
        src.close();
        //--

        final ProlContext context = new ProlContext("test", DefaultProlStreamManagerImpl.getInstance());
        final ProlConsult consult = new ProlConsult(new ByteArrayInputStream(buffer.toByteArray()), context);

        consult.consult();

        final Goal goal = new Goal("solution([1/Y1,2/Y2,3/Y3,4/Y4,5/Y5,6/Y6,7/Y7,8/Y8]),Res = [Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8].", context);
        int combinatioCounter = 0;

        for (final String anEtal : etal) {
            assertNotNull(goal.solve());
            assertEquals(goal.getVarAsText("Res"), anEtal);
        }
        assertNull(goal.solve());
    }
}
