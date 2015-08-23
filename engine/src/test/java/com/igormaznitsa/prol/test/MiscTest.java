/*
 * Copyright 2015 Igor Maznitsa (http://www.igormaznitsa.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermInteger;
import com.igormaznitsa.prol.data.Var;
import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import com.igormaznitsa.prol.utils.Utils;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import static org.junit.Assert.*;
import org.junit.Test;

public class MiscTest {
  
  @Test
  public void testGetAllGoalsAndConvertThem() throws Exception {
    final ProlContext context = new ProlContext("test", DefaultProlStreamManagerImpl.getInstance());
    final ProlConsult consult = new ProlConsult("map(one,1). map(two,2). map(three,3).", context);
    consult.consult();
    
    final Goal goal = new Goal("map(X,Y).", context);

    final class Result {
      final String name;
      final int value;
      Result(final Map<String,Var> map){
        this.name = map.get("X").getValue().getText();
        this.value = (Integer)((TermInteger)map.get("Y").getValue()).getNumericValue();
      }
      
      @Override
      public String toString(){
        return this.name +" is "+this.value;
      }
    }
    
    final List<Result> result = new ArrayList<Result>();
    while(true){
      final Term t = goal.solve();
      if (t == null) break;
      result.add(new Result(Utils.fillTableWithVars(t)));
    }
    
    assertEquals(3,result.size());
    assertEquals("one",result.get(0).name);
    assertEquals("two",result.get(1).name);
    assertEquals("three",result.get(2).name);
    assertEquals(1,result.get(0).value);
    assertEquals(2,result.get(1).value);
    assertEquals(3,result.get(2).value);
    
    System.out.println("List: "+result);
  }
}
