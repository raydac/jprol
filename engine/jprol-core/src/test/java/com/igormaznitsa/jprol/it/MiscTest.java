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

package com.igormaznitsa.jprol.it;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

class MiscTest extends AbstractJProlTest {

  @Test
  void testGetAllGoalsAndConvertThem() {
    final JProlContext context = makeContextAndConsult("map(one,1,a). map(two,2,b). map(three,3,c).");

    final JProlChoicePoint goal = new JProlChoicePoint("map(X,Y,_).", context);

    final class Result {
      final String name;
      final int value;

      Result(final Map<String, Term> map) {
        assertEquals(2, map.size());
        this.name = map.get("X").getText();
        this.value = map.get("Y").toNumber().intValue();
      }

      @Override
      public String toString() {
        return this.name + " is " + this.value;
      }
    }

    final List<Result> result = new ArrayList<>();
    Term t;
    while ((t = goal.prove()) != null) {
      result.add(new Result(t.allNamedVarValuesAsMap()));
    }

    assertEquals(3, result.size());
    assertEquals("one", result.get(0).name);
    assertEquals("two", result.get(1).name);
    assertEquals("three", result.get(2).name);
    assertEquals(1, result.get(0).value);
    assertEquals(2, result.get(1).value);
    assertEquals(3, result.get(2).value);
  }

  @Test
  void findAllTest() {
    JProlContext context;
    context = makeContextAndConsult("powerSet([],[]).powerSet([_|Xt],Y) :- powerSet(Xt,Y).powerSet([Xh|Xt],[Xh|Yt]) :- powerSet(Xt,Yt).");
    final JProlChoicePoint goal = new JProlChoicePoint("findall(X,powerSet([a,b],X),Y).", context);

    TermVar result = null;
    while (goal.prove() != null) {
      final TermVar valueAtY = goal.findVar("Y").orElseThrow(() -> new IllegalArgumentException("Must be presented"));
      assertNull(result, "Result must be null because it should be called only once");
      result = valueAtY;
    }

    assertNotNull(result);
    assertInstanceOf(TermList.class, result.getValue());
    assertEquals("[[],['b'],['a'],['a','b']]", result.getValue().toString());
  }

}
