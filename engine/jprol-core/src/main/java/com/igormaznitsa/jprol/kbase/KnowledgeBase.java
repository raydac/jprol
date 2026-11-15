/*
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
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

package com.igormaznitsa.jprol.kbase;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermOperator;
import com.igormaznitsa.jprol.data.TermOperatorContainer;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.utils.CloseableIterator;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Store of dynamic clauses and user-defined operators for one Prolog context.
 *
 * <p>A context owns one base. Reach it with {@link JProlContext#getKnowledgeBase()} and pass that
 * same context into every method that takes a context: assert and retract notify triggers
 * registered on it. {@code asserta/1}, {@code assertz/1}, {@code retract/1}, {@code abolish/1},
 * and {@code clause/2} are the Prolog surface of this store.
 *
 * <p>Clauses are kept under a predicate indicator {@code name/arity}, for example {@code parent/2}.
 * A fact is a structure. A rule is a {@code :-/2} structure whose first argument is the head.
 * {@link #assertA} inserts a clause where it is tried first. {@link #assertZ} inserts it where it
 * is tried last.
 *
 * <p>Share a base across threads only when {@link #isConcurrent()} is true. An async proof refuses
 * a base that reports {@code false}. {@link #makeCopy()} builds an independent base with the same
 * clauses and operators.
 *
 * <pre>{@code
 * JProlContext context = new JProlContext("demo", new File("."));
 * KnowledgeBase base = context.getKnowledgeBase();
 * TermStruct fact = Terms.newStruct(
 *     Terms.newAtom("parent"), Terms.newAtom("tom"), Terms.newAtom("bob"));
 * base.assertZ(context, fact);
 *
 * try (CloseableIterator<TermStruct> clauses = base.iterate(
 *     IteratorType.FACTS, fact, (signature, term) -> { })) {
 *   while (clauses.hasNext()) {
 *     TermStruct clause = clauses.next();
 *   }
 * }
 * }</pre>
 *
 * @see JProlContext#getKnowledgeBase()
 * @see RetractCursor
 */
public interface KnowledgeBase {

  /**
   * Text identity of this base.
   *
   * <p>The id is fixed for the life of the instance. A copy made by {@link #makeCopy()} has its
   * own id.
   *
   * @return the base id, never {@code null}
   */
  String getId();

  /**
   * Drop one user-defined operator associativity.
   *
   * <p>A system operator is not stored here, so this call cannot remove it. When the name has
   * several associativities, only {@code type} is removed.
   *
   * @param name operator name, must not be {@code null}
   * @param type associativity to remove, must not be {@code null}
   * @return {@code true} when that name and associativity were present and have been removed
   * @throws NullPointerException when {@code name} or {@code type} is {@code null}
   */
  boolean removeOperator(String name, OpAssoc type);

  /**
   * Define a user operator.
   *
   * <p>The operator is then visible to the parser of {@code context}. The same name may carry
   * more than one associativity when the parser allows that pair. Defining the same name and
   * associativity again is rejected.
   *
   * @param context  context whose parser will see the operator, must not be {@code null}
   * @param operator operator to add, must not be {@code null}
   * @throws SecurityException     when {@code operator} redefines a system operator
   * @throws IllegalStateException when this name and associativity are already defined
   * @throws NullPointerException  when {@code context} or {@code operator} is {@code null}
   */
  void addOperator(JProlContext context, TermOperator operator);

  /**
   * Resolve an operator by its exact name.
   *
   * <p>A system operator of that name wins. A user operator is returned only when the context has
   * no system operator with the same text.
   *
   * @param context context whose system operators are consulted first, must not be {@code null}
   * @param name operator name, must not be {@code null}
   * @return the operator container, or {@code null} when neither a system nor a user operator
   *     has this name
   * @throws NullPointerException when {@code context} or {@code name} is {@code null}
   */
  TermOperatorContainer findOperatorForName(JProlContext context, String name);

  /**
   * Report whether any operator name has {@code str} as a prefix.
   *
   * <p>System operator names are tested first, then user-defined names, with
   * {@link String#startsWith(String)}. An empty prefix matches every existing name.
   *
   * @param context context whose system operators are included, must not be {@code null}
   * @param str prefix, must not be {@code null}
   * @return {@code true} when at least one operator name starts with {@code str}
   * @throws NullPointerException when {@code context} or {@code str} is {@code null}
   */
  boolean hasOperatorStartsWith(JProlContext context, String str);

  /**
   * Walk the clauses of one predicate, in source order, that unify with {@code template}.
   *
   * <p>The predicate is {@code template.getSignature()} ({@code name/arity}). {@code type} keeps
   * facts, rules, or both. Each returned clause is the stored term: a ground fact may be shared
   * with the base, so the caller must not mutate it.
   *
   * <p>When the predicate has no clauses, {@code unknownSignatureConsumer} is called once with the
   * signature and the template, and the iterator is empty. Close the iterator when the scan ends.
   * {@link java.util.Iterator#next()} throws {@link java.util.NoSuchElementException} after the
   * last clause.
   *
   * @param type which clauses to yield, must not be {@code null}
   * @param template clause or head used both as the predicate indicator and as the unify pattern,
   *     must not be {@code null}
   * @param unknownSignatureConsumer notified with the signature and the template when the
   *     predicate is absent, must not be {@code null}
   * @return a closable iterator of matching clauses, never {@code null}
   * @throws NullPointerException when an argument is {@code null}
   * @throws RuntimeException when {@code unknownSignatureConsumer} throws
   * @see IteratorType
   */
  CloseableIterator<TermStruct> iterate(IteratorType type, TermStruct template,
                                        BiConsumer<String, Term> unknownSignatureConsumer);

  /**
   * List the stored clauses of one predicate, in source order, without unifying them.
   *
   * <p>{@code signature} is a predicate indicator such as {@code parent/2}. The iterator yields
   * the stored clause terms. When the predicate is absent, {@code unknownSignatureConsumer} is
   * called once with that signature and the iterator is empty. Close the iterator when the scan
   * ends.
   *
   * @param signature predicate indicator {@code name/arity}, must not be {@code null}
   * @param unknownSignatureConsumer notified with {@code signature} when the predicate is absent,
   *     must not be {@code null}
   * @return a closable iterator of stored clauses, never {@code null}
   * @throws NullPointerException when an argument is {@code null}
   * @throws RuntimeException when {@code unknownSignatureConsumer} throws
   */
  CloseableIterator<TermStruct> iterate(String signature,
                                        Consumer<String> unknownSignatureConsumer);

  /**
   * List predicate indicators currently stored in this base.
   *
   * <p>Each element is a {@code name/arity} structure. An element is yielded only when it unifies
   * with {@code indicator}. Pass a structure of two anonymous variables to list every predicate.
   * Close the iterator when the scan ends.
   *
   * @param indicator predicate-indicator pattern, for example {@code parent/2} or {@code _/_},
   *     must not be {@code null}
   * @return a closable iterator of matching {@code name/arity} structures, never {@code null}
   * @throws NullPointerException when {@code indicator} is {@code null}
   */
  CloseableIterator<TermStruct> iterateSignatures(TermStruct indicator);

  /**
   * Walk the user-defined operators of this base.
   *
   * <p>System operators are not included. Close the iterator when the scan ends.
   *
   * @return a closable iterator of user operators, never {@code null}
   */
  CloseableIterator<TermOperator> makeOperatorIterator();

  /**
   * Store {@code clause} so it is tried after the clauses already present ({@code assertz/1}).
   *
   * <p>A fact is stored under its own signature. A rule is stored under the signature of its head.
   * The head atom of a rule is wrapped as a zero-arity structure. A rule body must be callable.
   * An unbound body variable is accepted only when that variable also occurs in the head.
   * On success, assert triggers for the predicate are notified on {@code context}.
   *
   * @param context context that receives assert triggers, must not be {@code null}
   * @param clause fact or {@code :-/2} rule to append, must not be {@code null}
   * @return {@code true} when the clause is stored. A base may return {@code false} when it
   *     refuses the clause without an error. The in-memory bases throw instead of returning
   *     {@code false}
   * @throws com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException when a rule body is
   *     an unbound variable that does not occur in the head
   * @throws com.igormaznitsa.jprol.exceptions.ProlTypeErrorException when a rule body is not
   *     callable, or a rule head is not a structure
   * @throws com.igormaznitsa.jprol.exceptions.ProlKnowledgeBaseException when the clause cannot
   *     be stored
   * @throws com.igormaznitsa.jprol.exceptions.ProlException when the context is already disposed
   *     and a trigger must be notified
   * @throws NullPointerException when {@code context} or {@code clause} is {@code null}
   */
  boolean assertZ(JProlContext context, TermStruct clause);

  /**
   * Store {@code clause} so it is tried before the clauses already present ({@code asserta/1}).
   *
   * <p>The acceptance rules and the trigger notification are the same as {@link #assertZ}. Only
   * the position differs: this call inserts the clause at the front of the predicate.
   *
   * @param context context that receives assert triggers, must not be {@code null}
   * @param clause fact or {@code :-/2} rule to insert first, must not be {@code null}
   * @return {@code true} when the clause is stored. A base may return {@code false} when it
   *     refuses the clause without an error. The in-memory bases throw instead of returning
   *     {@code false}
   * @throws com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException when a rule body is
   *     an unbound variable that does not occur in the head
   * @throws com.igormaznitsa.jprol.exceptions.ProlTypeErrorException when a rule body is not
   *     callable, or a rule head is not a structure
   * @throws com.igormaznitsa.jprol.exceptions.ProlKnowledgeBaseException when the clause cannot
   *     be stored
   * @throws com.igormaznitsa.jprol.exceptions.ProlException when the context is already disposed
   *     and a trigger must be notified
   * @throws NullPointerException when {@code context} or {@code clause} is {@code null}
   */
  boolean assertA(JProlContext context, TermStruct clause);

  /**
   * Remove every clause of the predicate that unifies with {@code clause} ({@code retractall/1}).
   *
   * <p>The predicate is taken from the clause head. Variables in {@code clause} are a search
   * pattern only: they are not bound to the removed clauses. Each removed clause notifies retract
   * triggers on {@code context}. The predicate entry disappears when its last clause is removed.
   *
   * @param context context that receives retract triggers, must not be {@code null}
   * @param clause fact or rule used as the removal pattern, must not be {@code null}
   * @return {@code true} when at least one clause was removed
   * @throws com.igormaznitsa.jprol.exceptions.ProlTypeErrorException when {@code clause} is a rule
   *     whose head is not a structure
   * @throws com.igormaznitsa.jprol.exceptions.ProlException when the context is already disposed
   *     and a trigger must be notified
   * @throws NullPointerException when {@code context} or {@code clause} is {@code null}
   */
  boolean retractAll(JProlContext context, TermStruct clause);

  /**
   * Remove the first clause that unifies with {@code clause} and bind that clause to it
   * ({@code retract/1} of the oldest match).
   *
   * <p>On success the removed clause is unified with {@code clause}, so variables in the pattern
   * receive the stored values. A retract trigger for the predicate is then notified.
   *
   * @param context context that receives the retract trigger, must not be {@code null}
   * @param clause fact or rule used as the removal pattern, must not be {@code null}
   * @return {@code true} when a clause was removed and unified with {@code clause}
   * @throws com.igormaznitsa.jprol.exceptions.ProlTypeErrorException when {@code clause} is a rule
   *     whose head is not a structure
   * @throws com.igormaznitsa.jprol.exceptions.ProlException when the context is already disposed
   *     and a trigger must be notified
   * @throws NullPointerException when {@code context} or {@code clause} is {@code null}
   * @see #openRetract(JProlContext, TermStruct)
   */
  boolean retractA(JProlContext context, TermStruct clause);

  /**
   * Remove the last clause that unifies with {@code clause} and bind that clause to it.
   *
   * <p>On success the removed clause is unified with {@code clause}. A retract trigger for the
   * predicate is then notified. Use {@link #retractA} for the first match and
   * {@link #openRetract} to remove one match per redo.
   *
   * @param context context that receives the retract trigger, must not be {@code null}
   * @param clause fact or rule used as the removal pattern, must not be {@code null}
   * @return {@code true} when a clause was removed and unified with {@code clause}
   * @throws com.igormaznitsa.jprol.exceptions.ProlTypeErrorException when {@code clause} is a rule
   *     whose head is not a structure
   * @throws com.igormaznitsa.jprol.exceptions.ProlException when the context is already disposed
   *     and a trigger must be notified
   * @throws NullPointerException when {@code context} or {@code clause} is {@code null}
   */
  boolean retractZ(JProlContext context, TermStruct clause);

  /**
   * Open a logical-update scan that removes one matching clause per call.
   *
   * <p>This is the cursor behind {@code retract/1}. The visible generation is the clause list at
   * the moment of this call. The cursor is {@link RetractCursor#EMPTY} when the predicate has no
   * clauses then.
   *
   * @param context context that receives a retract trigger for each removal, must not be
   *     {@code null}
   * @param clause fact or rule used as the removal pattern, must not be {@code null}
   * @return a cursor over the clauses visible at this call, never {@code null}
   * @throws com.igormaznitsa.jprol.exceptions.ProlTypeErrorException when {@code clause} is a rule
   *     whose head is not a structure
   * @throws NullPointerException when {@code context} or {@code clause} is {@code null}
   * @see RetractCursor
   */
  RetractCursor openRetract(JProlContext context, TermStruct clause);

  /**
   * Remove every clause of one predicate ({@code abolish/1}).
   *
   * <p>{@code signature} is a predicate indicator such as {@code parent/2}. A missing predicate
   * leaves the base unchanged. On a successful removal an abolish trigger is notified with a
   * {@code null} term.
   *
   * @param context context that receives the abolish trigger, must not be {@code null}
   * @param signature predicate indicator {@code name/arity}, must not be {@code null}
   * @return {@code true} when the predicate had clauses and they have been removed
   * @throws IllegalArgumentException when {@code signature} is not a {@code name/arity} indicator
   * @throws com.igormaznitsa.jprol.exceptions.ProlException when the context is already disposed
   *     and a trigger must be notified
   * @throws NullPointerException when {@code context} or {@code signature} is {@code null}
   */
  boolean abolish(JProlContext context, String signature);

  /**
   * Copy clauses, operators, and the other stored state into a new base.
   *
   * <p>Later asserts and retracts on either base stay on that base. The copy reports the same
   * answer from {@link #isConcurrent()} as this base. Its id is chosen by the implementation.
   *
   * @return an independent base with the same clauses and operators, never {@code null}
   */
  KnowledgeBase makeCopy();

  /**
   * Report whether this base may be used from several threads at once.
   *
   * <p>{@code true} means the tables tolerate concurrent assert, retract, and lookup.
   * {@code false} means the caller must keep all access on one thread. Async proof requires a
   * concurrent base.
   *
   * @return {@code true} when concurrent use is supported
   */
  boolean isConcurrent();

  /**
   * Drop every clause, user operator, and lookup index.
   *
   * <p>Triggers are not notified. The id returned by {@link #getId()} stays the same.
   *
   * @since 3.0.0
   */
  void clear();
}
