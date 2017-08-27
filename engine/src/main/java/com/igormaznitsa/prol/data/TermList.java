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
package com.igormaznitsa.prol.data;

import com.igormaznitsa.prol.exceptions.ProlCriticalError;

import java.util.Map;

/**
 * The term represents a prolog list
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see com.igormaznitsa.prol.data.TermStruct
 */
public final class TermList extends TermStruct {

    /**
     * The text which is used for the list functor
     */
    public static final String LIST_FUNCTOR = ".";
    /**
     * The term which is used as the functor for a list
     */
    public static final Term LIST_FUNCTOR_AS_TERM = new Term(LIST_FUNCTOR);
    /**
     * The term which is used as the NULL LIST
     */
    public static final TermList NULLLIST = new TermList();
    /**
     * The index of the list head in the element array
     */
    private static final int INDEX_HEAD = 0;
    /**
     * The index of the list tail in the element array
     */
    private static final int INDEX_TAIL = 1;

    /**
     * a constructor to make NULL_LIST
     */
    private TermList() {
        super(LIST_FUNCTOR, null);
    }

    /**
     * A constructor allows to make a list with a term as its head
     *
     * @param term the term which will be used as the head of the created list,
     *             must not be null
     */
    public TermList(final Term term) {
        super(LIST_FUNCTOR, new Term[]{term, NULLLIST});
    }

    /**
     * A constructor which allows to make a list contains the head and the tail
     *
     * @param head the head of the created list, must not be null
     * @param tail the tail of the created list, must not be null
     */
    public TermList(final Term head, final Term tail) {
        super(LIST_FUNCTOR, new Term[]{head, tail});
    }

    /**
     * Append a term to the end of a list
     *
     * @param list the destination list, must not be null
     * @param term the term to be added, must not be null
     * @return new generated list for the added term as a TermList object
     */
    public static TermList appendItem(final TermList list, final Term term) {
        final TermList newList = new TermList(term);
        if (!list.isNullList()) {
            newList.setTail(list.getTail());
            list.setTail(newList);
        }
        return newList;
    }

    /**
     * Inside auxulary function to convert a term into ints source representation
     *
     * @param term term to be converted into its source repesentation, must not be
     *             null
     * @return the source like representation of the term
     */
    private static String elementToSourceString(final Term term) {
        switch (term.getTermType()) {
            case Term.TYPE_ATOM: {
                return term.getSourceLikeRepresentation();
            }
            default:
                return term.getSourceLikeRepresentation();
        }
    }

    /**
     * Get the head of the list
     *
     * @return the head of the list as a term, must not be null (if it is not the
     * NULL LIST)
     */
    public Term getHead() {
        return terms[INDEX_HEAD];
    }

    /**
     * Get the tail of the list
     *
     * @return the tail of the list as term because it can be as list as not list
     * (for '|' case), must not be null (if it is not the NULL LIST)
     */
    public Term getTail() {
        final Term tail = this.terms[INDEX_TAIL];
        switch (tail.getTermType()) {
            case TYPE_LIST:
                return tail;
            case TYPE_VAR: {
                final Var var = (Var) tail;
                final Term val = var.getValue();
                return val == null ? var : val;
            }
            default:
                return tail;
        }
    }

    /**
     * Set the tail for the list
     *
     * @param newTail the new tail value, must not be null
     */
    public void setTail(final Term newTail) {
        if (newTail == null) {
            throw new NullPointerException("NULL as Tail in list");
        }
        this.terms[INDEX_TAIL] = newTail;
    }

    /**
     * Calculate the depth of the list
     *
     * @return the depth of the list, 0 if the list is the NULL_LIST
     */
    public int calculateLength() {
        if (this == NULLLIST) {
            return 0;
        }
        final Term tail = this.terms[INDEX_TAIL];
        switch (tail.getTermType()) {
            case TYPE_LIST: {
                return ((TermList) tail).calculateLength() + 1;
            }
            default:
                return 2;
        }
    }

    @Override
    public void fillVarables(final Map<String, Var> table) {
        if (isNullList()) {
            return;
        }
        terms[INDEX_HEAD].fillVarables(table);
        terms[INDEX_TAIL].fillVarables(table);
    }

    /**
     * Check the list to be the NULL LIST
     *
     * @return true if it is the NULL LIST else false
     */
    public boolean isNullList() {
        return this == NULLLIST;
    }

    @Override
    public boolean checkVariables() {
        if (isNullList()) {
            return true;
        }

        Term lst = this;

        while (lst != NULLLIST) {
            if (lst.getTermType() == TYPE_LIST) {
                final TermList tList = (TermList) lst;
                if (!tList.getHead().checkVariables()) {
                    return false;
                }
                lst = tList.getTail();
            } else {
                if (!lst.checkVariables()) {
                    return false;
                }
            }

        }
        return true;
    }

    @Override
    public int getTermType() {
        return TYPE_LIST;
    }

    @Override
    public String toString() {
        if (isNullList()) {
            return "[]";
        }
        final StringBuilder builder = new StringBuilder("[");

        boolean notfirst = false;
        Term list = this;

        while (list != NULLLIST) {
            if (list.getTermType() == Term.TYPE_LIST) {
                if (notfirst) {
                    builder.append(',');
                }
                final TermList asList = (TermList) list;
                builder.append(asList.getHead().toString());
                list = asList.getTail();
            } else {
                if (notfirst) {
                    builder.append('|');
                }
                builder.append(list.toString());
                break;
            }
            notfirst = true;
        }

        builder.append(']');
        return builder.toString();
    }

    @Override
    public String getSourceLikeRepresentation() {
        if (isNullList()) {
            return "[]";
        }
        StringBuilder builder = new StringBuilder("[");

        boolean notfirst = false;
        Term list = this;

        while (list != NULLLIST) {
            if (list.getTermType() == Term.TYPE_LIST) {
                if (notfirst) {
                    builder.append(',');
                }
                final TermList asList = (TermList) list;
                builder.append(elementToSourceString(asList.getHead()));
                list = asList.getTail();
            } else {
                if (notfirst) {
                    builder.append('|');
                }
                builder.append(elementToSourceString(list));
                break;
            }
            notfirst = true;
        }

        builder.append(']');
        return builder.toString();
    }

    /**
     * Replace the last element of the list
     *
     * @param newLastElement new the last element of the list, must not be null
     */
    public final void replaceLastElement(final Term newLastElement) {
        if (isNullList()) {
            throw new ProlCriticalError("Attemption to change Null list");
        }
        TermList curList = this;
        while (true) {
            Term tail = curList.getTail();
            if (tail == NULLLIST || tail.getTermType() != TYPE_LIST) {
                curList.setTail(newLastElement);
                break;
            }
            curList = (TermList) tail;
        }
    }

    @Override
    public String getSignature() {
        return getSourceLikeRepresentation();
    }

    @Override
    public String forWrite() {
        if (isNullList()) {
            return "[]";
        }
        StringBuilder builder = new StringBuilder("[");

        boolean notfirst = false;
        Term list = this;

        while (list != NULLLIST) {
            if (list.getTermType() == Term.TYPE_LIST) {
                if (notfirst) {
                    builder.append(',');
                }
                final TermList asList = (TermList) list;
                builder.append(asList.getHead().forWrite());
                list = asList.getTail();
            } else {
                if (notfirst) {
                    builder.append('|');
                }
                builder.append(list.forWrite());
                break;
            }
            notfirst = true;
        }

        builder.append(']');
        return builder.toString();
    }

    @Override
    public boolean Equ(final Term atom) {
        if (this == atom) {
            return true;
        }

        switch (atom.getTermType()) {
            case Term.TYPE_LIST: {
                TermList thatList = (TermList) atom;
                if (this.isNullList()) {
                    return thatList.isNullList();
                } else if (thatList.isNullList()) {
                    return this.isNullList();
                }

                return this.getHead().Equ(thatList.getHead()) && this.getTail().Equ(thatList.getTail());
            }
            case Term.TYPE_VAR: {
                final Var var = (Var) atom;
                final Term value = var.getValue();
                if (value == null) {
                    return ((Var) atom).setValue(this);
                } else {
                    return Equ(value);
                }
            }
        }
        return false;
    }

    @Override
    public boolean equWithoutSet(Term atom) {
        if (this == atom) {
            return true;
        }

        if (atom.getTermType() == TYPE_VAR) {
            atom = ((Var) atom).getValue();
        }

        if (atom == null) {
            return true;
        }

        if (atom.getTermType() == Term.TYPE_LIST) {
            if (this == atom) {
                return true;
            }
            final TermList thisList = this;
            final TermList thatList = (TermList) atom;

            if (thisList.isNullList()) {
                return thatList.isNullList();
            } else {
                if (thatList.isNullList()) {
                    return false;
                }
            }

            return thisList.getHead().equWithoutSet(thatList.getHead()) && thisList.getTail().equWithoutSet(thatList.getTail());
        }
        return false;
    }

    @Override
    public int termComparsion(Term atom) {
        if (this == atom) {
            return 0;
        }

        if (atom.getTermType() == Term.TYPE_VAR && !((Var) atom).isUndefined()) {
            atom = ((Var) atom).getValue();
        }

        switch (atom.getTermType()) {
            case Term.TYPE_LIST: {
                final TermList thatList = (TermList) atom;
                if (isNullList() && thatList.isNullList()) {
                    return 0;
                }

                final TermList thisList = this;

                if (thisList.isNullList() && !thatList.isNullList()) {
                    return -1;
                }
                if (!thisList.isNullList() && thatList.isNullList()) {
                    return 1;
                }

                final Term thisHead = thisList.getHead();
                final Term thatHead = thatList.getHead();

                int result = thisHead.termComparsion(thatHead);
                if (result != 0) {
                    return result;
                }

                return thisList.getTail().termComparsion(thatList.getTail());
            }
            default:
                return 1;
        }
    }

    @Override
    public boolean hasAnyDifference(final Term atom) {
        if (atom.getTermType() != Term.TYPE_LIST) {
            return true;
        }

        TermList thisList = this;
        TermList thatList = (TermList) atom;

        if (thisList == NULLLIST) {
            return thisList != thatList;
        } else if (thatList == NULLLIST) {
            return true;
        }

        if (thisList.getHead().hasAnyDifference(thatList.getHead())) {
            return true;
        }
        return thisList.getTail().hasAnyDifference(thatList.getTail());
    }

    @Override
    public boolean hasVariableWithName(final String name) {
        if (this == NULLLIST) {
            return false;
        }
        return getHead().hasVariableWithName(name) || getTail().hasVariableWithName(name);
    }
}
