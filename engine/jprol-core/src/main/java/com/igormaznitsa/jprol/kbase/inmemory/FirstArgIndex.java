package com.igormaznitsa.jprol.kbase.inmemory;

import static com.igormaznitsa.jprol.data.TermType.ATOM;
import static com.igormaznitsa.jprol.data.TermType.LIST;
import static com.igormaznitsa.jprol.data.TermType.OPERATOR;
import static com.igormaznitsa.jprol.data.TermType.STRUCT;
import static com.igormaznitsa.jprol.data.TermType.VAR;

import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermDouble;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.kbase.inmemory.items.InMemoryItem;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

final class FirstArgIndex {

  static final int MIN_CLAUSES = 8;

  private static final int[] EMPTY = new int[0];

  private final InMemoryItem[] items;
  private final Map<String, int[]> atoms;
  private final Map<String, int[]> structs;
  private final Map<Long, int[]> longs;
  private final Map<Long, int[]> doubles;
  private final int[] lists;
  private final int[] nils;
  private final int[] wildcard;

  private FirstArgIndex(
      final InMemoryItem[] items,
      final Map<String, int[]> atoms,
      final Map<String, int[]> structs,
      final Map<Long, int[]> longs,
      final Map<Long, int[]> doubles,
      final int[] lists,
      final int[] nils,
      final int[] wildcard) {
    this.items = items;
    this.atoms = atoms;
    this.structs = structs;
    this.longs = longs;
    this.doubles = doubles;
    this.lists = lists;
    this.nils = nils;
    this.wildcard = wildcard;
  }

  static boolean canSelect(final TermStruct query) {
    if (query.getArity() == 0) {
      return false;
    }
    final Term formed = indexForm(query.getArgumentAt(0));
    if (formed.getTermType() == VAR
        || (formed instanceof NumericTerm
        && !(formed instanceof TermLong)
        && !(formed instanceof TermDouble))) {
      return false;
    }
    switch (formed.getTermType()) {
      case LIST:
      case STRUCT:
      case ATOM:
      case OPERATOR:
        return true;
      default:
        return formed instanceof TermLong || formed instanceof TermDouble;
    }
  }

  static boolean incompatibleFirstArg(final Term clauseHead, final Term query) {
    if (!(clauseHead instanceof TermStruct) || !(query instanceof TermStruct)) {
      return false;
    }
    final TermStruct clauseStruct = (TermStruct) clauseHead;
    final TermStruct queryStruct = (TermStruct) query;
    if (clauseStruct.getArity() == 0 || queryStruct.getArity() == 0) {
      return false;
    }
    return keysReject(
        indexForm(clauseStruct.getArgumentAt(0)),
        indexForm(queryStruct.getArgumentAt(0)));
  }

  static FirstArgIndex build(final List<InMemoryItem> source) {
    final InMemoryItem[] items = source.toArray(new InMemoryItem[0]);
    final Accumulator accumulator = new Accumulator();
    for (int i = 0; i < items.length; i++) {
      accumulator.add(items[i], i);
    }
    return accumulator.toIndex(items);
  }

  private static int[] orEmpty(final int[] found) {
    return found == null ? EMPTY : found;
  }

  private static Term indexForm(final Term term) {
    final Term grounded = term.tryGround();
    if (grounded.getTermType() == STRUCT && ((TermStruct) grounded).getArity() == 0) {
      return ((TermStruct) grounded).getFunctor();
    }
    return grounded;
  }

  private static boolean keysReject(final Term left, final Term right) {
    if (left.getTermType() == VAR || right.getTermType() == VAR) {
      return false;
    }
    if (left instanceof NumericTerm || right instanceof NumericTerm) {
      if (left.getClass() != right.getClass()) {
        return true;
      }
      return ((NumericTerm) left).compare((NumericTerm) right) != 0;
    }
    final TermType leftType = left.getTermType();
    final TermType rightType = right.getTermType();
    if (leftType == LIST || rightType == LIST) {
      if (leftType != LIST || rightType != LIST) {
        return true;
      }
      return left.isNullList() != right.isNullList();
    }
    if ((leftType == ATOM || leftType == OPERATOR)
        && (rightType == ATOM || rightType == OPERATOR)) {
      return !left.getText().equals(right.getText());
    }
    if (leftType == STRUCT && rightType == STRUCT) {
      return !left.getSignature().equals(right.getSignature());
    }
    return leftType != rightType;
  }

  private static int[] toInts(final List<Integer> values) {
    if (values.isEmpty()) {
      return EMPTY;
    }
    final int[] result = new int[values.size()];
    for (int i = 0; i < result.length; i++) {
      result[i] = values.get(i);
    }
    return result;
  }

  Iterator<InMemoryItem> select(final TermStruct query) {
    final int[] matched = this.bucket(query);
    if (matched == null) {
      return new ArrayIterator(this.items);
    }
    return new MergeIterator(this.items, matched, this.wildcard);
  }

  private int[] bucket(final TermStruct query) {
    if (query.getArity() == 0) {
      return null;
    }
    final Term formed = indexForm(query.getArgumentAt(0));
    if (formed.getTermType() == VAR) {
      return null;
    }
    if (formed instanceof TermLong) {
      return orEmpty(this.longs.get(formed.toNumber().longValue()));
    }
    if (formed instanceof TermDouble) {
      return orEmpty(this.doubles.get(
          Double.doubleToLongBits(formed.toNumber().doubleValue())));
    }
    switch (formed.getTermType()) {
      case LIST:
        return formed.isNullList() ? this.nils : this.lists;
      case STRUCT:
        return orEmpty(this.structs.get(formed.getSignature()));
      case ATOM:
      case OPERATOR:
        return orEmpty(this.atoms.get(formed.getText()));
      default:
        return null;
    }
  }

  private static final class Accumulator {
    private final Map<String, List<Integer>> atoms = new HashMap<>();
    private final Map<String, List<Integer>> structs = new HashMap<>();
    private final Map<Long, List<Integer>> longs = new HashMap<>();
    private final Map<Long, List<Integer>> doubles = new HashMap<>();
    private final List<Integer> lists = new ArrayList<>();
    private final List<Integer> nils = new ArrayList<>();
    private final List<Integer> wildcard = new ArrayList<>();

    private static <K> Map<K, int[]> freeze(final Map<K, List<Integer>> source) {
      if (source.isEmpty()) {
        return Map.of();
      }
      final Map<K, int[]> frozen = new HashMap<>();
      for (final Map.Entry<K, List<Integer>> entry : source.entrySet()) {
        frozen.put(entry.getKey(), toInts(entry.getValue()));
      }
      return frozen;
    }

    private void add(final InMemoryItem item, final int position) {
      final Term head = item.getHead();
      if (!(head instanceof TermStruct) || ((TermStruct) head).getArity() == 0) {
        this.wildcard.add(position);
        return;
      }
      final Term formed = indexForm(((TermStruct) head).getArgumentAt(0));
      if (formed.getTermType() == VAR) {
        this.wildcard.add(position);
        return;
      }
      if (formed instanceof TermLong) {
        this.addTo(this.longs, formed.toNumber().longValue(), position);
        return;
      }
      if (formed instanceof TermDouble) {
        this.addTo(
            this.doubles,
            Double.doubleToLongBits(formed.toNumber().doubleValue()),
            position);
        return;
      }
      switch (formed.getTermType()) {
        case LIST:
          (formed.isNullList() ? this.nils : this.lists).add(position);
          return;
        case STRUCT:
          this.addTo(this.structs, formed.getSignature(), position);
          return;
        case ATOM:
        case OPERATOR:
          this.addTo(this.atoms, formed.getText(), position);
          return;
        default:
          this.wildcard.add(position);
      }
    }

    private <K> void addTo(final Map<K, List<Integer>> buckets, final K key, final int position) {
      final List<Integer> existing = buckets.get(key);
      if (existing == null) {
        final List<Integer> created = new ArrayList<>();
        created.add(position);
        buckets.put(key, created);
      } else {
        existing.add(position);
      }
    }

    private FirstArgIndex toIndex(final InMemoryItem[] items) {
      return new FirstArgIndex(
          items,
          freeze(this.atoms),
          freeze(this.structs),
          freeze(this.longs),
          freeze(this.doubles),
          toInts(this.lists),
          toInts(this.nils),
          toInts(this.wildcard));
    }
  }

  private static final class ArrayIterator implements Iterator<InMemoryItem> {
    private final InMemoryItem[] items;
    private int index;

    private ArrayIterator(final InMemoryItem[] items) {
      this.items = items;
    }

    @Override
    public boolean hasNext() {
      return this.index < this.items.length;
    }

    @Override
    public InMemoryItem next() {
      if (!this.hasNext()) {
        throw new NoSuchElementException();
      }
      return this.items[this.index++];
    }
  }

  private static final class MergeIterator implements Iterator<InMemoryItem> {
    private final InMemoryItem[] items;
    private final int[] left;
    private final int[] right;
    private int leftIndex;
    private int rightIndex;

    private MergeIterator(final InMemoryItem[] items, final int[] left, final int[] right) {
      this.items = items;
      this.left = left;
      this.right = right;
    }

    @Override
    public boolean hasNext() {
      return this.leftIndex < this.left.length || this.rightIndex < this.right.length;
    }

    @Override
    public InMemoryItem next() {
      if (!this.hasNext()) {
        throw new NoSuchElementException();
      }
      final int position;
      if (this.rightIndex >= this.right.length
          || (this.leftIndex < this.left.length
          && this.left[this.leftIndex] < this.right[this.rightIndex])) {
        position = this.left[this.leftIndex++];
      } else {
        position = this.right[this.rightIndex++];
      }
      return this.items[position];
    }
  }
}
