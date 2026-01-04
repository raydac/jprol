package com.igormaznitsa.jprol.jsr223;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Objects;
import java.util.stream.Collectors;

public final class NamedList implements List<Object> {

  private final String name;
  private final List<Object> objects;

  private NamedList(final String name, final List<Object> objects) {
    this.name = Objects.requireNonNull(name, "Name must not be null");
    this.objects = List.copyOf(objects);
  }

  public static NamedList namedListOf(final String name, final List<Object> objects) {
    return new NamedList(name, objects);
  }

  public static NamedList namedListOf(final String name, final Object... objects) {
    return new NamedList(name, Arrays.asList(objects));
  }

  public String getName() {
    return this.name;
  }

  @Override
  public int size() {
    return this.objects.size();
  }

  @Override
  public boolean isEmpty() {
    return this.objects.isEmpty();
  }

  @Override
  public boolean contains(Object o) {
    return this.objects.contains(o);
  }

  @Override
  public Iterator<Object> iterator() {
    return this.objects.iterator();
  }

  @Override
  public Object[] toArray() {
    return this.objects.toArray();
  }

  @Override
  public <T> T[] toArray(T[] a) {
    return this.objects.toArray(a);
  }

  @Override
  public boolean add(Object object) {
    throw new UnsupportedOperationException("Unmodifable list");
  }

  @Override
  public boolean remove(Object o) {
    throw new UnsupportedOperationException("Unmodifable list");
  }

  @Override
  public boolean containsAll(Collection<?> c) {
    return this.objects.containsAll(c);
  }

  @Override
  public boolean addAll(Collection<?> c) {
    throw new UnsupportedOperationException("Unmodifable list");
  }

  @Override
  public boolean addAll(int index, Collection<?> c) {
    throw new UnsupportedOperationException("Unmodifable list");
  }

  @Override
  public boolean removeAll(Collection<?> c) {
    throw new UnsupportedOperationException("Unmodifable list");
  }

  @Override
  public boolean retainAll(Collection<?> c) {
    throw new UnsupportedOperationException("Unmodifable list");
  }

  @Override
  public void clear() {
    throw new UnsupportedOperationException("Unmodifable list");
  }

  @Override
  public Object get(final int index) {
    return this.objects.get(index);
  }

  @Override
  public Object set(int index, Object element) {
    throw new UnsupportedOperationException("Unmodifable list");
  }

  @Override
  public void add(int index, Object element) {
    throw new UnsupportedOperationException("Unmodifable list");
  }

  @Override
  public Object remove(int index) {
    throw new UnsupportedOperationException("Unmodifable list");
  }

  @Override
  public int indexOf(Object o) {
    return this.objects.indexOf(o);
  }

  @Override
  public int lastIndexOf(Object o) {
    return this.objects.lastIndexOf(o);
  }

  @Override
  public ListIterator<Object> listIterator() {
    return this.objects.listIterator();
  }

  @Override
  public ListIterator<Object> listIterator(int index) {
    return this.objects.listIterator(index);
  }

  @Override
  public List<Object> subList(int fromIndex, int toIndex) {
    return this.objects.subList(fromIndex, toIndex);
  }

  @Override
  public int hashCode() {
    return this.objects.hashCode();
  }

  @Override
  public boolean equals(final Object object) {
    if (object == null) {
      return false;
    }
    if (this == object) {
      return true;
    }
    if (object instanceof NamedList) {
      final NamedList that = (NamedList) object;
      return this.name.equals(that.name) && this.objects.equals(that.objects);
    }
    return false;
  }

  @Override
  public String toString() {
    return this.name
        + this.objects.stream().map(String::valueOf).collect(Collectors.joining(",", "[", "]"));
  }
}
