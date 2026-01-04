package com.igormaznitsa.jprol.data;

import static java.util.stream.Collectors.toMap;

import java.util.Map;
import java.util.Spliterator;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public abstract class CompoundTerm extends Term implements Iterable<Term> {
  CompoundTerm(final String text, final Object payload, final SourcePosition sourcePosition) {
    super(text, payload, sourcePosition);
  }

  public Stream<Term> streamChildren() {
    return StreamSupport.stream(this.spliteratorChildren(), false);
  }

  public abstract Spliterator<Term> spliteratorChildren();

  @Override
  public Map<String, TermVar> findAllNamedVariables() {
    return this.variables()
        .collect(
            toMap(TermVar::getText,
                e -> e,
                (v1, v2) -> v2
            )
        );
  }

  @Override
  public Map<String, Term> findAllNamedVariableValues() {
    return this.variables()
        .filter(v -> !v.isUnground())
        .collect(toMap(TermVar::getText, e -> e.<Term>tryGroundOrDefault(e), (v1, v2) -> v2));
  }
}
