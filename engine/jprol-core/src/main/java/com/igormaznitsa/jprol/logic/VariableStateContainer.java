package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermVar;
import java.util.Map;

final class VariableStateContainer {
  final TermVar variable;
  final Term sampleValue;

  VariableStateContainer(
      final TermVar var,
      final Map<String, Term> predefinedValues
  ) {
    this.variable = var;

    if (predefinedValues == null) {
      this.sampleValue = var.getImmediateValue();
    } else {
      final Term predefined = predefinedValues.get(var.getText());
      this.sampleValue = predefined == null ? var.getImmediateValue() : predefined;
    }
  }

  void reset() {
    this.variable.setImmediateValue(this.sampleValue);
  }

  boolean isChanged() {
    return this.variable.getImmediateValue() != this.sampleValue;
  }
}
