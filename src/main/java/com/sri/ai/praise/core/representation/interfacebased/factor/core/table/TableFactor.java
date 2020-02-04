package com.sri.ai.praise.core.representation.interfacebased.factor.core.table;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public interface TableFactor extends Factor {

	@Override
	ArrayList<TableVariable> getVariables();

	void setName(String newName);

	TableFactor slice(Map<TableVariable, Integer> assignment);

	TableFactor slice(List<TableVariable> variables, List<Integer> values);

	Double getEntryFor(Map<? extends Variable, ?> assignment);

	Double ABS_getEntryFor(ArrayList<Integer> values);

	void setEntryFor(Map<? extends Variable, ? extends Integer> assignment, Double newParameterValue);

	void ABS_setEntryFor(ArrayList<Integer> values, Double newParameterValue);

}