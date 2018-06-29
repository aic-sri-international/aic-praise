package com.sri.ai.praise.learning.parameterlearning.representation.dataset;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.learning.parameterlearning.Datapoint;

public class DefaultDatapoint implements Datapoint {
	
	private HashMap<Variable, Object> variablesAndTheirValues;
	
	public DefaultDatapoint(List<? extends Variable> variables, List<? extends Object> variablesValues) {
		if(variables.size() != variablesValues.size()) {
			throw new Error("The lists Variables and VariablesValues must have the same size.");
		}
		
		variablesAndTheirValues = new HashMap<Variable, Object>();
		
		Iterator<? extends Variable> variablesIterator = variables.iterator();
		Iterator<? extends Object> valuesIterator = variablesValues.iterator();
		while(variablesIterator.hasNext()) {
			variablesAndTheirValues.put(variablesIterator.next(), valuesIterator.next());
		}
	}
	
	@Override
	public Object getValueOfVariable(Variable variable) {
		if(!variablesAndTheirValues.containsKey(variable)) {
			throw new Error("Variable " + variable + " is not present on the datatpoint.");
		}
		return variablesAndTheirValues.get(variable);
	}

	@Override
	public List<? extends Object> getValuesOfVariables(List<? extends Variable> variables) {
		ArrayList<Object> valueOfVariables = new ArrayList<Object>();
		for(Variable variable : variables) {
			Object valueOfCurrentVariable = getValueOfVariable(variable);
			valueOfVariables.add(valueOfCurrentVariable);
		}
		return valueOfVariables;
	}

}
