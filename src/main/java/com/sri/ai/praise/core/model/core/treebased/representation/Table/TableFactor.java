package com.sri.ai.praise.core.model.core.treebased.representation.Table;

import static com.sri.ai.praise.core.model.core.treebased.representation.core.IdentityFactor.IDENTITY_FACTOR;
import static com.sri.ai.util.Util.accumulate;
import static com.sri.ai.util.Util.arrayListFilledWith;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import com.sri.ai.praise.core.model.core.treebased.representation.api.Factor;
import com.sri.ai.praise.core.model.core.treebased.representation.api.Variable;
import com.sri.ai.praise.core.model.core.treebased.representation.core.IdentityFactor;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.collect.CartesianProductIterator;
import com.sri.ai.util.math.MixedRadixNumber;

/**
 * @author gabriel
 *
 */

public class TableFactor implements Factor{
	ArrayList<TableVariable> listOfVariables;
	ArrayList<Integer> listOfVariableCardinalities;
	LinkedHashSet<TableVariable> setOfVariables;
	//
	ArrayList<Double> entries;
	MixedRadixNumber entryIndex;
	//
	LinkedHashMap<TableVariable, Integer> mapFromVariableToItsIndexOnTheList;//TODO initialize	
	
	public TableFactor(ArrayList<TableVariable> listOfVariables,ArrayList<Double> entries) {
		this.listOfVariables =listOfVariables;
		this.setOfVariables = new LinkedHashSet<>(listOfVariables);
		
		this.mapFromVariableToItsIndexOnTheList = new LinkedHashMap<>();
		for (int i = 0; i < listOfVariables.size(); i++) {
			this.mapFromVariableToItsIndexOnTheList.put(listOfVariables.get(i),i);
		}
		this.listOfVariableCardinalities = Util.mapIntoArrayList(listOfVariables, v->v.getCardinality());
		this.entries = entries;
		
		entryIndex = new MixedRadixNumber(BigInteger.ZERO, listOfVariableCardinalities);
	}
	
	public TableFactor(ArrayList<TableVariable> listOfVariables) {
		this(listOfVariables,arrayListFilledWith(-1., numEntries(listOfVariables)));
	}
	
	public static int numEntries(ArrayList<TableVariable> listOfVariables2) {
		int result = 1;
		for(TableVariable v:listOfVariables2) {
			result*=v.getCardinality();
		}
		return result;
	}

	@Override
	public boolean contains(Variable variable) {
		boolean res = setOfVariables.contains(variable);
		return res;
	}
	@Override
	public ArrayList<TableVariable> getVariables() {
		return this.listOfVariables;
	}

	@Override
	public boolean isIdentity() {
		if(entries.size() == 0 || entries.get(0) == 0) {
			return false;	
		}
		double valueAtZero = entries.get(0);
		for(Double v : entries) {
			if (v != valueAtZero) {
				return false;
			}
		}
		return true;
	}
	
	@Override
	public String toString() {
	
		String result = "phi(";
		
		boolean first = true;
		for(TableVariable v : this.getVariables()) {
			if(first) {
				first = false;
			}
			else {
				result = result + ", ";
			}
			result = result + v.getName();
		}
		result = result + ") = ";
		
		for(Double entry : entries) {
			result = result + entry + " ";
		}
		
	/*	String result = "";
		MixedRadixNumber radix = new MixedRadixNumber(BigInteger.ZERO,fillWithCardinality(listOfVariables));
		
		for(int j = 0; j < table.numberEntries();j++) {
			if(j != 0) {
				result = result + " | ";
			}
			int nCols = listOfVariables.size();
			String s = "";
			for (int i = 0; i < nCols; i++) {
				s = s + " " + radix.getCurrentNumeralValue(i);
			}
			radix.increment();
			result = result + s + " : " + table.getEntries().get(j);
		}*/
		return result;
	}

	public TableFactor normalize() {
		
		Double sum = 0.;
		for(Double entry : this.entries) {
			sum = sum + entry;
		}
		if(sum == 0.) {
			return this;
		}
		
		ArrayList<Double> newEntries = new ArrayList<>(entries.size());
		for (Double entry :entries) {
			newEntries.add(entry/sum);
		}
		TableFactor result = new TableFactor(listOfVariables, newEntries);
		return result ;
	}

	@Override
	public Double getEntryFor(Map<? extends Variable, ? extends Object> variableValues) {
		int entryPosition = fromMapOfVariableValuesToEntryPosition(variableValues);
		return entries.get(entryPosition);
	}
	
	public <T extends Variable, U extends Object> Double getEntryFor(List<T> variables, List<U> variableValues ) {
		Map<T, U> map = Util.mapFromListOfKeysAndListOfValues(variables, variableValues);
		return getEntryFor(map);
	}
	
	public void setEntryFor(Map<? extends Variable, ? extends Object> variableValues, Double newEntryValue) {
		int entryPosition = fromMapOfVariableValuesToEntryPosition(variableValues);
		entries.set(entryPosition, newEntryValue);
	}
	
	public int fromMapOfVariableValuesToEntryPosition(Map<? extends Variable, ? extends Object> variableValues) {
		int[] varValues = new int[listOfVariables.size()];
		/*for(Entry<? extends Variable, ? extends Object> entry : variableValues.entrySet()) {//iterate over listOfVariavles:moreEfficient
			Integer indexOnTheList = mapFromVariableToItsIndexOnTheList.get(entry.getKey());
			if(indexOnTheList != null) {
				varValues[indexOnTheList] = (Integer) entry.getValue();
			}
		}*/
		for (int i = 0; i < varValues.length; i++) {
			varValues[i] = (Integer) variableValues.get(listOfVariables.get(i));
		}
		
		int entryPosition = this.entryIndex.getValueFor(varValues).intValue();
		return entryPosition;
	}

	@Override
	public Factor multiply(Factor another) {
		//Check if the class is the same
		if(another.getClass() == IdentityFactor.class) {
			return this;
		}
		
		if(another.getClass() != this.getClass()) {
			Util.println("Trying to multiply different types of factors: this is a " +
							this.getClass() + "and another is a " + another.getClass());
			return null;
		}

		TableFactor anotherTable = (TableFactor)another;
		
		ArrayList<TableVariable> newListOfVariables = new ArrayList<>(this.listOfVariables);
		for(TableVariable v : anotherTable.getVariables()) {
			if(!this.setOfVariables.contains(v)) {
				newListOfVariables.add(v);
			}
		}

		Integer numberOfEntries= accumulate(mapIntoList(newListOfVariables, v->v.getCardinality()),
												(i,j)->i*j, 1);
		ArrayList<Double> newEntries = new ArrayList<>(numberOfEntries);
		for (int i = 0; i < numberOfEntries; i++) {
			newEntries.add(-1.);
		}
		
		TableFactor result = new TableFactor(newListOfVariables, newEntries);
		Iterator<ArrayList<Integer>> cartesianProduct = getCartesianProduct(newListOfVariables);
		
		LinkedHashMap<Variable, Integer> mapFromVariableToValue = new LinkedHashMap<>();
		for(ArrayList<Integer> values: in(cartesianProduct)) {
			for (int i = 0; i < values.size(); i++) {
				mapFromVariableToValue.put(newListOfVariables.get(i), values.get(i));
			}
			Double product = this.getEntryFor(mapFromVariableToValue) * anotherTable.getEntryFor(mapFromVariableToValue);
			result.setEntryFor(mapFromVariableToValue, product);
		}
		return result;
	}


	public static Iterator<ArrayList<Integer>> getCartesianProduct(ArrayList<TableVariable> listOfVariables) {
		
		ArrayList<ArrayList<Integer>> listOfValuesForTheVariables = mapIntoArrayList(listOfVariables, 
																	v -> makeArrayWithValuesFromZeroToNMinusOne(v.getCardinality()));
		ArrayList<NullaryFunction<Iterator<Integer>>> iteratorForListOfVariableValues = 
				mapIntoArrayList(listOfValuesForTheVariables, element -> () -> element.iterator());
		
		Iterator<ArrayList<Integer>> cartesianProduct = new CartesianProductIterator<Integer>(iteratorForListOfVariableValues);
		return cartesianProduct;
	}

	private static ArrayList<Integer> makeArrayWithValuesFromZeroToNMinusOne(int cardinality) {
		ArrayList<Integer> result = new ArrayList<>(cardinality);
		for (int i = 0; i < cardinality; i++) {
			result.add(i);
		}
		return result;
	}

	@Override
	public Factor sumOut(List<? extends Variable> variablesToSumOut) {
		LinkedHashSet<TableVariable> setOfVariablesToSumOut = new LinkedHashSet<>();
		for(Variable v : variablesToSumOut) {
			setOfVariablesToSumOut.add((TableVariable) v);
		}
		ArrayList<TableVariable> variablesNotToSumOut = new ArrayList<>();
		for(TableVariable v: this.listOfVariables) {
			if(!setOfVariablesToSumOut.contains(v)) {
				variablesNotToSumOut.add(v);
			}
		}
		
		if(variablesNotToSumOut.isEmpty()) {
			return IDENTITY_FACTOR;
		}
		
		Integer numberOfEntries= accumulate(mapIntoList(variablesNotToSumOut, v->v.getCardinality()),(i,j)->i*j, 1);
		
		ArrayList<Double> entries = new ArrayList<>(numberOfEntries);
		for (int i = 0; i < numberOfEntries; i++) {
			entries.add(0.);
		}
		
		TableFactor result = new TableFactor(variablesNotToSumOut, entries);
		
		Iterator<ArrayList<Integer>> cartesianProduct = getCartesianProduct(this.listOfVariables);
		LinkedHashMap<Variable, Integer> mapFromVariableToValue = new LinkedHashMap<>();
		for(ArrayList<Integer> values: in(cartesianProduct)) {
			for (int i = 0; i < values.size(); i++) {
				mapFromVariableToValue.put(this.listOfVariables.get(i), values.get(i));
			}
			Double currentValue = result.getEntryFor(mapFromVariableToValue);
			Double addedValue = this.getEntryFor(mapFromVariableToValue);
			result.setEntryFor(mapFromVariableToValue, currentValue + addedValue);
		}
		return result;
	}
	
	public ArrayList<Double> getEntries() {
		return this.entries;
	}
	
	
	public static TableFactor copyToSubTableFactor(TableFactor factor,
			List<TableVariable> variablesPredetermined,List<Integer> valuesPredetermined) {
		
		Map<TableVariable, Integer> mapOfvaluesPredetermined = Util.mapFromListOfKeysAndListOfValues(variablesPredetermined, valuesPredetermined);
		TableFactor result = copyToSubTableFactorWithoutRecreatingANewMap(factor, mapOfvaluesPredetermined);
		return result;
	}
	
	public static TableFactor copyToSubTableFactor(TableFactor factor,
			Map<TableVariable, Integer> mapOfvaluesPredetermined) {

		Map<TableVariable, Integer> map2= new LinkedHashMap<>(mapOfvaluesPredetermined);
		TableFactor result = copyToSubTableFactorWithoutRecreatingANewMap(factor, map2);
		
		return result;
	}
	
	private static TableFactor copyToSubTableFactorWithoutRecreatingANewMap(TableFactor factor,
			Map<TableVariable, Integer> mapOfvaluesPredetermined) {
		ArrayList<TableVariable> newVariables = new ArrayList<>(factor.getVariables());
		
		newVariables.removeAll(mapOfvaluesPredetermined.keySet());
		if(newVariables.size() == 0) {
			return null;
		}
		Iterator<ArrayList<Integer>> cartesianProduct = getCartesianProduct(newVariables);
		
		TableFactor result = new TableFactor(newVariables);
		for(ArrayList<Integer> instantiations: in(cartesianProduct)) {
			for (int i = 0; i < newVariables.size(); i++) {
				mapOfvaluesPredetermined.put(newVariables.get(i), instantiations.get(i));
			}
			Double newEntryValue =  factor.getEntryFor(mapOfvaluesPredetermined);
			result.setEntryFor(mapOfvaluesPredetermined, newEntryValue);
		}
		return result;
	}
}
