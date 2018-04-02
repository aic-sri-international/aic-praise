package com.sri.ai.praise.inference.representation.Table;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.praise.lang.grounded.common.FunctionTable;

/**
 * @author gabriel
 *
 */

public class TableFactor implements Factor{
	List<TableVariable> listOfVariables;
	LinkedHashSet<TableVariable> setOfVariables;
	FunctionTable table;
	
	
	public TableFactor(List<TableVariable> listOfVariables, FunctionTable table) {
		this.listOfVariables =listOfVariables;
		this.setOfVariables = new LinkedHashSet<>(listOfVariables);
		this.table = table;
	}
	
	@Override
	public boolean contains(Variable variable) {
		boolean res = listOfVariables.contains(variable);
		return res;
	}
	@Override
	public List<TableVariable> getVariables() {
		ArrayList<TableVariable> res = new ArrayList<>(listOfVariables);
		return res;
	}
	@Override
	public Factor multiply(Factor another) {
		//Check if the class is the same
		if(another.getClass() != this.getClass()) {
			//TODO error message
		}
		
		TableFactor anotherTable = (TableFactor)another;
		
		// Add the variables not there...
		
		// Maybe create a new factor. 
		
		//criar uma nova lista de variaveis. achar a intercessao entra as posicoes da main e da nova
		List<TableVariable> newVariables = new ArrayList<>(listOfVariables);
		List<Integer> newVariablesCardialities = new ArrayList<>(table.getVariableCardinalities());
		
		List<Variable> intersection = new ArrayList<>();
		List<Variable> onlyInNew = new ArrayList<>();
		
		int i = 0;
		for(Variable v : anotherTable.getVariables()) {
			if(this.setOfVariables.contains(v)) {
				//v is in the intersection
				intersection.add(v);
			}
			else {
				newVariables.add((TableVariable)v);
				newVariablesCardialities.add(anotherTable.getCardinality(i));
				onlyInNew.add(v);
			}
			i++;
		}
		
		//for each instantiation at only in new
		//	for each instantiation of old
		//		preencher a entrada la 
		return null;
	}
	
	
	public Integer getCardinality(int i) {
		return this.table.getVariableCardinalities().get(i);
	}
	
	
	@Override
	public Factor sumOut(List<? extends Variable> variablesToSumOut) {
		// TODO Auto-generated method stub
		return null;
	}
	@Override
	public boolean isIdentity() {
		List<Double> entries = table.getEntries();
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
	
}
