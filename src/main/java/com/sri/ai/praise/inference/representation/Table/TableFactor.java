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
		//Conventions: 
		//	A = Var(this)      \ Var(another)  
		//	B = Var(another)   \ Var(this)
		//  C = Var(another) cap Var(this)
		//
		// Var(this) = AC; Var(another) = BC
		
		
		List<TableVariable> ABC = new ArrayList<>(listOfVariables);
		List<Integer> ABCCardialities = new ArrayList<>(table.getVariableCardinalities());

		List<Variable> A = new ArrayList<>();
		List<Variable> B = new ArrayList<>();
		List<Variable> C = new ArrayList<>();

		List<Integer> PositionOfAInVarThis = new ArrayList<>();
		List<Integer> PositionOfAInVarABC = new ArrayList<>();
		List<Integer> PositionOfBInVarAnother = new ArrayList<>();
		List<Integer> PositionOfBInVarABC = new ArrayList<>();

		List<Integer> PositionOfCInVarABC = new ArrayList<>();
		
		int i = 0;
		for(TableVariable v : anotherTable.getVariables()) {
			if(this.setOfVariables.contains(v)) {
				//v is in the intersection
				
				C.add(v);
			}
			else {
				ABC.add(v);
				ABCCardialities.add(anotherTable.table.getVariableCardinalities().get(i));
				B.add(v);
			}
			i++;
		}
		for(TableVariable v : this.getVariables()) {
			if(!anotherTable.setOfVariables.contains(v)) {
				A.add(v);
			}
		}
		 
		return null;
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
