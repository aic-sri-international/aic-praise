package com.sri.ai.praise.inference.anytimeexactbp.gabrielstry;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.util.collect.ArrayHashSet;

/**
 * 
 * @author gabriel
 *
 */

public class Bound implements FactorApproximaton{
	
	ArrayList<Factor> factors;
	Set<Variable> variables;
	
	
	Bound(){
		this.factors = new ArrayList<>();
		this.variables = new ArrayHashSet<>();
	}
	
	Bound(ArrayList<Factor> factors){	
		this.factors = factors;

		//TODO
		//Do this:
		for(Factor f: this.factors){
			this.variables.addAll(f.getVariables());
		}
		//or this:
		//CHECK IF THE VARIABLES ARE EQUAL, AND SEND ERROR IF NOT...
	}
	
	@Override
	public boolean contains(Variable variable) {
		boolean result = this.variables.contains(variable);
		return result;
	}

	@Override
	public List<? extends Variable> getVariables() {
		ArrayList<Variable> result = new ArrayList<>();
		result.addAll(variables);
		return result;
	}

	@Override
	public Factor multiply(Factor another) {
		ArrayList<Factor> newFactorsList = new ArrayList<>();
		for(Factor f : factors) {
			newFactorsList.add(f.multiply(another));
		}
		Bound result = new Bound(newFactorsList);
		return result;
	}

	@Override
	public Factor sumOut(List<? extends Variable> variablesToSumOut) {
		ArrayList<Factor> newFactorsList = new ArrayList<>();
		for(Factor f : factors) {
			newFactorsList.add(f.sumOut(variablesToSumOut));
		}
		Bound result = new Bound(newFactorsList);
		return result;
	}

	@Override
	public Factor totalIgnorance(List<? extends Variable> variablesToSumOut) {
		//TODO: for each value of the variable, return a indicator function on that value (like if V = v then 1 else 0)
		return null;
	}

}


/*
public class Bound<T extends Factor> implements FactorApproximaton<T>{
	
	ArrayList<T> factors;
	Set<Variable> variables;
	
	
	Bound(){
		this.factors = new ArrayList<>();
		this.variables = new ArrayHashSet<>();
	}
	
	Bound(ArrayList<T> factors){	
		this.factors = factors;

		//TODO
		//Do this:
		for(T f: this.factors){
			this.variables.addAll(f.getVariables());
		}
		//or this:
		//CHECK IF THE VARIABLES ARE EQUAL, AND SEND ERROR IF NOT...
	}
	
	@Override
	public boolean contains(Variable variable) {
		boolean result = this.variables.contains(variable);
		return result;
	}

	@Override
	public List<? extends Variable> getVariables() {
		ArrayList<Variable> result = new ArrayList<>();
		result.addAll(variables);
		return result;
	}

	@SuppressWarnings("unchecked")
	//Actually, another should be of type T, and so is the returned element 
	// For example, we don't want to multiply a table cpd factor by a factor represented as a Expression...
	// I don't know how to avoid this situation, though...
	// Ass. Gabriel
	@Override
	public Factor multiply(Factor another) {
		ArrayList<T> newFactorsList = new ArrayList<>();
		for(T f : factors) {
			newFactorsList.add((T) f.multiply(another));
		}
		Bound<T> result = new Bound<>(newFactorsList);
		return result;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Factor sumOut(List<? extends Variable> variablesToSumOut) {
		ArrayList<T> newFactorsList = new ArrayList<>();
		for(T f : factors) {
			newFactorsList.add((T) f.sumOut(variablesToSumOut));
		}
		Bound<T> result = new Bound<>(newFactorsList);
		return result;
	}

	@Override
	public T totalIgnorance(List<? extends Variable> variablesToSumOut) {
		//TODO: for each value of the variable, return a indicator function on that value (like if V = v then 1 else 0)
		return null;
	}

}
*/