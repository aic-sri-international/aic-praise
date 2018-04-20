package com.sri.ai.praise.inference.anytimeexactbp.polytope.box;


import static com.sri.ai.praise.inference.anytimeexactbp.polytope.box.TableBoxVariable.TABLE_BOX_VARIABLE;
import static com.sri.ai.praise.inference.representation.Table.TableFactor.getCartesianProduct;
import static com.sri.ai.util.Util.in;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.function.BiFunction;

import com.sri.ai.praise.inference.representation.Table.TableFactor;
import com.sri.ai.praise.inference.representation.Table.TableVariable;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

public class TableFactorBoxBuilder {
	public TableFactor buildBoxFactor(TableFactor factor, Collection<Variable> notFreeVariables) {
		notFreeVariables.retainAll(factor.getVariables());
		notFreeVariables.remove(TABLE_BOX_VARIABLE);
		if(notFreeVariables.isEmpty()) {
			return factor; 
		}
		TableFactor result = buildBoxFactorIfSetIsNotEmpty(factor,notFreeVariables);
		return result;
	}

	private TableFactor buildBoxFactorIfSetIsNotEmpty(TableFactor factor, Collection<Variable> notFreeVariables) {
		LinkedHashSet<Variable> newFactorSetOfVariables = new LinkedHashSet<Variable>(factor.getVariables());
		newFactorSetOfVariables.removeAll(notFreeVariables);
		
		boolean initialFactorIsABox = newFactorSetOfVariables.contains(TABLE_BOX_VARIABLE);
		if(initialFactorIsABox) {
			newFactorSetOfVariables.remove(TABLE_BOX_VARIABLE);
		}

		ArrayList<Variable> listOfFreeVariables = Util.arrayList(TABLE_BOX_VARIABLE);
		listOfFreeVariables.addAll(newFactorSetOfVariables);

		TableFactor newFactorLowHalf; 
		TableFactor newFactorHighHalf;
		if(initialFactorIsABox) {
			Pair<TableFactor, TableFactor> factorHalves = divideABoxFactorIntoTwoHalves(factor);
			TableFactor factorLowHalf  = factorHalves.first;
			TableFactor factorHighHalf = factorHalves.second;

			newFactorLowHalf  = maxOrMinOut(factorLowHalf, listOfFreeVariables,notFreeVariables,(oldValue,newValue)->newValue<oldValue);
			newFactorHighHalf = maxOrMinOut(factorHighHalf,listOfFreeVariables,notFreeVariables,(oldValue,newValue)->(newValue<oldValue));
		}
		else {
			newFactorLowHalf  = maxOrMinOut(factor,listOfFreeVariables, notFreeVariables,(oldValue,newValue)->(newValue<oldValue));
			newFactorHighHalf = maxOrMinOut(factor,listOfFreeVariables, notFreeVariables,(oldValue,newValue)->(newValue<oldValue));
		}
		
		Pair<TableFactor, TableFactor> factorPair = new Pair<>(newFactorLowHalf,newFactorHighHalf);
		
		TableFactor result = makeABoxFactorHavingTheBoxesExtremes(factorPair.first,factorPair.second);
		
		return result;
	}

	private Pair<TableFactor, TableFactor> divideABoxFactorIntoTwoHalves(TableFactor factor) {
		ArrayList<TableVariable> varaibles = (ArrayList<TableVariable>)factor.getVariables();
		
		ArrayList<TableVariable> variablesWithoutBoxVariable;
		TableFactor minValueHalfFactor;
		TableFactor maxValueHalfFactor;
		if(varaibles.get(0).equals(TABLE_BOX_VARIABLE)) {
			variablesWithoutBoxVariable = (ArrayList<TableVariable>) varaibles.subList(1, varaibles.size());
			ArrayList<Double> entries = factor.getEntries();
			int len = entries.size();
			ArrayList<Double> lowHalfEntries  = (ArrayList<Double>) entries.subList(0, len/2 );
			ArrayList<Double> highHalfEntries = (ArrayList<Double>) entries.subList(len/2, len);
			minValueHalfFactor = new TableFactor(variablesWithoutBoxVariable, lowHalfEntries);
			maxValueHalfFactor = new TableFactor(variablesWithoutBoxVariable, highHalfEntries);	
		}
		else {
			Util.println(TABLE_BOX_VARIABLE.toString() + "Should be the first variable, but wasn't");
			variablesWithoutBoxVariable = varaibles;
			
			LinkedHashMap<TableVariable, Integer> mapOfvaluesPredetermined = new LinkedHashMap<>();
			mapOfvaluesPredetermined.put(TABLE_BOX_VARIABLE, 0);
			minValueHalfFactor = copyToSubTable(factor,mapOfvaluesPredetermined);
			mapOfvaluesPredetermined.put(TABLE_BOX_VARIABLE, 1);
			maxValueHalfFactor = copyToSubTable(factor,mapOfvaluesPredetermined);
		}

		Pair<TableFactor, TableFactor> result = new Pair<>(minValueHalfFactor,maxValueHalfFactor);
		return result;
	}

	private TableFactor copyToSubTable(TableFactor factor,
			LinkedHashMap<TableVariable, Integer> mapOfvaluesPredetermined) {
		ArrayList<TableVariable> newVariables = new ArrayList<>(factor.getVariables());
		newVariables.removeAll(mapOfvaluesPredetermined.keySet());
		
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

	private TableFactor maxOrMinOut(TableFactor factorLowHalf, ArrayList<Variable> freeVariables,Collection<Variable> notFreeVariables, 
			BiFunction<Double,Double,Boolean> comparisson) {
		// TODO Auto-generated method stub
		return null;
	}

	private TableFactor makeABoxFactorHavingTheBoxesExtremes(TableFactor first, TableFactor second) {
		// TODO Auto-generated method stub
		return null;
	}
	
	public static void main(String[] args) {
		Util.println(TABLE_BOX_VARIABLE.equals(TABLE_BOX_VARIABLE));

		TableVariable v1 = new TableVariable("v", 2);
		TableVariable v2 = new TableVariable("v", 2);
		Util.println(v1.equals(v2));
		Util.println(v2.equals(v1));
		
	}

	
}
