package com.sri.ai.praise.inference.representation.Table;

import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.mapIntoArrayList;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.praise.lang.grounded.common.FunctionTable;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.collect.CartesianProductIterator;
import com.sri.ai.util.math.MixedRadixNumber;

/**
 * @author gabriel
 *
 */

public class TableFactor implements Factor{
	List<TableVariable> listOfVariables;
	LinkedHashSet<TableVariable> setOfVariables;
	FunctionTable table;
	//
	Map<TableVariable, Integer> mapFromVariableToItsIndexOnTheList;//TODO initialize	
	
	public TableFactor(ArrayList<TableVariable> listOfVariables, FunctionTable table) {
		this.listOfVariables =listOfVariables;
		this.setOfVariables = new LinkedHashSet<>(listOfVariables);
		this.table = table;
		
		mapFromVariableToItsIndexOnTheList = new LinkedHashMap<>();
		for (int i = 0; i < listOfVariables.size(); i++) {
			mapFromVariableToItsIndexOnTheList.put(listOfVariables.get(i),i);
		}
	}
	
	public TableFactor(ArrayList<TableVariable> listOfVariables, FunctionTable table, 
			LinkedHashMap<TableVariable, Integer> mapFromVariableToItsIndexOnTheList) {
		this.listOfVariables =listOfVariables;
		this.setOfVariables = new LinkedHashSet<>(listOfVariables);
		this.table = table;
		this.mapFromVariableToItsIndexOnTheList = mapFromVariableToItsIndexOnTheList;
	}
	
	@Override
	public boolean contains(Variable variable) {
		boolean res = setOfVariables.contains(variable);
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
			Util.println("Trying to multiply different types of factors: this is a " +
							this.getClass() + "and another is a " + another.getClass());
		}
		
		TableFactor anotherTable = (TableFactor)another;
		/*Conventions:
		 * 	A = Var(this)      \ Var(another)  
		 *  B = Var(another)   \ Var(this)
		 *  C = Var(another) cap Var(this)
		 *  In other words:  	Var(this) = AC  ; Var(another) = BC
		 *  OR: 				this = phi(A,C) ; another = phi(B,C), returned type = phi(ABC)
		 */
		
		//Defining A, listA and cardA (list with the cardinalities of A)
		Set<TableVariable> A = new LinkedHashSet<>(this.setOfVariables);
		A.removeAll(anotherTable.setOfVariables);
		List<TableVariable> listA = new ArrayList<>(A);
		List<Integer> cardA = fillWithCardinality(listA);
		//Defining B, listB and cardB
		Set<TableVariable> B = new LinkedHashSet<>(anotherTable.setOfVariables);
		B.removeAll(this.setOfVariables);
		List<TableVariable> listB = new ArrayList<>(B);
		List<Integer> cardB = fillWithCardinality(listB);
		//Defining C, listC and card
		Set<TableVariable> C = new LinkedHashSet<>(anotherTable.setOfVariables);
		C.removeAll(B);
		List<TableVariable> listC = new ArrayList<>(C);
		List<Integer> cardC = fillWithCardinality(listC);
		LinkedHashMap<TableVariable, Integer> mapFromVariablesAtABCToItsIdxs = new LinkedHashMap<>();
		for(int i = 0;i<listC.size();i++) {
			mapFromVariablesAtABCToItsIdxs.put(listC.get(i), i);
		}
		
		//Entry index allows to a find the entry (probability) for a specific instance
		//It is tricky without it because the entries are stored as a list
		MixedRadixNumber entryIndexA = new MixedRadixNumber(BigInteger.ZERO, cardA);
		MixedRadixNumber entryIndexB = new MixedRadixNumber(BigInteger.ZERO, cardB);
		MixedRadixNumber entryIndexC = new MixedRadixNumber(BigInteger.ZERO, cardC);
		
		//Initializing entryC with an empty array
		Integer nEntriesC =1;
		for(Integer card : cardC) {
			nEntriesC = nEntriesC * card;
		}
		Double[] entryC = new Double[nEntriesC];
		
		// For each instance of A,B,C (noted as iA,iB,iC) we do \phi(iA,iB,iC) = \phi(iB,iC) * \phi(iA,iC)
		while(entryIndexA.increment()) {
			while(entryIndexB.increment()) {
				while(entryIndexC.increment()) {
					ArrayList<Integer> instantiationAtAC = new ArrayList<>(this.listOfVariables.size());
					
					transferInstantiationsAtOneSubListToAFullList(this.mapFromVariableToItsIndexOnTheList,
							listA, entryIndexA, instantiationAtAC);
					transferInstantiationsAtOneSubListToAFullList(this.mapFromVariableToItsIndexOnTheList,
							listC, entryIndexC, instantiationAtAC);
					
					Double entryAtThis = this.table.entryFor(instantiationAtAC);
					
					ArrayList<Integer> instantiationAtBC = new ArrayList<>(anotherTable.listOfVariables.size());
					transferInstantiationsAtOneSubListToAFullList(anotherTable.mapFromVariableToItsIndexOnTheList,
							listB, entryIndexB, instantiationAtBC);
					
					transferInstantiationsAtOneSubListToAFullList(anotherTable.mapFromVariableToItsIndexOnTheList,
							listC, entryIndexC, instantiationAtBC);
					Double entryAtAnother = anotherTable.table.entryFor(instantiationAtBC);
					
					ArrayList<Integer> instantiationAtABC = new ArrayList<>(listC.size());
					transferInstantiationsAtOneSubListToAFullList(mapFromVariablesAtABCToItsIdxs,
							listA, entryIndexA, instantiationAtABC);
					transferInstantiationsAtOneSubListToAFullList(mapFromVariablesAtABCToItsIdxs,
							listB, entryIndexB, instantiationAtABC);
					transferInstantiationsAtOneSubListToAFullList(mapFromVariablesAtABCToItsIdxs,
							listC, entryIndexC, instantiationAtABC);
					
					Double product = entryAtThis * entryAtAnother;
					
					int[] instantiationABCArray = new int[listC.size()];
					for (int i = 0; i < instantiationABCArray.length; i++) {
						instantiationABCArray[i] = instantiationAtABC.get(i);
					}
					
					entryC[entryIndexC.getValueFor(instantiationABCArray).intValue()] = product;
				}
				entryIndexC = new MixedRadixNumber(BigInteger.ZERO, cardC);
			}
			entryIndexB = new MixedRadixNumber(BigInteger.ZERO, cardB);
		}
		
		return null;
	}

	private void transferInstantiationsAtOneSubListToAFullList(Map<TableVariable,Integer> mapFromVariableToItsIndexOnTheList,
			List<TableVariable> partialListOfVariables, MixedRadixNumber entryIndexAtPartialList,
			ArrayList<Integer> listOfInstantiationsAtFullList) {
		for (int i = 0; i < partialListOfVariables.size(); i++) {
			Integer valueAtI = entryIndexAtPartialList.getCurrentNumeralValue(i);
			Variable varAtI = partialListOfVariables.get(i);
			Integer indexOfVarAtI = mapFromVariableToItsIndexOnTheList.get(varAtI);
			listOfInstantiationsAtFullList.add(indexOfVarAtI,valueAtI);
		}
	}

	private List<Integer> fillWithCardinality(List<TableVariable> list) {
		List<Integer> card = new ArrayList<>();
		for(TableVariable v:list) {
			card.add(v.getCardinality());
		}
		return card;
	}

	@Override
	public Factor sumOut(List<? extends Variable> variablesToSumOut) {
		// redo this function using value "for"
		//remove var not in set of variables...
		//Check if is Table Variable
		if(variablesToSumOut == null || variablesToSumOut.isEmpty()) {
			try {
				return (Factor) this.clone();
			} catch (CloneNotSupportedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		if(!(variablesToSumOut.get(0) instanceof TableVariable)){
			//TODO error message
		}
		List<List<Integer>> listOfInstantiationsForTheVariablesToSumOut = getListOfListOfInstantiations(variablesToSumOut);
		//List<Integer> cardinalitiesOfVariablesToSumOut = getCardinalitiesOfVarToSumOut(variablesToSumOut);
		List<Integer> positionOfEachVariableOnTheListOfVariablesToSumOut = getPositionOfEachVar(variablesToSumOut);		
		Iterator<ArrayList<Integer>> cartesianProductOfVariablesToSumOut = getCartesianProductWithValuesOfVariablesToSum(listOfInstantiationsForTheVariablesToSumOut);		
		//
		ArrayList<TableVariable> variablesNotToSumOut = getVariablesNotToSumOut(variablesToSumOut);
		List<List<Integer>> listOfInstantiationsForTheVariablesNotToSumOut = getListOfListOfInstantiations(variablesNotToSumOut);
		List<Integer> cardinalitiesOfVariablesNotToSumOut = getCardinalitiesOfVarToSumOut(variablesNotToSumOut);
		List<Integer> positionOfEachVariableOnTheListOfVariablesNotToSumOut = getPositionOfEachVar(variablesToSumOut);		
		Iterator<ArrayList<Integer>> cartesianProductOfVariablesNotToSumOut = getCartesianProductWithValuesOfVariablesToSum(listOfInstantiationsForTheVariablesNotToSumOut);		
		
		List<Double> entries = new ArrayList<>();
		for(List<Integer> instantiationOfVariablesNotToSumOut : in(cartesianProductOfVariablesNotToSumOut)) {
			Double summedEntry = 0.;
			for(List<Integer> instantiationOfVariablesToSumOut : in(cartesianProductOfVariablesToSumOut)) {
				
				List<Integer> instantiationOnListOfAllValues = mappingInstantiationsIntoOneInstantiationAtTheWholeListOfVariables(
						positionOfEachVariableOnTheListOfVariablesToSumOut,
						positionOfEachVariableOnTheListOfVariablesNotToSumOut, 
						instantiationOfVariablesNotToSumOut,
						instantiationOfVariablesToSumOut);
				
				Double entry = this.table.entryFor(instantiationOnListOfAllValues);
				summedEntry = summedEntry + entry;
			}
			entries.add(summedEntry);
		}
		
		FunctionTable resultTable= new FunctionTable(cardinalitiesOfVariablesNotToSumOut, entries);
		
		Factor result = new TableFactor(variablesNotToSumOut, resultTable);
		return result;
	}

	private List<Integer> mappingInstantiationsIntoOneInstantiationAtTheWholeListOfVariables(
			List<Integer> positionOfEachVariableOnTheListOfVariablesToSumOut,
			List<Integer> positionOfEachVariableOnTheListOfVariablesNotToSumOut,
			List<Integer> instantiationOfVariablesNotToSumOut, List<Integer> instantiationOfVariablesToSumOut) {
		Integer[] varValues = new Integer[this.listOfVariables.size()];//This is an instantiation in the list of all variables (bigList)
		for(int i = 0;i < instantiationOfVariablesNotToSumOut.size();i++){
			int correpondentPositionAtBigList = positionOfEachVariableOnTheListOfVariablesNotToSumOut.get(i);
			varValues[correpondentPositionAtBigList]=instantiationOfVariablesNotToSumOut.get(i);
		}
		for(int i = 0;i < instantiationOfVariablesToSumOut.size();i++){
			int correpondentPositionAtBigList = positionOfEachVariableOnTheListOfVariablesToSumOut.get(i);
			varValues[correpondentPositionAtBigList]=instantiationOfVariablesToSumOut.get(i);
		}
		
		List<Integer> instantiationOnListOfAllValues = Arrays.asList(varValues);
		return instantiationOnListOfAllValues;
	}

	private ArrayList<TableVariable> getVariablesNotToSumOut(List<? extends Variable> variablesToSumOut) {
		ArrayList<TableVariable> listOfVariablesNotToSumOut = new ArrayList<>();
		Set<Variable> setOfvariablesToSumOut = new LinkedHashSet<>(variablesToSumOut);
		for(TableVariable v :listOfVariables) {
			if(!setOfvariablesToSumOut.contains(v)) {
				listOfVariablesNotToSumOut.add(v);
			}
		}
		return listOfVariablesNotToSumOut;
	}

	private List<Integer> getPositionOfEachVar(List<? extends Variable> variablesToSumOut) {
		List<Integer> positionOfEachVariableOnTheList = new ArrayList<>();
		for(Variable v:variablesToSumOut) {
			positionOfEachVariableOnTheList.add(mapFromVariableToItsIndexOnTheList.get(v));
		}
		return positionOfEachVariableOnTheList;
	}

	private List<Integer> getCardinalitiesOfVarToSumOut(List<? extends Variable> variablesToSumOut) {
		List<Integer> cardinalitiesOfVariablesToSumOut = new ArrayList<>();
		for(Variable v:variablesToSumOut) {
			int variableCardinality = ((TableVariable)v).getCardinality();
			cardinalitiesOfVariablesToSumOut.add(variableCardinality);
		}
		return cardinalitiesOfVariablesToSumOut;
	}

	private List<List<Integer>> getListOfListOfInstantiations(List<? extends Variable> variablesToSumOut) {
		List<List<Integer>> listOfValuesForTheVariables = new ArrayList<>();
		for(Variable v:variablesToSumOut) {
			int variableCardinality = ((TableVariable)v).getCardinality();
			ArrayList<Integer> l = new ArrayList<>();
			listOfValuesForTheVariables.add(l);
			for(int i = 0;i<variableCardinality;i++) {
				l.add(i);
			}
		}
		return listOfValuesForTheVariables;
	}

	private Iterator<ArrayList<Integer>> getCartesianProductWithValuesOfVariablesToSum(List<List<Integer>> listOfValuesForTheVariables) {
		
		ArrayList<NullaryFunction<Iterator<Integer>>> iteratorForListOfVariableValues = 
				mapIntoArrayList(listOfValuesForTheVariables, element -> () -> element.iterator());
		
		Iterator<ArrayList<Integer>> cartesianProduct = new CartesianProductIterator<Integer>(iteratorForListOfVariableValues);
		return cartesianProduct;
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
	
	@Override
	public String toString() {
		String result = "";
		MixedRadixNumber radix = new MixedRadixNumber(BigInteger.ZERO,fillWithCardinality(listOfVariables));
		
		for(int j = 0; j < table.numberEntries();j++) {
			if(j != 0) {
				result = result + " | ";
			}
			int nCols = listOfVariables.size();
			String s = "";
			for (int i = 0; i < nCols; i++) {
				s = radix.getCurrentNumeralValue(i) +" "+ s;
			}
			radix.increment();
			result = result + s + " : " + table.getEntries().get(j);
		}
		return result;
	}
	
}
