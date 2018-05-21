package com.sri.ai.praise.inference.generic.anytime.polytope.box;


import static com.sri.ai.praise.inference.generic.anytime.polytope.box.TableBoxVariable.TABLE_BOX_VARIABLE;
import static com.sri.ai.praise.inference.generic.representation.Table.TableFactor.copyToSubTableFactor;
import static com.sri.ai.praise.inference.generic.representation.Table.TableFactor.getCartesianProduct;
import static com.sri.ai.praise.inference.generic.representation.Table.TableFactor.numEntries;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.arrayListFilledWith;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.println;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.function.BiFunction;

import com.sri.ai.praise.inference.generic.anytime.polytope.core.IntensionalConvexHullOfFactors;
import com.sri.ai.praise.inference.generic.representation.Table.TableFactor;
import com.sri.ai.praise.inference.generic.representation.Table.TableVariable;
import com.sri.ai.praise.inference.generic.representation.api.Variable;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

public class TableFactorBoxBuilder {
	
	/*public static Box boxIt(IntensionalConvexHullOfFactors polytope) {
		TableFactor factor = buildBoxFactor(polytope);
		Box result = new Box(TABLE_BOX_VARIABLE, factor);
		return result;
	}
	
	public static TableFactor buildBoxFactor(IntensionalConvexHullOfFactors polytope) {
		ArrayList<TableVariable> notFreeVariables = copyAllButTableBoxVariable(polytope.getIndices());
		notFreeVariables.add(TABLE_BOX_VARIABLE);
		ArrayList<TableVariable> freeVariables 	  = copyAllButTableBoxVariable(polytope.getFreeVariables());
		
		TableFactor factor = (TableFactor) polytope.getFactor();
		TableFactor newFactorLowHalf  = maxOrMinOut(factor, freeVariables, notFreeVariables,(newValue,oldValue)->(newValue<oldValue));
		TableFactor newFactorHighHalf = maxOrMinOut(factor, freeVariables, notFreeVariables,(newValue,oldValue)->(newValue>oldValue));
		
		TableFactor result = makeABoxFactorHavingTheBoxesExtremes(newFactorLowHalf, newFactorHighHalf);
		return result;
	}

	private static ArrayList<TableVariable> copyAllButTableBoxVariable(Collection<? extends Variable> collection) {
		return filterArrayList(collection,(v)->!v.equals(TABLE_BOX_VARIABLE));
	}
		
	private static <T extends Variable> ArrayList<TableVariable> filterArrayList(Collection<T> collection,
			Predicate<T> filter) {
		ArrayList<TableVariable> result = new ArrayList<>(collection.size());
		
		for(T v : collection) {
			if(filter.apply(v)) {
				result.add((TableVariable) v);
			}
		}
		
		return result;
	}
*/

	private static boolean normalize_ = false;

	public static Box makeTableBox(IntensionalConvexHullOfFactors bound,boolean normalize) {
		if(!normalize)
			return makeTableBox(bound);
		normalize_ = true;
		Box result =  makeTableBox(bound);
		normalize_ = false;
		
		return result;
		
	}
	public static Box makeTableBox(IntensionalConvexHullOfFactors bound) {
		ArrayList<TableVariable> notFreeVariables = new ArrayList<>(bound.getIndices().size());
		for(Variable v : bound.getIndices()) {
			notFreeVariables.add((TableVariable)v);
		}
		
		TableFactor factor = buildBoxFactor((TableFactor) bound.getFactor(), notFreeVariables);
		
		Box result = new Box(TABLE_BOX_VARIABLE, factor);
		return result;
		
	}
	
	public static TableFactor buildBoxFactor(TableFactor factor, ArrayList<TableVariable> notFreeVariables) {
		//notFreeVariables.retainAll(factor.getVariables());//Maybe it is not necessary
		notFreeVariables.remove(TABLE_BOX_VARIABLE);
		if(notFreeVariables.isEmpty()) {
			return factor; 
		}
		TableFactor result = buildBoxFactorIfSetIsNotEmpty(factor,notFreeVariables);
		return result;
	}

	private static TableFactor buildBoxFactorIfSetIsNotEmpty(TableFactor factor, ArrayList<TableVariable> notFreeVariables) {
		LinkedHashSet<TableVariable> setOfFreeVariables = new LinkedHashSet<TableVariable>(factor.getVariables());
		setOfFreeVariables.removeAll(notFreeVariables);
		
		boolean initialFactorIsABox = setOfFreeVariables.contains(TABLE_BOX_VARIABLE);
		if(initialFactorIsABox) {
			setOfFreeVariables.remove(TABLE_BOX_VARIABLE);
		}

		//ArrayList<TableVariable> listOfFreeVariables = Util.arrayList(TABLE_BOX_VARIABLE);
		//listOfFreeVariables.addAll(newFactorSetOfVariables);
		ArrayList<TableVariable> listOfFreeVariables =  new ArrayList<>(setOfFreeVariables);

		TableFactor newFactorLowHalf; 
		TableFactor newFactorHighHalf;
		if(initialFactorIsABox) {
			Pair<TableFactor, TableFactor> factorHalves = divideABoxFactorIntoTwoHalves(factor);
			TableFactor factorLowHalf  = factorHalves.first;
			TableFactor factorHighHalf = factorHalves.second;

			newFactorLowHalf  = maxOrMinOut(factorLowHalf, listOfFreeVariables,notFreeVariables,(newValue,oldValue)->newValue<oldValue);
			newFactorHighHalf = maxOrMinOut(factorHighHalf,listOfFreeVariables,notFreeVariables,(newValue,oldValue)->(newValue>oldValue));
		}
		else {
			newFactorLowHalf  = maxOrMinOut(factor,listOfFreeVariables, notFreeVariables,(newValue,oldValue)->(newValue<oldValue));
			newFactorHighHalf = maxOrMinOut(factor,listOfFreeVariables, notFreeVariables,(newValue,oldValue)->(newValue>oldValue));
		}
		
		TableFactor result = makeABoxFactorHavingTheBoxesExtremes(newFactorLowHalf,newFactorHighHalf);
		
		return result;
	}

	private static Pair<TableFactor, TableFactor> divideABoxFactorIntoTwoHalves(TableFactor factor) {
		ArrayList<TableVariable> varaibles = (ArrayList<TableVariable>)factor.getVariables();
		
		
		TableFactor minValueHalfFactor;
		TableFactor maxValueHalfFactor;
		if(varaibles.get(0).equals(TABLE_BOX_VARIABLE)) {
			println(varaibles.subList(1, varaibles.size()).getClass());
			ArrayList<TableVariable> variablesWithoutBoxVariable = new ArrayList<>(varaibles.subList(1, varaibles.size()));
			ArrayList<Double> entries = factor.getEntries();
			int len = entries.size();
			ArrayList<Double> lowHalfEntries  = new ArrayList<>(entries.subList(0, len/2 ));
			ArrayList<Double> highHalfEntries = new ArrayList<>(entries.subList(len/2, len));
			minValueHalfFactor = new TableFactor(variablesWithoutBoxVariable, lowHalfEntries);
			maxValueHalfFactor = new TableFactor(variablesWithoutBoxVariable, highHalfEntries);	
		}
		else {
			//Util.println(TABLE_BOX_VARIABLE.toString() + "should be the first variable, but wasn't");
			
			LinkedHashMap<TableVariable, Integer> mapOfvaluesPredetermined = new LinkedHashMap<>();
			mapOfvaluesPredetermined.put(TABLE_BOX_VARIABLE, 0);
			minValueHalfFactor = copyToSubTableFactor(factor,mapOfvaluesPredetermined);
			mapOfvaluesPredetermined = new LinkedHashMap<>();
			mapOfvaluesPredetermined.put(TABLE_BOX_VARIABLE, 1);
			maxValueHalfFactor = copyToSubTableFactor(factor,mapOfvaluesPredetermined);
		}

		Pair<TableFactor, TableFactor> result = new Pair<>(minValueHalfFactor,maxValueHalfFactor);
		return result;
	}

	public static void variablesWithoutElement(ArrayList<TableVariable> varaibles) {
		ArrayList<TableVariable> variablesWithoutBoxVariable;
		variablesWithoutBoxVariable = new ArrayList<>(varaibles.size()-1);
		for(TableVariable v : varaibles) {
			if(!v.equals(TABLE_BOX_VARIABLE)) {
				variablesWithoutBoxVariable.add(v);
			}
		}
	}
	
	/*private TableFactor copyToSubTableFactor(TableFactor factor,
			List<TableVariable> predeterminedVariables, List<Integer> predeterminedValues) {
		LinkedHashMap<TableVariable, Integer> mapOfvaluesPredetermined= new LinkedHashMap<>();
		
		addValuesToMapFromVariableToInstantiation(predeterminedVariables, predeterminedValues,mapOfvaluesPredetermined);
		TableFactor result = copyToSubTableFactor(factor, mapOfvaluesPredetermined);
		return result;
	}*/

	public static void addValuesToMapFromVariableToInstantiation(List<TableVariable> predeterminedVariables,
			List<Integer> predeterminedValues, LinkedHashMap<TableVariable, Integer> mapOfvaluesPredetermined) {
		Util.myAssert(predeterminedValues.size()==predeterminedVariables.size(), ()->"Two arrays shoulf have the same size");

		Iterator<TableVariable> variableIterator = predeterminedVariables.iterator();
		Iterator<Integer> valueIterator = predeterminedValues.iterator();
		
		while(variableIterator.hasNext()) {
			mapOfvaluesPredetermined.put(variableIterator.next(), valueIterator.next());
		}
	}

	public static TableFactor maxOrMinOut(TableFactor nonBoxfactor, ArrayList<TableVariable> freeVariables,ArrayList<TableVariable> notFreeVariables, 
			BiFunction<Double,Double,Boolean> comparisson) {
		TableFactor result = new TableFactor(freeVariables,arrayListFilledWith(-1.0, numEntries(freeVariables)));

		for(ArrayList<Integer> notFreeVariablesInstantiation : in(getCartesianProduct(notFreeVariables))) {
			LinkedHashMap<TableVariable, Integer> mapOfInstantiations = new LinkedHashMap<>();
			addValuesToMapFromVariableToInstantiation(notFreeVariables, notFreeVariablesInstantiation, mapOfInstantiations);
			TableFactor thisInstanciation = copyToSubTableFactor(nonBoxfactor, mapOfInstantiations);
			if(normalize_) {
				thisInstanciation = thisInstanciation.normalize();
			}
			for(ArrayList<Integer> freeVariablesInstantiation : in(getCartesianProduct(freeVariables))) {
				addValuesToMapFromVariableToInstantiation(freeVariables, freeVariablesInstantiation, mapOfInstantiations);

				Double currentValue = result.getEntryFor(mapOfInstantiations);
				Double newValue = thisInstanciation.getEntryFor(mapOfInstantiations);
				
				if(currentValue == -1.0 || comparisson.apply(newValue, currentValue) ) {
					result.setEntryFor(mapOfInstantiations, newValue);
				}
			}
		}
		return result;
	}

	private static TableFactor makeABoxFactorHavingTheBoxesExtremes(TableFactor first, TableFactor second) {
		ArrayList<TableVariable> variables = Util.arrayList(TABLE_BOX_VARIABLE);
		variables.addAll(first.getVariables());
		ArrayList<Double> entries = new ArrayList<>(first.getVariables().size()*2);
		entries.addAll(first.getEntries());
		entries.addAll(second.getEntries());
		
		TableFactor result = new TableFactor(variables,entries);
		return result;
	}
	
	public static void main(String[] args) {
		TableVariable A = new TableVariable("A", 2);
		TableVariable B = new TableVariable("B", 2);
		TableVariable C = new TableVariable("C", 2);
		TableVariable D = new TableVariable("D", 2);
		
		TableFactor fABCD = new TableFactor(arrayList(A,B,C,D),
				arrayList(0.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5));
		
		TableFactor fBoxABCD = new TableFactor(arrayList(TABLE_BOX_VARIABLE,A,B,C,D),
				arrayList(0.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,
						10.,10.1,10.2,10.3,10.4,10.5,10.6,10.7,10.8,10.9,11.0,11.1,11.2,11.3,11.4,11.5));
		
		ArrayList<TableVariable> list = arrayList(A,B,C,D,TABLE_BOX_VARIABLE);
		TableFactor fABCDBox = new TableFactor(list);
		
		LinkedHashMap<TableVariable,Integer> map = new LinkedHashMap<>();
		for(ArrayList<Integer> instantiation:in(getCartesianProduct(list))) {
			addValuesToMapFromVariableToInstantiation(list, instantiation, map);
			fABCDBox.setEntryFor(map, fBoxABCD.getEntryFor(map));
		}

		println(fABCD);
		println(fBoxABCD);
		println(fABCDBox);
		
		Pair<TableFactor, TableFactor> pair = divideABoxFactorIntoTwoHalves(fBoxABCD);
		println("Lower  box: " + pair.first);
		println("Higher box: " + pair.second);
		
		pair = divideABoxFactorIntoTwoHalves(fABCDBox);
		println("Lower  box: " + pair.first);
		println("Higher box: " + pair.second);

		println(maxOrMinOut(fABCD, arrayList(A,B), arrayList(C,D), (newValue,oldValue)->(newValue<oldValue)));
		println(maxOrMinOut(fABCD, arrayList(A,B), arrayList(C,D), (newValue,oldValue)->(newValue>oldValue)));
		
		println("BoxFactor"+makeABoxFactorHavingTheBoxesExtremes(fABCD, fABCD));
		
		//-----------------------------------------------
		
		IntensionalConvexHullOfFactors ICHOF = new IntensionalConvexHullOfFactors(Util.list(TABLE_BOX_VARIABLE,A,B), fABCDBox);
		println(ICHOF);
		
		println(makeTableBox(ICHOF));		
		
		TableFactor fCBDA = copyFactorInDifferentOrder(arrayList(D,B,TABLE_BOX_VARIABLE,C,A),fABCDBox);
		ICHOF = new IntensionalConvexHullOfFactors(Util.list(A,TABLE_BOX_VARIABLE,B), fCBDA);
		println(makeTableBox(ICHOF));
	}

	private static TableFactor copyFactorInDifferentOrder(ArrayList<TableVariable> arrayList, TableFactor factor) {
		
		TableFactor result = new TableFactor(arrayList,arrayListFilledWith(-1.0, numEntries(arrayList)));

		ArrayList<TableVariable> var = new ArrayList<>(factor.getVariables());
		for(ArrayList<Integer> notFreeVariablesInstantiation : in(getCartesianProduct(var ))) {
			LinkedHashMap<TableVariable, Integer> mapOfInstantiations = new LinkedHashMap<>();
			addValuesToMapFromVariableToInstantiation(var, notFreeVariablesInstantiation, mapOfInstantiations);
			result.setEntryFor(mapOfInstantiations, factor.getEntryFor(mapOfInstantiations));
		}
		return result;
	}

	
}
