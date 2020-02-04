package com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.box;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.ArrayListTableFactor.makeCartesianProductIterator;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.box.TableBoxVariable.TABLE_BOX_VARIABLE;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.ArrayListTableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.convexhull.IntensionalConvexHullOfFactors;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.collect.CartesianProductIterator;

public class Box extends IntensionalConvexHullOfFactors{
	
	public Box(BoxVariable boxVariable,Factor boxUnderTheFormOfAFactor) {
		super(list(boxVariable), boxUnderTheFormOfAFactor);
	}
	
	public Box(ArrayListTableFactor phiMin,ArrayListTableFactor phiMax) {
		super(list(TABLE_BOX_VARIABLE), makeTableFactorBox(phiMin,phiMax));
	}
	
	private static ArrayListTableFactor makeTableFactorBox(ArrayListTableFactor phiMin, ArrayListTableFactor phiMax) {
		ArrayList<TableVariable> variables = Util.arrayList(TABLE_BOX_VARIABLE);
		variables.addAll(phiMax.getVariables());
		ArrayList<Double> entries = new ArrayList<>(phiMin.getEntries());
		entries.addAll(phiMax.getEntries());
		ArrayListTableFactor result = new ArrayListTableFactor(variables,entries);
		return result;
	}

	public static Box boxIt(IntensionalConvexHullOfFactors polytope) {
		if(!(polytope.getFactor() instanceof ArrayListTableFactor)) {
			throw new Error("For now only support convex hulls on Table Factors");
		}
		List<ArrayListTableFactor> normalizedFactors = normalize(polytope);
		ArrayList<TableVariable> freeVariables = mapIntoArrayList(polytope.getFreeVariables(), v->(TableVariable)v);
		
		Box result =  boxNormalizedFactors(normalizedFactors,freeVariables);
		
		return result;
	}

	private static Box boxNormalizedFactors(List<ArrayListTableFactor> normalizedFactors, ArrayList<TableVariable> variables) {
		
		ArrayListTableFactor phiMin= new ArrayListTableFactor(variables);
		ArrayListTableFactor phiMax= new ArrayListTableFactor(variables);
		for(ArrayList<Integer> values : in(makeCartesianProductIterator(variables))){
			Map<TableVariable, Integer> map = Util.mapFromListOfKeysAndListOfValues(variables, values);
			List<Double> phiValues = mapIntoList(normalizedFactors, (f)-> f.getEntryFor(map));

			phiMin.setEntryFor(map, Collections.min(phiValues));
			phiMax.setEntryFor(map, Collections.max(phiValues));
		}			
		return new Box(phiMin,phiMax);
	}

	private static List<ArrayListTableFactor> normalize(IntensionalConvexHullOfFactors polytope) {
		List<ArrayListTableFactor> nonNormalizedFactors = instantiateAllEdgesOfAICHOF(polytope);
		List<ArrayListTableFactor> result = Util.mapIntoList(nonNormalizedFactors, f->f.normalize());
		return result;
	}

	//private static List<TableFactor> listOfFactorsRepresentedByThePolytope(IntensionalConvexHullOfFactors polytope) {
		//List<Box> boxesGeneratedByInstantiatingNonBoxVariablesInTheConvexHull = instantiateAllEdgesOfAICHOF(polytope);
		//List<TableFactor> nonNormalizedFactors = new LinkedList<>();
		//for(Box b : boxesGeneratedByInstantiatingNonBoxVariablesInTheConvexHull) {
			//nonNormalizedFactors.addAll(instantiateAllEdgesOfABox(b));
		//}
		//List<TableFactor> nonNormalizedFactors = instantiateAllEdgesOfAICHOF(polytope);
		//return nonNormalizedFactors;
//	}

	private static List<ArrayListTableFactor> instantiateAllEdgesOfAICHOF(IntensionalConvexHullOfFactors polytope) {
		ArrayListTableFactor factor = (ArrayListTableFactor) polytope.getFactor();
		
		ArrayList<TableVariable> indexes = indexesThatAreNotTableBoxVariable(polytope);
		boolean isABox = indexes.size() < polytope.getIndices().size();
		
		List<ArrayListTableFactor> result = new LinkedList<>();
		for(List<Integer> instantiations : in(ArrayListTableFactor.makeCartesianProductIterator(indexes))) {
			ArrayListTableFactor subFactor = factor.slice(indexes, instantiations);
			if (isABox) {
				List<ArrayListTableFactor> factorsFromBox = instantiateAllEdgesOfABox(new Box(TABLE_BOX_VARIABLE, subFactor));
				result.addAll(factorsFromBox);
			}
			else {
				if(!isNullProbability(subFactor)) {
					result.add(subFactor);
				}
			}
		}
		return result;
	}

	private static boolean isNullProbability(ArrayListTableFactor subFactor) {
		for(Double entry :  subFactor.getEntries()) {
			if(entry!=0.) {
				return false;
			}
		}
		return true;
	}

	private static ArrayList<TableVariable> indexesThatAreNotTableBoxVariable(IntensionalConvexHullOfFactors polytope) {
		ArrayList<TableVariable> result = new ArrayList<>();
		for(Variable v : polytope.getIndices()) {
			if(!(v instanceof BoxVariable)) {
				result.add((TableVariable) v);
			}
		}
		return result;
	}

	private static List<ArrayListTableFactor> instantiateAllEdgesOfABox(Box box) {
		ArrayListTableFactor phiMin = box.getPhiMin();
		ArrayListTableFactor phiMax = box.getPhiMax();
		
		List<ArrayListTableFactor> result = new LinkedList<>();
		
		Iterator<ArrayList<Integer>> cartesianProduct = getCartesianProductWitZerosAndOnes(phiMax.getEntries().size());
		for(ArrayList<Integer> binaryNumber : in(cartesianProduct)) {
			ArrayListTableFactor newFactor = makeFactorWithAPermutationOfTheEntriesOnPhiMaxAndPhiMin(phiMin,phiMax,binaryNumber);
			result.add(newFactor);
		}
		return result;
	}

	private static ArrayListTableFactor makeFactorWithAPermutationOfTheEntriesOnPhiMaxAndPhiMin(ArrayListTableFactor phiMin, ArrayListTableFactor phiMax,
			ArrayList<Integer> binaryNumber) {
		ArrayList<Double> phiMinEntries = phiMin.getEntries();
		ArrayList<Double> phiMaxEntries = phiMax.getEntries();
		
		ArrayList<Double> newEntries = new ArrayList<>(phiMin.getEntries().size()); 
		for (int i = 0; i < binaryNumber.size(); i++) {
			newEntries.add(
					binaryNumber.get(i) == 0?
							phiMinEntries.get(i)
							:
							phiMaxEntries.get(i)
					);
		}
		ArrayList<TableVariable> variables = phiMax.getVariables();
		ArrayListTableFactor result = new ArrayListTableFactor(variables,newEntries);
		return result;
	}

	private static Iterator<ArrayList<Integer>> getCartesianProductWitZerosAndOnes(int n) {
		ArrayList<NullaryFunction<Iterator<? extends Integer>>> l = new ArrayList<>();
		 
		for (int i = 0; i < n; i++) {
			l.add(() -> list(0,1).iterator());
		}
		
		Iterator<ArrayList<Integer>> cartesianProduct = new CartesianProductIterator<Integer>(l);
		return cartesianProduct;
	}
	
	public ArrayListTableFactor getPhiMax() {
		ArrayListTableFactor tableFactor = (ArrayListTableFactor) this.getFactor();
		ArrayListTableFactor result = tableFactor.slice(list(TABLE_BOX_VARIABLE), list(1));
		return result;
	}

	public ArrayListTableFactor getPhiMin() {
		ArrayListTableFactor tableFactor = (ArrayListTableFactor) this.getFactor();
		ArrayListTableFactor result = tableFactor.slice(list(TABLE_BOX_VARIABLE), list(0));
		return result;
	}

	/*public Box(Factor phiMin, Factor phiMax, BoxFactorFactory factory) {
		this(factory.make(phiMin,phiMax));
	}*/
	
	/*public static void main(String[] args) {
		TableVariable A = new TableVariable("A", 2);
		TableVariable B = new TableVariable("B", 2);
		Box b = new Box(new TableFactor(arrayList(A,B) 						 ),
						new TableFactor(arrayList(A,B),arrayList(1.,2.,3.,4.)));
		
		
		
		
		
		println(b);
		for(TableFactor f : instantiateAllEdgesOfABox(b)) {
			println(f);
		}
		for(TableFactor f : instantiateAllEdgesOfAICHOF(b)) {
			println(f);
		}
		
	}*/
	
}
