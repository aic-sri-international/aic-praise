package com.sri.ai.praise.core.representation.interfacebased.polytope.core.box;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor.makeCartesianProductIterator;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.core.box.TableBoxVariable.TABLE_BOX_VARIABLE;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.FunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.AbstractFunctionConvexHull;
import com.sri.ai.util.Enclosing;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.collect.CartesianProductIterator;

public class Box extends AbstractFunctionConvexHull {
	
	public Box(BoxVariable boxVariable,Factor boxUnderTheFormOfAFactor) {
		super(list(boxVariable), boxUnderTheFormOfAFactor);
	}
	
	public Box(ArrayTableFactor phiMin,ArrayTableFactor phiMax) {
		super(list(TABLE_BOX_VARIABLE), makeTableFactorBox(phiMin,phiMax));
	}
	
	private static ArrayTableFactor makeTableFactorBox(ArrayTableFactor phiMin, ArrayTableFactor phiMax) {
		ArrayList<TableVariable> variables = Util.arrayList(TABLE_BOX_VARIABLE);
		variables.addAll(phiMax.getVariables());
		ArrayList<Double> entries = new ArrayList<>(phiMin.getEntries());
		entries.addAll(phiMax.getEntries());
		ArrayTableFactor result = new ArrayTableFactor(variables,entries);
		return result;
	}

	public static Box boxIt(FunctionConvexHull polytope) {
		if(!(polytope.getFactor() instanceof ArrayTableFactor)) {
			throw new Error("For now only support convex hulls on Table Factors");
		}
		List<ArrayTableFactor> normalizedFactors = normalize(polytope);
		ArrayList<TableVariable> freeVariables = mapIntoArrayList(polytope.getFreeVariables(), v->(TableVariable)v);
		
		Box result =  boxNormalizedFactors(normalizedFactors,freeVariables);
		
		return result;
	}

	private static Box boxNormalizedFactors(List<ArrayTableFactor> normalizedFactors, ArrayList<TableVariable> variables) {
		
		ArrayTableFactor phiMin= new ArrayTableFactor(variables, -1.0);
		ArrayTableFactor phiMax= new ArrayTableFactor(variables, -1.0);
		for(ArrayList<Integer> values : in(makeCartesianProductIterator(variables))){
			Map<TableVariable, Integer> map = Util.mapFromListOfKeysAndListOfValues(variables, values);
			List<Double> phiValues = mapIntoList(normalizedFactors, (f)-> f.getEntryFor(map));

			phiMin.setEntryFor(map, Collections.min(phiValues));
			phiMax.setEntryFor(map, Collections.max(phiValues));
		}			
		return new Box(phiMin,phiMax);
	}

	private static List<ArrayTableFactor> normalize(FunctionConvexHull polytope) {
		List<ArrayTableFactor> nonNormalizedFactors = instantiateAllEdgesOfAICHOF(polytope);
		List<ArrayTableFactor> result = Util.mapIntoList(nonNormalizedFactors, f->f.normalize());
		return result;
	}

	private static List<ArrayTableFactor> instantiateAllEdgesOfAICHOF(FunctionConvexHull polytope) {
		ArrayTableFactor factor = (ArrayTableFactor) polytope.getFactor();
		
		ArrayList<TableVariable> indexes = indexesThatAreNotTableBoxVariable(polytope);
		boolean isABox = indexes.size() < polytope.getIndices().size();
		
		List<ArrayTableFactor> result = new LinkedList<>();
		for(List<Integer> instantiations : in(ArrayTableFactor.makeCartesianProductIterator(indexes))) {
			ArrayTableFactor subFactor = factor.slice(indexes, instantiations);
			if (isABox) {
				List<ArrayTableFactor> factorsFromBox = instantiateAllEdgesOfABox(new Box(TABLE_BOX_VARIABLE, subFactor));
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

	private static boolean isNullProbability(ArrayTableFactor subFactor) {
		for(Double entry :  subFactor.getEntries()) {
			if(entry!=0.) {
				return false;
			}
		}
		return true;
	}

	private static ArrayList<TableVariable> indexesThatAreNotTableBoxVariable(FunctionConvexHull polytope) {
		ArrayList<TableVariable> result = new ArrayList<>();
		for(Variable v : polytope.getIndices()) {
			if(!(v instanceof BoxVariable)) {
				result.add((TableVariable) v);
			}
		}
		return result;
	}

	private static List<ArrayTableFactor> instantiateAllEdgesOfABox(Box box) {
		ArrayTableFactor phiMin = box.getPhiMin();
		ArrayTableFactor phiMax = box.getPhiMax();
		
		List<ArrayTableFactor> result = new LinkedList<>();
		
		Iterator<ArrayList<Integer>> cartesianProduct = getCartesianProductWitZerosAndOnes(phiMax.getEntries().size());
		for(ArrayList<Integer> binaryNumber : in(cartesianProduct)) {
			ArrayTableFactor newFactor = makeFactorWithAPermutationOfTheEntriesOnPhiMaxAndPhiMin(phiMin,phiMax,binaryNumber);
			result.add(newFactor);
		}
		return result;
	}

	private static ArrayTableFactor makeFactorWithAPermutationOfTheEntriesOnPhiMaxAndPhiMin(ArrayTableFactor phiMin, ArrayTableFactor phiMax,
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
		ArrayList<? extends TableVariable> variables = phiMax.getVariables();
		ArrayTableFactor result = new ArrayTableFactor(variables,newEntries);
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
	
	public ArrayTableFactor getPhiMax() {
		ArrayTableFactor tableFactor = (ArrayTableFactor) this.getFactor();
		ArrayTableFactor result = tableFactor.slice(list(TABLE_BOX_VARIABLE), list(1));
		return result;
	}

	public ArrayTableFactor getPhiMin() {
		ArrayTableFactor tableFactor = (ArrayTableFactor) this.getFactor();
		ArrayTableFactor result = tableFactor.slice(list(TABLE_BOX_VARIABLE), list(0));
		return result;
	}

	@Override
	public Box newInstance(Collection<? extends Variable> indices, Factor factor) {
		throw new Error("newInstance not implemented for " + Box.class);
	}

	@Override
	public FunctionConvexHull addIndices(Collection<? extends Variable> newIndices) {
		throw new Error("addIndices not implemented for " + Box.class);
	}

	@Override
	public boolean equalsModuloPermutations(Object another) {
		throw new Error((new Enclosing(){}).methodName() + " not implemented for " + getClass());
	}

	@Override
	public PolytopesEqualityCheck checkEquality(Polytope another) {
		throw new Error((new Enclosing(){}).methodName() + " not implemented for " + getClass());
	}

	@Override
	public Factor probabilityRange() {
		throw new Error((new Enclosing(){}).methodName() + " not implemented for " + getClass());
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
	
	@Override
	public int memory() {
		throw new Error((new Enclosing()).methodName() + " not implemented for " + getClass());
	}
}
