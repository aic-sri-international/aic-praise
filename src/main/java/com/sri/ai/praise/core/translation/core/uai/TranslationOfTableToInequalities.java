/*
 * Copyright (c) 2015, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-praise nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.praise.core.translation.core.uai;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.util.Util.arrayListFilledWith;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.getFirstOrNull;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.putInListValue;
import static java.math.BigInteger.ONE;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.MultiQuantifierEliminator;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.core.representation.classbased.table.core.data.FunctionTable;
import com.sri.ai.praise.core.representation.classbased.table.core.uai.UAIUtil;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.CartesianProductEnumeration;
import com.sri.ai.util.collect.IntegerIterator;
import com.sri.ai.util.math.MixedRadixNumber;

/**
 * Translation of a function table to a HOGM expression using inequalities.
 * 
 * @author oreilly
 */
@Beta
public class TranslationOfTableToInequalities {

	/**
	 * A class representing a <i>contiguous</i> sub-set of indices of a function table.
	 * It keeps a variable's index (the <i>current variable</i>)
	 * which restricts the sub-set to that variable and the subsequent ones only.
	 * It keeps initial and final indices for the sub-set.
	 * The variables ahead of the current variables are called <i>remaining variables</i>.
	 * @author braz
	 *
	 */
	private static class FunctionTableIndicesSubSet {
		private FunctionTable functionTable;
		private int currentVariableIndex;
		
		// Indices vectors denoting the start and end of the sub-set.
		// IMPORTANT: these are always on *all* variables of the function table,
		// even the ones coming *before* the current variable, which are irrelevant
		// from this object's point of view.
		// However, keeping the entire vector is more efficient as vectors can often be re-used
		// among subsets on different sets of variables.
		private List<Integer> indicesStart; // if null, it means all indices equal to 0
		private List<Integer> indicesEnd; // if null, it means all indices equal to last value's index
		private Double value;
		private BigInteger size;
		
		/**
		 * Constructs a {@link FunctionTableIndicesSubSet} from <code>currentVariableIndex</code> on,
		 * from <code>indicesStart</code> up to <code>indicesEnd</code>.
		 * @param functionTable
		 * @param currentVariableIndex
		 * @param indicesStart
		 * @param indicesEnd
		 * @param value
		 */
		public FunctionTableIndicesSubSet(
				FunctionTable functionTable,
				int currentVariableIndex,
				List<Integer> indicesStart,
				List<Integer> indicesEnd,
				Double value) {
			
			super();
			this.functionTable = functionTable;
			this.currentVariableIndex = currentVariableIndex;
			this.indicesStart = indicesStart;
			this.indicesEnd = indicesEnd;
			this.value = value;
		}
		
		public FunctionTableIndicesSubSet onRemainingVariables() {
			FunctionTableIndicesSubSet result
			= new FunctionTableIndicesSubSet(functionTable, currentVariableIndex + 1, indicesStart, indicesEnd, value);
			return result;
		}
	
		/**
		 * Creates a copy of this indices sub-set, but set for the remaining variables only,
		 * and only for indices representing the assignment to the <i>first</i> value of the current variable.
		 * @return
		 */
		public FunctionTableIndicesSubSet onRemainingVariablesUnderFirstValueOfCurrentVariableOnly() {
			List<Integer> newIndicesEnd;
			if (subSetIncludesASingleValueForCurrentVariable()) {
				// if single value, last indices on remaining variables remain the same
				newIndicesEnd = indicesEnd;
			}
			else {
				// if multiple values for current variable, first value of current variable hits last indices of remaining variables
				newIndicesEnd = null; 
			}
			FunctionTableIndicesSubSet result
			= new FunctionTableIndicesSubSet(functionTable, currentVariableIndex + 1, indicesStart, newIndicesEnd, value);
			return result;
		}
	
		/**
		 * The flip side of {@link #onRemainingVariablesUnderFirstValueOfCurrentVariableOnly()}.
		 * Creates subset of this sub-set, set for the remaining variables only, and 
		 * only for indices representing the first assignment to the <i>last</i> value of the current variable.
		 * @return
		 */
		public FunctionTableIndicesSubSet onRemainingVariablesUnderLastValueOfCurrentVariableOnly() {
			List<Integer> newIndicesStart;
			if (subSetIncludesASingleValueForCurrentVariable()) {
				// if single value, initial indices on remaining variables remain the same
				newIndicesStart = indicesStart;
			}
			else {
				// if multiple values for current variable, last value for current variable hits first indices of remaining variables
				newIndicesStart = null; 
			}
			FunctionTableIndicesSubSet result
			= new FunctionTableIndicesSubSet(functionTable, currentVariableIndex + 1, newIndicesStart, indicesEnd, value);
			return result;
		}
	
		private boolean subSetIncludesASingleValueForCurrentVariable() {
			boolean result =
					getIndexOfFirstValueOf(currentVariableIndex) == getIndexOfLastValueOf(currentVariableIndex);
			return result;
		}
	
		public FunctionTable getFunctionTable() {
			return functionTable;
		}
		
		public int getCurrentVariableIndex() {
			return currentVariableIndex;
		}
		
		public int getIndexOfFirstValueOf(int variableIndex) {
			int result;
			if (indicesStart == null) {
				result = 0;
			}
			else {
				result = indicesStart.get(variableIndex);
			}
			return result;
		}
		
		public int getIndexOfLastValueOf(int variableIndex) {
			int result;
			if (indicesEnd == null) {
				result = functionTable.cardinality(variableIndex) - 1;
			}
			else {
				result = indicesEnd.get(variableIndex);
			}
			return result;
		}
		
		/**
		 * Let v1 be the first value of the current variable occurring in this sub-set;
		 * this method indicates whether the sub-set's first indices are also the first indices of v1
		 * in the general table.
		 * This is equivalent to the sub-set's first indices being 0 for the remaining variables
		 * (or the first indices of the sub-set being represented by null, which stands for all-zeros).
		 * 
		 * @return
		 */
		public boolean subSetFirstIndicesAreAlsoFirstIndicesOfCurrentVariableFirstValueInTheSubSet() {
			boolean result =
					indicesStart == null ||
					forAll(
							new IntegerIterator(currentVariableIndex + 1, functionTable.numberVariables()),
							i -> indicesStart.get(i) == 0);
			return result;
		}
		
		/**
		 * Let vn be the last value of the current variable occurring in this sub-set;
		 * this method indicates whether the sub-set's last indices are also the last indices of vn
		 * in the general table.
		 * This is equivalent to the sub-set's last indices being the index of the last value of each remaining variable
		 * (or the last indices of the portion being represented by null, which stands for all-last-values).
		 * 
		 * @return
		 */
		public boolean subSetLastIndicesAreAlsoLastIndicesOfCurrentVariableLastValueInTheSubSet() {
			boolean result =
					indicesEnd == null ||
					forAll(
							new IntegerIterator(currentVariableIndex + 1, functionTable.numberVariables()),
							i -> indicesEnd.get(i) == functionTable.cardinality(i) - 1);
			return result;
		}
		
		public Double getFunctionValue() {
			return value;
		}
		
		/**
		 * Returns the number of indices in this sub-set.
		 * @return
		 */
		public BigInteger size() {
			if (size == null) {
				List<Integer> relevantIndicesStart = getRelevantIndicesStart();
				List<Integer> relevantIndicesEnd   = getRelevantIndicesEnd();
				List<Integer> relevantCardinalities = 
						functionTable.getVariableCardinalities().subList(currentVariableIndex, functionTable.numberVariables());
				
				MixedRadixNumber start = new MixedRadixNumber(relevantIndicesStart, relevantCardinalities);
				MixedRadixNumber end   = new MixedRadixNumber(relevantIndicesEnd,   relevantCardinalities);
				
				size = end.getValue().subtract(start.getValue()).add(ONE);
			}
			return size;
		}

		private List<Integer> getRelevantIndicesStart() {
			List<Integer> result;
			if (indicesStart == null) {
				result = arrayListFilledWith(0, functionTable.numberVariables() - currentVariableIndex);
			}
			else {
				result = indicesStart.subList(currentVariableIndex, functionTable.numberVariables());
			}
			return result;
		}

		private List<Integer> getRelevantIndicesEnd() {
			List<Integer> result;
			if (indicesEnd == null) {
				int size = functionTable.numberVariables() - currentVariableIndex;
				result = new ArrayList<>(size);
				for (int i = 0; i != size; i++) {
					result.add(functionTable.cardinality(currentVariableIndex + i) - 1);
				}
			}
			else {
				result = indicesEnd.subList(currentVariableIndex, functionTable.numberVariables());
			}
			return result;
		}

		@Override
		public String toString() {
			return "Subset from " + getRelevantIndicesStart() + " to " + getRelevantIndicesEnd() + " with value " + getFunctionValue();
		}
	}

	/**
	 * Returns an {@link Expression} equivalent to a given {@link FunctionTable} but in the form of a decision tree
	 * (so hopefully more compact) using inequalities.
	 * @param functionTable
	 * @param solverListener if not null, invoked on solver used for compilation, before and after compilation is performed; returned solver from "before" invocation is used (it may be the same one used as argument, of course).
	 * @return
	 */
	public static Expression constructGenericTableExpressionUsingInequalities(FunctionTable functionTable) {
		return constructGenericTableExpressionUsingInequalities(functionTable, null);
	}

	/**
	 * Returns an {@link Expression} equivalent to a given {@link FunctionTable} but in the form of a decision tree
	 * (so hopefully more compact) using inequalities.
	 * @param functionTable
	 * @param solverListener if not null, invoked on solver used for compilation,
	 * before and after compilation is performed; returned solver from "before" invocation is used (it may be the same one used as argument, of course).
	 * @return
	 */
	public static Expression constructGenericTableExpressionUsingInequalities(
			FunctionTable functionTable, Function<MultiQuantifierEliminator, MultiQuantifierEliminator> solverListener) {
	
		// the strategy in this method is the following:
		// we collect all the contiguous indices sub-sets of the function table sharing their function value.
		// They are kept in a map from each value to a list of indices sub-sets with that value.
		//
		// Then, we sort these groups of indices sub-sets by the sum of their sizes (number of entries), from smallest to largest.
		// This will help us later to create an expression that tests for the largest groups first.
		//
		// Finally, we create an if-then-else expression, starting from the leaf (least common value).
		// For each group of indices sub-sets with the same value, we obtain an inequalities expression describing
		// the conditions for a variable assignment to be in that indices sub-set of the function table.
		// Each portion generates a conjunction, and the group of portions generates a disjunction.
		//
		// The resulting if-then-else expression is linearly organized (only else clauses have nested if-then-else expressions).
		// A more balanced (and thus efficient) representation is obtained by compiling it using SGDPLL(T).
		
		Map<Double, List<FunctionTableIndicesSubSet>> functionValuesAndCorrespondingIndicesSubSet = map();
		
		Double currentSubSetFunctionValueIfAny = null;
		List<Integer> firstIndicesOfCurrentSubSetIfAny = null;
		List<Integer> previousIndices = null;
		List<Integer> indices = null;
		
		CartesianProductEnumeration<Integer> cartesianProduct = new CartesianProductEnumeration<>(UAIUtil.cardinalityValues(functionTable));
		while (cartesianProduct.hasMoreElements()) {
			previousIndices = indices;
			indices = new ArrayList<>(cartesianProduct.nextElement());
			
			Double functionValue = Math.round(functionTable.entryFor(indices)*100)/100.0;
			boolean hitNewFunctionValue = currentSubSetFunctionValueIfAny == null || ! functionValue.equals(currentSubSetFunctionValueIfAny);
			if (hitNewFunctionValue) {
				storeIndicesSubSetOnAllVariables(functionTable, firstIndicesOfCurrentSubSetIfAny, previousIndices, currentSubSetFunctionValueIfAny, functionValuesAndCorrespondingIndicesSubSet);
				// get information for next indices sub-set
				currentSubSetFunctionValueIfAny = functionValue;
				firstIndicesOfCurrentSubSetIfAny = indices;
			}
		}
		previousIndices = indices;
		storeIndicesSubSetOnAllVariables(functionTable, firstIndicesOfCurrentSubSetIfAny, previousIndices, currentSubSetFunctionValueIfAny, functionValuesAndCorrespondingIndicesSubSet);
		
		// we sort (by using TreeMap) lists of indices sub-set with the same function value from those with smaller to greater sizes,
		// and form the final expression backwards, thus prioritizing larger sub-sets
		// whose conditions will be more often satisfied and leading to greater simplifications during inference.
	
		List<Pair<BigInteger, List<FunctionTableIndicesSubSet>>>
		listOfPairsOfSizeAndListsOfIndicesSubSetsWithSameFunctionValue = 
		new ArrayList<>(functionValuesAndCorrespondingIndicesSubSet.size());
		
		for (
				Map.Entry<Double, List<FunctionTableIndicesSubSet>> functionValueAndIndicesSubSet
				: functionValuesAndCorrespondingIndicesSubSet.entrySet()) {
			
			List<FunctionTableIndicesSubSet> indicesSubSetsWithSameFunctionValue = functionValueAndIndicesSubSet.getValue();
			BigInteger sumOfSizes = BigInteger.ZERO;
			for (FunctionTableIndicesSubSet indicesSubSet : indicesSubSetsWithSameFunctionValue) {
				sumOfSizes = sumOfSizes.add(indicesSubSet.size());
			}
			listOfPairsOfSizeAndListsOfIndicesSubSetsWithSameFunctionValue.add(Pair.make(sumOfSizes, indicesSubSetsWithSameFunctionValue));
		}
		Collections.sort(listOfPairsOfSizeAndListsOfIndicesSubSetsWithSameFunctionValue, (Comparator<? super Pair<BigInteger, List<FunctionTableIndicesSubSet>>>) (p1,p2) -> p1.first.compareTo(p2.first));

		List<List<FunctionTableIndicesSubSet>> listsOfIndicesSubSetsWithSameFunctionValue
		= mapIntoList(listOfPairsOfSizeAndListsOfIndicesSubSetsWithSameFunctionValue, p -> p.second);
		
		Iterator<List<FunctionTableIndicesSubSet>> listsOfIndicesSubSetsWithSameFunctionValueIterator =
				listsOfIndicesSubSetsWithSameFunctionValue.iterator();
		
		List<FunctionTableIndicesSubSet> firstListOfIndicesSubSets = 
				listsOfIndicesSubSetsWithSameFunctionValueIterator.next();
		Double valueOfFirstListOfIndicesSubSets = getFirstOrNull(firstListOfIndicesSubSets).getFunctionValue();
		
		Expression currentExpression = makeSymbol(valueOfFirstListOfIndicesSubSets);
		while (listsOfIndicesSubSetsWithSameFunctionValueIterator.hasNext()) {
			List<FunctionTableIndicesSubSet> indicesSubSetsWithSameFunctionValue = 
					listsOfIndicesSubSetsWithSameFunctionValueIterator.next();
			Expression functionValueOfIndicesSubSetsWithSameFunctionValue = 
					makeSymbol(getFirstOrNull(indicesSubSetsWithSameFunctionValue).getFunctionValue());
			Expression conditionForThisFunctionValue =
					Or.make(
							mapIntoList(
									indicesSubSetsWithSameFunctionValue,
									TranslationOfTableToInequalities::getInequalitiesExpressionForFunctionTableIndicesSubSet));
			currentExpression = 
					IfThenElse.make(
							conditionForThisFunctionValue,
							functionValueOfIndicesSubSetsWithSameFunctionValue,
							currentExpression);
		}
		
		return currentExpression;
	}

	private static void storeIndicesSubSetOnAllVariables(FunctionTable functionTable, List<Integer> firstIndicesIfAny, List<Integer> lastIndices, Double currentPortionValueIfAny, Map<Double, List<FunctionTableIndicesSubSet>> valuesAndCorrespondingPortions) {
		if (currentPortionValueIfAny != null) {
			FunctionTableIndicesSubSet portion = 
					new FunctionTableIndicesSubSet(
							functionTable,
							0,
							firstIndicesIfAny,
							lastIndices,
							currentPortionValueIfAny);
			putInListValue(valuesAndCorrespondingPortions, currentPortionValueIfAny, portion);
		}
	}

	/**
	 * Returns an expression using inequalities to represent the same function as given portion.
	 * @return
	 */
	private static Expression getInequalitiesExpressionForFunctionTableIndicesSubSet(FunctionTableIndicesSubSet indicesSubSet) {
		Expression result;
	
		int variableIndex = indicesSubSet.getCurrentVariableIndex();
		Expression variable = makeSymbol(UAIUtil.genericVariableName(variableIndex));
		int numberOfVariables = indicesSubSet.getFunctionTable().numberVariables();
		
		int firstValueIndex = indicesSubSet.getIndexOfFirstValueOf(variableIndex);
		Expression variableIsGreaterThanOrEqualToFirstValue = greaterThanOrEqualTo(variable, firstValueIndex);
	
		int lastValueIndex = indicesSubSet.getIndexOfLastValueOf(variableIndex);
		Expression variableIsLessThanOrEqualToLastValue = lessThanOrEqualTo(variable, variableIndex, lastValueIndex, indicesSubSet.getFunctionTable());
		
		if (indicesSubSet.getCurrentVariableIndex() == numberOfVariables - 1) {
			if (firstValueIndex != lastValueIndex) {
				result = And.make(variableIsGreaterThanOrEqualToFirstValue, variableIsLessThanOrEqualToLastValue);
			}
			else {
				result = equalTo(variable, variableIndex, firstValueIndex, indicesSubSet.getFunctionTable());
			}
		}
		else {
			Expression variableIsEqualToValue = equalTo(variable, variableIndex, firstValueIndex, indicesSubSet.getFunctionTable());
	
			boolean indicesStartIsAlsoStartOfFirstVariableFirstValue
			= indicesSubSet.subSetFirstIndicesAreAlsoFirstIndicesOfCurrentVariableFirstValueInTheSubSet();
			
			boolean indicesEndIsAlsoEndOfFirstVariableLastValue
			= indicesSubSet.subSetLastIndicesAreAlsoLastIndicesOfCurrentVariableLastValueInTheSubSet();
	
			if (firstValueIndex == lastValueIndex) { // variable has a single value in this portion
				
				boolean allIndicesWithVariableEqualToThisValueAreInIndicesSubSet =
						indicesStartIsAlsoStartOfFirstVariableFirstValue && indicesEndIsAlsoEndOfFirstVariableLastValue;
				
				if (allIndicesWithVariableEqualToThisValueAreInIndicesSubSet) {
					result = variableIsEqualToValue;
				}
				else {
					// not all, only some, indices with variable equal to this value are in the indices sub-set
					// so we need to use the condition on the remaining variables
					Expression conditionOnRemainingVariables =
							getInequalitiesExpressionForFunctionTableIndicesSubSet(
									indicesSubSet.onRemainingVariables());
					result = And.make(variableIsEqualToValue, conditionOnRemainingVariables);
				}
			}
			else { // first and last value are different
				
				// this means the first value may or may not be completely in the indices sub-set,
				// the last value may or may not be completely in the portion,
				// and all intermediary values are completely in the portion.
				
				Expression conditionForFirstValue;
				int firstValueIndexOfIntermediaryRegion;
				if (indicesStartIsAlsoStartOfFirstVariableFirstValue) {
					conditionForFirstValue = FALSE; // will be taken care of in the intermediary region
					firstValueIndexOfIntermediaryRegion = firstValueIndex;
				}
				else {
					// first value requires a specific condition
					FunctionTableIndicesSubSet onRemainingVariables = 
							indicesSubSet.onRemainingVariablesUnderFirstValueOfCurrentVariableOnly();
					Expression conditionOnRemainingVariablesForFirstValue =
							getInequalitiesExpressionForFunctionTableIndicesSubSet(onRemainingVariables);
					conditionForFirstValue =
							And.make(
									equalTo(variable, variableIndex, firstValueIndex, indicesSubSet.getFunctionTable()), 
									conditionOnRemainingVariablesForFirstValue); 
					firstValueIndexOfIntermediaryRegion = firstValueIndex + 1;
				}
	
				
				Expression conditionForLastValue;
				int lastValueIndexOfIntermediaryRegion;
				if (indicesEndIsAlsoEndOfFirstVariableLastValue) {
					conditionForLastValue = FALSE; // will be taken care of in the intermediary region
					lastValueIndexOfIntermediaryRegion = lastValueIndex;
				}
				else {
					// last value requires a specific condition
					FunctionTableIndicesSubSet onRemainingVariables =
							indicesSubSet.onRemainingVariablesUnderLastValueOfCurrentVariableOnly();
					Expression conditionOnRemainingVariablesForLastValue =
							getInequalitiesExpressionForFunctionTableIndicesSubSet(onRemainingVariables);
					conditionForLastValue =
							And.make(
									equalTo(variable, variableIndex, lastValueIndex, indicesSubSet.getFunctionTable()),
									conditionOnRemainingVariablesForLastValue); 
					lastValueIndexOfIntermediaryRegion = lastValueIndex - 1;
				}
				
				Expression conditionOnIntermediaryRegion;
				int sizeOfIntermediaryRegion = lastValueIndexOfIntermediaryRegion - firstValueIndexOfIntermediaryRegion + 1;
				if (sizeOfIntermediaryRegion == 0) {
					conditionOnIntermediaryRegion = FALSE;
				}
				else if (sizeOfIntermediaryRegion == 1) {
					conditionOnIntermediaryRegion = equalTo(variable, variableIndex, firstValueIndexOfIntermediaryRegion, indicesSubSet.getFunctionTable());
				}
				else {
					conditionOnIntermediaryRegion = 
							And.make(
									greaterThanOrEqualTo(variable, firstValueIndexOfIntermediaryRegion),
									lessThanOrEqualTo(variable, variableIndex, lastValueIndexOfIntermediaryRegion, indicesSubSet.getFunctionTable()));
				}
				
				result = Or.make(conditionForFirstValue, conditionOnIntermediaryRegion, conditionForLastValue);
			}
		}
		return result;
	}

	public static Expression greaterThanOrEqualTo(Expression variable, int value) {
		Expression result = value == 0? TRUE : apply(GREATER_THAN_OR_EQUAL_TO, variable, value);
		return result;
	}

	public static Expression lessThanOrEqualTo(Expression variable, int variableIndex, int value, FunctionTable functionTable) {
		Expression result = value == functionTable.cardinality(variableIndex) - 1? TRUE : apply(LESS_THAN_OR_EQUAL_TO, variable, value);
		return result;
	}

	public static Expression equalTo(Expression variable, int variableIndex, int value, FunctionTable functionTable) {
		Expression result = functionTable.cardinality(variableIndex) == 1 && value == 0? TRUE : apply(EQUALITY, variable, value);
		return result;
	}
}
