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
package com.sri.ai.praise.core.representation.interfacebased.polytope.core;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.product;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.FunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.util.Timer;
import com.sri.ai.util.Util;

/**
 * An implementation of {@link AbstractFunctionConvexHull} that 
 * simplifies the convex hull based on function f(I, FreeVariables) by the product polytope
 * <code>prod_{F in FreeVariables} Margin_Simplex_( Projection(F) )</code> 
 * where
 * <code>NormalizedProjection_F = normalize_F sum_{FreeVariables \ {F}} ConvexHull ( { f(Indices, FreeVariables) }_Indices ) </code>
 * and
 * <code>Margin_Simplex(ConvexHull ( { f(I, V) }_I ) ) = { Vertex_v : v in Values(V) }</code>
 * and
 * <code>Vertex_v = ( min_I f(I,v) if u = v, max_I f(I,v) otherwise }_{u in Values(V)}</code>
 * <p>
 * Intuitively, we replace the convex hull by a product of the "margin simplexes" on each free variable.
 * A "margin simplex" is a convex hull of the same shape as the variable's polytope
 * (therefore with the same number of vertices, which is to say the variable's number of values).
 * It therefore has one vertex per value.
 * The vertex for a certain value is a distribution in which all other values have the minimum probability the exhibit in the original convex hull,
 * while the special value has the maximum probability (computed as 1 - the sum of the minimal probabilities of the remaining values).
 * 
 * @author braz
 *
 */
final public class MinimumBasedFunctionConvexHull extends AbstractFunctionConvexHull {
	
	public MinimumBasedFunctionConvexHull(Collection<? extends Variable> indices, Factor factor) {
		super(indices, factor);
	}
	
	@Override
	public MinimumBasedFunctionConvexHull newInstance(Collection<? extends Variable> indices, Factor factor) {
		return new MinimumBasedFunctionConvexHull(indices, factor);
	}
	
	@Override
	public Polytope simplify() {
		Collection<? extends Variable> variables = getIndices();
		var summationCostOfIndices = summationCost(variables);
		var summationCostOfFreeVariables = summationCost(getFreeVariables());
		if (summationCostOfFreeVariables >= summationCostOfIndices) {
			return this;
		}
		
		println("\nBefore simplification: " + getFactor().summationCost());
		println("                indices: " + getIndices());
		println("         free variables: " + getFreeVariables());
		
		var result = Timer.getResultAndTime(() -> marginSimplex(normalize(getFreeVariables())));
		
		println(  "After  simplification: " + result.first.getFactor().summationCost());
		println(  "              indices: " + result.first.getIndices());
		println(  "       free variables: " + result.first.getFreeVariables());
		
		return result.first;
//		List<Variable> freeVariablesBuffer = listFrom(getFreeVariables());
//		var marginSimplices = mapIntoList(getFreeVariables(), v -> getMarginSimplex(v, freeVariablesBuffer));
//		return ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(marginSimplices);
	}

	private int summationCost(Collection<? extends Variable> variables) {
		return product(functionIterator(variables, v -> ((TableVariable)v).getCardinality())).intValue();
	}

	FunctionConvexHull getMarginSimplex(Variable v, List<Variable> freeVariablesBuffer) {

		freeVariablesBuffer.remove(v);

		var otherFreeVariables = freeVariablesBuffer;
		var projection = MinimumBasedFunctionConvexHull.this.sumOut(otherFreeVariables); 
		var normalizedProjection = projection.normalize(list(v));
		var result = marginSimplex(normalizedProjection);

		freeVariablesBuffer.add(v);

		return result;
	}

	private FunctionConvexHull marginSimplex(FunctionConvexHull normalizedConvexHull) {
		var minima = normalizedConvexHull.getFactor().min(normalizedConvexHull.getIndices());
		var marginSimplexFactor = marginSimplexFactor(minima);
		int numberOfMarginSimplexIndices = marginSimplexFactor.getVariables().size()/2;
		var marginSimplexIndices = marginSimplexFactor.getVariables().subList(0, numberOfMarginSimplexIndices);
		return MinimumBasedFunctionConvexHull.this.newInstance(marginSimplexIndices, marginSimplexFactor);
	}

	private static int indexIndex = 0;
	
	/**
	 * Takes a factor containing the minimum probabilities for the values of a variable,
	 * and returns a new factor with twice the dimensions as the original one,
	 * with the first half being new index variables, based on the original ones,
	 * such that each assignment to these index variables leaves a matrix on the original
	 * variables with values equal to the minima, with the exception of the
	 * position equal to the assignment to the indices (so it's the diagonal if
	 * we consider indices assignments as the row and original variable assignments as the column)
	 * which is equal to 1 - sum of minima in row, that is the maximum probability for that assignment.
	 * @param minima
	 * @return
	 */
	private Factor marginSimplexFactor(Factor minima) {
		TableFactor tableFactor = (TableFactor) minima;
		int numberOfAssignments = tableFactor.numberOfEntries();
		ArrayList<Double> array = tableFactor.getEntries();
		double sumOfMinima = 0;
		for (int col = 0; col != numberOfAssignments; col++) {
			sumOfMinima += array.get(col);
		}
		double newArray[] = new double[numberOfAssignments * numberOfAssignments];
		for (int row = 0; row != numberOfAssignments; row++) {
			for (int col = 0; col != numberOfAssignments; col++) {
				newArray[row*numberOfAssignments + col] = array.get(col); 
			}
			int offsetForDiagonalColumnInThisRow = row*numberOfAssignments + row;
			newArray[offsetForDiagonalColumnInThisRow] = 1.0 - (sumOfMinima - array.get(row)); 
		}
		var indexVariables = mapIntoList(tableFactor.getVariables(), v -> new TableVariable("I" + indexIndex++, v.getCardinality()));
		var newVariables = Util.makeListWithElementsOfTwoCollections(indexVariables, tableFactor.getVariables());
		return new ArrayTableFactor(newVariables, newArray);
	}
	
	@Override
	public boolean equalsModuloPermutations(Polytope another) {
		throw new Error("mathematicallyEquals not implemented for " + MinimumBasedFunctionConvexHull.class);
	}

}