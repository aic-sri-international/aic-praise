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
package com.sri.ai.praise.inference.representation.expression;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.library.set.Sets.intensionalMultiSet;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.List;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.WrappedExpression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;

/**
 * A {@link Factor} represented by an {@link Expression}.
 * Note that even though {@link Expression#equals(Object)} considers two different instances representing the same expression equal,
 * here <code>equals</code> is reverted to instance comparison because one may have multiple factors in a factor network with
 * the same potential expression.
 * 
 * @author braz
 *
 */
public class ExpressionFactor extends WrappedExpression implements Factor {

	private static final long serialVersionUID = 1L;

	private Context context;

	public ExpressionFactor(Expression expression, Context context) {
		super(expression);
		this.context = context;
	}

	public Context getContext() {
		return context;
	}

	@Override
	public boolean contains(Variable variable) {
		boolean result = Expressions.contains(this, (Expression) variable);
		return result;
	}

	@Override
	public List<? extends Variable> getVariables() {
		Set<Expression> freeVariableExpressions = Expressions.freeVariables(getInnerExpression(), context);
		List<? extends Variable> result = mapIntoList(freeVariableExpressions, e -> new ExpressionVariable(e));
		return result;
	}

	@Override
	public Factor multiply(Factor another) {
		Factor result = evaluateAsFactor(Times.make(this, (Expression) another));
		return result;
	}

	@Override
	public Factor sumOut(List<? extends Variable> variablesToSumOut) {
		Expression sum = makeSum(variablesToSumOut);
		Factor result = evaluateAsFactor(sum);
		return result;
	}

	private Expression makeSum(List<? extends Variable> variablesToSumOut) {
		Expression set = makeIntensionalMultiSet(variablesToSumOut);
		Expression sum = apply(SUM, set);
		return sum;
	}

	private Expression makeIntensionalMultiSet(List<? extends Variable> variablesToSumOut) {
		List<Expression> variableExpressionsToSumOut = mapIntoList(variablesToSumOut, v -> ((ExpressionVariable)v).getInnerExpression());
		// TODO: should have been able to just cast variablesToSumOut to List<ExpressionVariable>, but expresso incorrectly assumes them to be Symbols
		// We should be able to correct that and have expresso accept any expression of syntactic form "Symbol".
		Expression set = intensionalMultiSet(variableExpressionsToSumOut, this, getContext());
		return set;
	}

	private Factor evaluateAsFactor(Expression expression) {
		Expression resultFactorExpression = evaluate(expression);
		Factor result = makeFactor(resultFactorExpression);
		return result;
	}

	private Expression evaluate(Expression expression) {
		Expression result = getContext().evaluate(expression);
		return result;
	}

	private ExpressionFactor makeFactor(Expression expression) {
		ExpressionFactor result = new ExpressionFactor(expression, getContext());
		return result;
	}
	
	/**
	 * {@link Expression#equals(Object)} is overridden to be instance comparison <code>==</code>
	 * because two different instances of factor with the same expression are to be considered distinct factors.
	 */
	public boolean equals(Object another) {
		boolean result = this == another;
		return result;
	}
	
	/**
	 * Reverting to {@link System#identityHashCode(Object)} to match instance comparison performed by {@link #equals(Object)Object)}.
	 */
	public int hashCode() {
		int result = System.identityHashCode(this);
		return result;
	}
}