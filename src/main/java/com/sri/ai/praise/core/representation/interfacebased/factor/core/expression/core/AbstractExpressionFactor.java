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
package com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.library.FunctorConstants.MAX;
import static com.sri.ai.grinder.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.library.set.Sets.intensionalMultiSet;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.collect.PredicateIterator.predicateIterator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ConstantFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.IdentityFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ZeroFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.util.collect.PredicateIterator;
import com.sri.ai.util.explanation.tree.DefaultExplanationTree;
import com.sri.ai.util.explanation.tree.ExplanationTree;

/**
 * A {@link Factor} represented by an {@link Expression} provided by {@link #computeInnerExpression()}.
 * 
 * It also takes a {@link Context} as a construction argument in order to identify types of indices,
 * as well as being able to perform multiplication and summation operations.
 * Because of that, the {@link Context} must have the indices and their types already registered
 * and a theory capable of evaluating products and summations in the chosen language.
 * <p>
 * Note that even though {@link Expression#equals(Object)} considers two different instances representing the same expression equal,
 * here <code>equals</code> is reverted to instance comparison because one may have multiple factors in a factor network with
 * the same potential expression, and they should still be considered distinct.
 * 
 * @author braz
 *
 */
public abstract class AbstractExpressionFactor extends AbstractExpressionWrapper implements ExpressionFactor {

	private static final long serialVersionUID = 1L;

	private Context context;

	public AbstractExpressionFactor(Context context) {
		this.context = context;
	}

	@Override
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
		PredicateIterator<Expression> freeVariablesMinusTypeNames = predicateIterator(freeVariableExpressions, isNotType());
		List<? extends Variable> result = mapIntoList(freeVariablesMinusTypeNames, e -> DefaultExpressionVariable.expressionVariable(e));
		return result;
	}

	private Predicate<Expression> isNotType() {
		return e -> context.getTypeFromTypeExpression(e) == null;
	}

	@Override
	public Factor multiply(Factor another) {
		
		Factor result;
		
		if (another instanceof ConstantFactor) {
			if(another instanceof IdentityFactor) {
				result = this;
			}
			if(another instanceof ZeroFactor) {
				result = another;
			}
			result = ((ConstantFactor) another).multiply(this);
		}
		
		else if (another instanceof AbstractExpressionFactor){
			result = evaluateAsFactor(Times.make(this, (Expression) another));
		}
		
		else {
			throw new Error("Trying to multiply factors that belong to incompatible classes : classes are "
					+ "respectively " + this.getClass() + " and " + another.getClass());
		}
		
		return result;
		
	}

	@Override
	public Factor sumOut(Collection<? extends Variable> variablesToSumOut) {
		Expression sum = makeSum(variablesToSumOut);
		Factor result = evaluateAsFactor(sum);
		return result;
	}

	@Override
	public boolean isIdentity() {
		boolean result = getInnerExpression().equals(ONE);
		return result;
	}

	private Expression makeSum(Collection<? extends Variable> variablesToSumOut) {
		Expression set = makeIntensionalMultiSet(variablesToSumOut);
		Expression sum = apply(SUM, set);
		return sum;
	}

	private Expression makeIntensionalMultiSet(Collection<? extends Variable> variablesToSumOut) {
		Collection<Expression> variableExpressionsToSumOut = mapIntoList(variablesToSumOut, v -> (ExpressionVariable) v);
		// TODO: should have been able to just cast variablesToSumOut to List<ExpressionVariable>, but expresso incorrectly assumes them to be Symbols
		// We should be able to correct that and have expresso accept any expression of syntactic form "Symbol".
		Expression set = intensionalMultiSet(variableExpressionsToSumOut, this, getContext());
		return set;
	}

	public Factor evaluateAsFactor(Expression expression) {
		Expression resultFactorExpression = evaluate(expression);
		Factor result = makeFactor(resultFactorExpression);
		return result;
	}

	private Expression evaluate(Expression expression) {
		Expression result = getContext().evaluate(expression);
		return result;
	}

	private ExpressionFactor makeFactor(Expression expression) {
		ExpressionFactor result = new DefaultExpressionFactor(expression, getContext());
		return result;
	}

	@Override
	public Factor normalize() {
		throw new Error("An Expression factor cannot be normalized.");
	}

	@Override
	public Factor add(Factor another) {
		
		Factor result;
		
		if (another instanceof ConstantFactor) {
			result = ((ConstantFactor) another).add(this);
		}
		
		else if(another.getClass() != this.getClass()) {
			throw new Error("Trying to add different types of factors: this is a " +
							this.getClass() + "and another is a " + another.getClass());
		}
		
		else {
			result = evaluateAsFactor(Plus.make(this, (Expression) another));
		}
		
		return result;
		
	}

	@Override
	public boolean isZero() {
		boolean result = getInnerExpression().equals(ZERO);
		return result;
	}

	@Override
	public Factor invert() {
		Factor result;
		if(isZero()) {
			throw new Error("Division by zero impossible.");
		}
		else {
			result = evaluateAsFactor(Division.make(ONE, this));
		}
		return result;
	}

	@Override
	public Factor max(Collection<? extends Variable> variablesToMaximize) {
		
		Factor result = this;
		
		List<? extends Variable> variablesOfInterest = keepOnlyVariablesOfInterest(variablesToMaximize);
		if(variablesOfInterest.isEmpty()) {
			return result;
		}
		
		Expression set = makeIntensionalMultiSet(variablesOfInterest);
		Expression max = apply(MAX, set);
		result = evaluateAsFactor(max);

		return result;
	}
	
	private List<? extends Variable> keepOnlyVariablesOfInterest(Collection<? extends Variable> variablesToMaximize) {
		List<Variable> result = new ArrayList<Variable>();
		for(Variable variable : variablesToMaximize) {
			if(contains(variable)) {
				result.add(variable);
			}
		}
		return result;
		
	}

	private ExplanationTree explanation = DefaultExplanationTree.PLACEHOLDER;

	@Override
	public ExplanationTree getExplanation() {
		return explanation;
	}
	
	@Override
	public void setExplanation(ExplanationTree explanation) {
		this.explanation = explanation;
	}

	@Override
	public int summationCost() {
		throw new Error("Summation cost not yet implemented for " + getClass() + ".");
	}

	@Override
	public boolean mathematicallyEquals(Factor another) {
		throw new Error("mathematicallyEquals not supported for " + AbstractExpressionFactor.class);
	}
}