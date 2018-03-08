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
package com.sri.ai.praise.inference.representation.expression.core;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.library.set.Sets.intensionalMultiSet;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.Collection;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.praise.inference.representation.expression.api.ExpressionFactor;
import com.sri.ai.praise.inference.representation.expression.api.ExpressionModel;

public class DefaultExpressionFactor extends AbstractExpressionNode implements ExpressionFactor {

	private static final long serialVersionUID = 1L;

	public DefaultExpressionFactor(Expression expression, ExpressionModel model) {
		super(expression, model);
	}

	@Override
	public Collection<Variable> getNeighbors() {
		Collection<Variable> result = getModel().getBsOfA(this);
		return result;
	}

	@Override
	public boolean contains(Variable variable) {
		boolean result = Expressions.contains(this, (Expression) variable);
		return result;
	}

	@Override
	public Factor multiply(Factor another) {
		Factor result = evaluate(Times.make(this, (Expression) another));
		return result;
	}

	@Override
	public Factor sumOut(List<Variable> variablesToSumOut) {
		// TODO: quite unfortunate need to cast each variable; find better solution
		List<Expression> variableExpressions = mapIntoList(variablesToSumOut, v -> (Expression) v);
		Expression set = intensionalMultiSet(variableExpressions, this, getModel().getContext());
		Expression sum = apply(SUM, set);
		Factor result = evaluate(sum);
		return result;
	}

	private Factor evaluate(Expression expression) {
		Expression resultFactorExpression = evaluateToExpression(expression);
		Factor result = makeFactor(resultFactorExpression);
		return result;
	}

	private Expression evaluateToExpression(Expression expression) {
		Context context = getModel().getContext();
		Expression result = context.getTheory().evaluate(expression, context);
		return result;
	}

	private DefaultExpressionFactor makeFactor(Expression expression) {
		DefaultExpressionFactor result = new DefaultExpressionFactor(expression, getModel());
		return result;
	}
}