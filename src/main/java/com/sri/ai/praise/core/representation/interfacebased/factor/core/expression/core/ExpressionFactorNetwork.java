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

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.base.IdentityWrapper.identityWrapper;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Tuple;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.representation.api.EditableFactorNetwork;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.representation.core.AbstractEditableFactorNetwrok;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionFactor;


/**
 * A factor network in which factors and indices are represented as {@link Expression}s 
 * encoding potential functions.
 * 
 * @author braz
 *
 */
public class ExpressionFactorNetwork extends AbstractEditableFactorNetwrok {
	
	private Context context;

	public static ExpressionFactorNetwork expressionFactorNetwork(String tupleOfFactorExpressions, Context context) {
		List<Expression> expressions = fromTupleOfExpressionsStringToListOfExpressions(tupleOfFactorExpressions);
		ExpressionFactorNetwork result = expressionFactorNetwork(expressions, context);
		return result;
	}
	
	private static List<Expression> fromTupleOfExpressionsStringToListOfExpressions(String tupleOfFactorExpressions) {
		Expression parsed = parse(tupleOfFactorExpressions);
		myAssert(parsed instanceof Tuple, () -> ExpressionFactorNetwork.class + " constructor taking String should receive string of tuple of factors, but got something that is not a tuple: " + tupleOfFactorExpressions);
		Tuple tuple = (Tuple) parsed;
		List<Expression> result = tuple.getArguments();
		return result;
	}

	public static ExpressionFactorNetwork expressionFactorNetwork(List<? extends Expression> factorExpressions, Context context) {
		List<? extends ExpressionFactor> expressionFactors = fromExpressionsToExpressionFactors(factorExpressions, context);
		ExpressionFactorNetwork result = new ExpressionFactorNetwork(expressionFactors, context);
		return result;
	}
	
	private static List<? extends ExpressionFactor> fromExpressionsToExpressionFactors(List<? extends Expression> expressions, Context context) {
		List<? extends ExpressionFactor> result = mapIntoList(expressions, e -> fromExpressionToExpressionFactor(e, context));
		return result;
	}

	private static ExpressionFactor fromExpressionToExpressionFactor(Expression expression, Context context) {
		ExpressionFactor result;
		if (expression instanceof ExpressionFactor) {
			result = (ExpressionFactor) expression;
		}
		else {
			 result = new DefaultExpressionFactor(expression, context);
		}
		return result;
	}
	
	public ExpressionFactorNetwork(List<? extends ExpressionFactor> factorExpressions, Context context) {
		indexFactorsAndVariables(factorExpressions, context);
		this.context = context;
	}
	
	// TODO: abstract this indexing to parent, since it works for any type of factors and variables
	
	private void indexFactorsAndVariables(List<? extends Factor> factors, Context context) {
		for (Factor factor : factors) {
			indexFactorAndItsVariables(factor);
		}
	}

	private void indexFactorAndItsVariables(Factor factor) {
		for (Variable variable : factor.getVariables()) {
			indexFactorAndVariable(factor, variable);
		}
	}

	private void indexFactorAndVariable(Factor factor, Variable variable) {
		this.add(identityWrapper(factor), variable);
	}

	public Context getContext() {
		return context;
	}

	@Override
	public EditableFactorNetwork makeEmptyNetwork() {
		return new ExpressionFactorNetwork(new ArrayList<>(), context);
	}
}