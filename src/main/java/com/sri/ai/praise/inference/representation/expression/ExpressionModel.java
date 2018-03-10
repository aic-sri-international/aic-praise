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

import static com.sri.ai.expresso.helper.Expressions.freeVariables;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.myAssert;

import java.util.List;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Tuple;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.inference.representation.api.FactorNode;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.util.collect.DefaultManyToManyRelation;


/**
 * 
 * @author braz
 *
 */
public class ExpressionModel extends DefaultManyToManyRelation<FactorNode, Variable> {
	
	private Context context;

	public ExpressionModel(String tupleOfFactorExpressions, Context context) {
		this(fromTupleOfExpressionsStringToListOfExpressions(tupleOfFactorExpressions), context);
	}

	private static List<Expression> fromTupleOfExpressionsStringToListOfExpressions(String tupleOfFactorExpressions) {
		Expression parsed = parse(tupleOfFactorExpressions);
		myAssert(parsed instanceof Tuple, () -> ExpressionModel.class + " constructor taking String should receive string of tuple of factors, but got something that is not a tuple: " + tupleOfFactorExpressions);
		Tuple tuple = (Tuple) parsed;
		List<Expression> result = tuple.getArguments();
		return result;
	}

	public ExpressionModel(List<Expression> factorExpressions, Context context) {
		indexFactorsAndVariables(factorExpressions, context);
		this.context = context;
	}
	
	private void indexFactorsAndVariables(List<Expression> factorExpressions, Context context) {
		for (Expression factorExpression : factorExpressions) {
			indexFactorsAndVariables(factorExpression, context);
		}
	}

	private void indexFactorsAndVariables(Expression factorExpression, Context context) {
		FactorNode factor = new ExpressionFactor(factorExpression, this);
		indexFactorAndItsVariables(factor, factorExpression, context);
	}

	private void indexFactorAndItsVariables(FactorNode factor, Expression factorExpression, Context context) {
		Set<Expression> freeVariables = freeVariables(factorExpression, context);
		for (Expression variableExpression : freeVariables) {
			indexFactorAndVariable(factor, variableExpression);
		}
	}

	private void indexFactorAndVariable(FactorNode factor, Expression variableExpression) {
		Variable variable = new ExpressionVariable(variableExpression, this);
		this.add(factor, variable);
	}

	public Context getContext() {
		return context;
	}
}