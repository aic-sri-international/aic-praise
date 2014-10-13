/*
 * Copyright (c) 2013, SRI International
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
package com.sri.ai.praise.lbp.core;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.BracketedExpression;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.helper.concurrent.RewriteOnBranch;
import com.sri.ai.grinder.library.SemanticSubstitute;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPRewriter;

/**
 * Default implementation of {@link LBPRewriter#R_normalize_random_variable_condition}.
 * 
 * Note: this is a poor man's approach to avoid messages represented as if friends(ann,bob) then if not friends(ann,bob)...
 * Ideally this should be done by the same constraint simplification mechanism, but we don't have that yet.
 * 
 * @author braz
 * 
 */
@Beta
public class NormalizeRandomVariableCondition extends AbstractLBPHierarchicalRewriter implements LBPRewriter {
	
	public NormalizeRandomVariableCondition() {
	}
	
	@Override
	public String getName() {
		return R_normalize_random_variable_condition;
	}
	
	/**
	 * @see LBPRewriter#R_normalize_random_variable_condition
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		
		// Assert input arguments
		// a tuple of the form: ([ v ], E).
		if (!Tuple.isTuple(expression) || Tuple.size(expression) != 2) {
			throw new IllegalArgumentException("Invalid input argument expression:"+expression);
		}
		
		Expression randomVariable = Tuple.get(expression, 0);
		Expression expressionE    = Tuple.get(expression, 1);
		
		Expression result = null;

		if (IfThenElse.isIfThenElse(expressionE) && LPIUtil.isConstraint(IfThenElse.getCondition(expressionE), process)) {
			Trace.log("Externalizing conditional");

			Expression condition  = IfThenElse.getCondition(expressionE);
			Expression thenBranch = IfThenElse.getThenBranch(expressionE);
			Expression elseBranch = IfThenElse.getElseBranch(expressionE);

			result = GrinderUtil.branchAndMergeOnACondition(
					condition,
					newNormalizeRandomVariableCondition(), new Expression[] { randomVariable, thenBranch },
					newNormalizeRandomVariableCondition(), new Expression[] { randomVariable, elseBranch },
					R_check_branch_reachable, 
					R_basic, process);
		}
		else {
			Expression randomVariableValueExpression = ((BracketedExpression) randomVariable).getInnerExpression();
			Trace.log("E ({}) is unconditional basic expression on {}", expressionE, randomVariableValueExpression);

			Expression expressionEForRandomVariableTrue  = SemanticSubstitute.replace(expressionE, randomVariableValueExpression, Expressions.TRUE,  process);
			Expression expressionEForRandomVariableFalse = SemanticSubstitute.replace(expressionE, randomVariableValueExpression, Expressions.FALSE, process);

			expressionEForRandomVariableTrue  = process.rewrite(R_normalize, expressionEForRandomVariableTrue);
			expressionEForRandomVariableFalse = process.rewrite(R_normalize, expressionEForRandomVariableFalse);

			result = IfThenElse.make(randomVariableValueExpression, expressionEForRandomVariableTrue, expressionEForRandomVariableFalse);
		} 

		return result;
	}
	
	//
	// PRIVATE
	//
	private RewriteOnBranch newNormalizeRandomVariableCondition() {
		return new RewriteOnBranch() {
			@Override
			public Expression rewrite(Expression[] expressions, RewritingProcess process) {
				Expression result = process.rewrite(R_normalize_random_variable_condition,
										LPIUtil.argForNormalizeRandomVariableConditionRewriteCall(expressions[0], expressions[1]));
				return result;
			}
		};
	}
}
