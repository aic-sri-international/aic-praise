/*
 * Copyright (c) 2014, SRI International
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
 * Neither the name of the aic-expresso nor the names of its
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
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasKind;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.praise.LPIUtil;

/**
 * An atomic rewriter for rewriting expressions of the type:<br>
 * 'if RandomVaribleValue = Formula then Alpha else Beta'<br>
 * to:<br>
 * 'if Formula then if RandomVariableValue then Alpha else Beta else if RandomVariableValue then Beta else Alpha'.
 * 
 * @author ctjoreilly
 *
 */
@Beta
public class EqualityOnRandomVariableAndFormulaInIfThenElseConditionToFormulaOnTop extends AbstractRewriter {
	
	public EqualityOnRandomVariableAndFormulaInIfThenElseConditionToFormulaOnTop() {
		this.setReifiedTests(new HasKind(FunctorConstants.IF_THEN_ELSE));
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = expression; // i.e. no rewrite by default.
		Expression condition;
		if (IfThenElse.isIfThenElse(expression) 
				&& 
			Equality.isEquality(condition = IfThenElse.getCondition(expression))
				&&
			condition.numberOfArguments() == 2) {
			
			Expression randomVariableValue = null;
			Expression formula             = null;
			Expression lOperand            = condition.get(0);
			Expression rOperand            = condition.get(1);
			
			// Note: Check for formula after testing for random variable as a 0-ary 
			// random variables will be interpreted as formulas
			if (LPIUtil.isRandomVariableValueExpression(lOperand, process)) {
				randomVariableValue = lOperand;
			}
			else if (FormulaUtil.isFormula(lOperand, process)) { 
				formula = lOperand;
			}
			
			if (LPIUtil.isRandomVariableValueExpression(rOperand, process)) {
				randomVariableValue = rOperand;
			}
			else if (FormulaUtil.isFormula(rOperand, process)) {
				formula = rOperand;
			}
			
			if (randomVariableValue != null && formula != null) {						
				Expression alpha      = IfThenElse.getThenBranch(expression);
				Expression beta       = IfThenElse.getElseBranch(expression);
				Expression thenBranch = IfThenElse.make(randomVariableValue, alpha, beta);
				Expression elseBranch = IfThenElse.make(randomVariableValue, beta, alpha);
				result = IfThenElse.make(formula, thenBranch, elseBranch);
			}
		}
		return result;
	}
}