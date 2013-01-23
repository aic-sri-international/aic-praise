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
package com.sri.ai.praise.result;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultCompoundSyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.LPIUtil;

/**
 * The purpose of this class is to contain the methods necessary for converting from fully specified
 * output formats to move human readable ones.
 * 
 * if sick(bob) then 0.3 else 0.7  -> sick(bob) 0.3 
 *
 * @author braz
 */
@Beta
public class ResultConverter {

	public static Expression potentialExpressionToRule(Expression input, RewritingProcess process) {
		
		boolean isIfThenElse = IfThenElse.isIfThenElse(input);
		
		//we can only really simplify if then else expressions
		if (isIfThenElse) {
			Expression condition = IfThenElse.getCondition(input);
			boolean isConstraint = LPIUtil.isConstraint(condition, process);
			if (isConstraint) {
				Expression translationOfE1 = potentialExpressionToRule(input.get(1), process);
				Expression translationOfE2 = potentialExpressionToRule(input.get(2), process);
				
				//if both clauses are true, result is true
				if (translationOfE1.equals(Expressions.TRUE) && translationOfE2.equals(Expressions.TRUE)) {
					return Expressions.TRUE;
				} 
				//if the then clause is true, return the else clause
				else if (translationOfE1.equals(Expressions.TRUE)) {
					return new DefaultCompoundSyntaxTree("conditional rule",
							Not.make(condition),
							translationOfE2);
				} 
				//if the else clause is true, return the if clause
				else if (translationOfE2.equals(Expressions.TRUE)) {
					return new DefaultCompoundSyntaxTree("conditional rule",
							condition,
							translationOfE1);
				}
				//if neither is true, then return the simplified form
				else {
					return new DefaultCompoundSyntaxTree("conditional rule", 
							condition, 
							translationOfE1, 
							translationOfE2);
				}
			}
			else {
				//assume that the 'condition' is a random variable value
				return Expressions.apply("atomic rule", condition, input.get(1));
			}
		}
		
		//the statement must have a constant potential, so it adds nothing
		//of value.  We simply return true here
		return Expressions.TRUE;
		
	}
	
}
