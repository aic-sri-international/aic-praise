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
package com.sri.ai.praise;

import com.google.common.annotations.Beta;
import com.google.common.collect.Lists;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasKind;
import com.sri.ai.grinder.core.KindAttribute;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.praise.model.IsRandomVariableValueExpression;

/**
 * A rewriter for evaluating all "type" syntax function applications to random variable value
 * expressions (e.g. type(tall(X)) in ALBP to <code>{{false, true}}</code>.
 * 
 * @author oreilly
 */
@Beta
public class Type extends AbstractRewriter {

	public Type() {
		this.setReifiedTests(new HasKind(KindAttribute.VALUE_TYPE_SYNTACTIC_FUNCTION));
	}

	// the reason for a multiset here is that it does not trigger a
	// normalization for eliminating repeated elements.
	private static final Expression _booleanType = ExtensionalSet
			.makeMultiSet(Lists.newArrayList((Expression) Expressions.makeSymbol("false"), Expressions.makeSymbol("true")));

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		if (expression.getSyntaxTree().numberOfImmediateSubTrees() == 1
				&& IsRandomVariableValueExpression.apply(Expressions.makeFromSyntaxTree(expression.getSyntaxTree().getImmediateSubTrees().get(0)), process)
			) {
			return _booleanType;
		}

		return expression;
	}
}
