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
package com.sri.ai.praise.core;

import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.library.FunctorConstants.DIVISION;
import static com.sri.ai.grinder.library.FunctorConstants.SUM;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.grinder.api.Context;


/**
 * @author braz
 *
 */
public class PRAiSEUtil {

	/**
	 * Normalizes an expression representing a potential on a given variable.
	 * This requires the given context to have that variable registered with the appropriate type,
	 * and to be able to {@link Context#evaluate(Expression)} the normalized expression definition
	 * <code>expression/sum({{(on variable in &lt;variable type&gt;) expression}})</code>.
	 * @param variable
	 * @param expression
	 * @param context
	 * @return
	 */
	public static Expression normalize(Expression variable, Expression expression, Context context) {
		Expression set = DefaultIntensionalMultiSet.intensionalMultiSet(variable, expression, TRUE, context);
		Expression sum = apply(SUM, set);
		Expression normalizedDefinition = apply(DIVISION, expression, sum);
		Expression result = context.evaluate(normalizedDefinition);
		return result;
	}

}