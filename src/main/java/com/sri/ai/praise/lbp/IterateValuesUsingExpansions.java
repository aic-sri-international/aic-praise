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
package com.sri.ai.praise.lbp;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;

@Beta
public interface IterateValuesUsingExpansions {
	/**
	 * <pre>
	 * iterate_values_using_expansions(msg_values, msg_expansions)
	 * Updates the values associated with messages by plugging the current values as values 
	 * for previous messages in given expansions, evaluating these expansions, and using 
	 * the result as the new value after one iteration of loopy BP.
	 * 
	 * next_msg_values <- empty set 
	 * for each union argument { (on I) (Destination, Origin, Expansion) | C } in msg_expansions
	 *     value <- use_values_for_previous_msgs(Expansion, msg_values)
	 *     next_msg_values <- next_msg_values union { (on I) (Destination, Origin, value) | C }
	 * return next_msg_values
	 * </pre>
	 * 
	 * @param msgValues
	 *            union of sets of tuples (N1, N2, value) where (N1,N2)
	 *            represents a message and value is a basic expression
	 *            representing its value at the current loopy BP iteration (this
	 *            expression is therefore free of previous message expressions).
	 * @param msgExpansions
	 *            a union of intensional sets contaning tuples of the form
	 *            (Destination, Origin, Expansion).
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return an updated msgValues based on an iteration using the expansions.
	 */
	Expression iterateValuesUsingExpansions(Expression msgValues,
			Expression msgExpansions, RewritingProcess process);
}
