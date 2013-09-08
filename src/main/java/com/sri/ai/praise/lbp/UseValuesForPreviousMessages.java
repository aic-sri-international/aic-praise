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
public interface UseValuesForPreviousMessages {
	/**
	 * <pre>
	 * use_values_for_previous_msgs(expansion, msg_values)
	 * expansion is a basic expression with occurrences of "previous messages" expressions
	 * msg_values is a union of sets of tuples (N1, N2, value) where (N1,N2) represents a 
	 * message and value is a basic expression representing its value at the current loopy 
	 * BP iteration (this expression is therefore free of previous message expressions).
	 * Returns a basic expression resulting from replacing the "previous messages" 
	 * expressions in the expansion by their corresponding values according to msg_values, 
	 * and evaluating it.
	 *         
	 * substituted <- expansion
	 * 
	 * exhaustively replace all expressions in substituted with the following replacement function: 
	 * // use ReplacementFunctionWithContextuallyUpdatedProcess        
	 * lambda E {
	 *     if E is not of the form "previous message to Destination' from Origin'
	 *         return E
	 *         
	 *     value <- find_msg_value_matching_previous_message(E, msg_values)
	 *    
	 *     return value
	 * }
	 * return R_complete_normalize(substituted)
	 * </pre>
	 * 
	 * @param expansion
	 *            a basic expression with occurrences of "previous messages"
	 *            expressions.
	 * @param msgValues
	 *            union of sets of tuples (N1, N2, value) where (N1,N2)
	 *            represents a message and value is a basic expression
	 *            representing its value at the current loopy BP iteration (this
	 *            expression is therefore free of previous message expressions).
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return a basic expression resulting from replacing the "previous
	 *         messages" expressions in the expansion by their corresponding
	 *         value according to msg_values, and evaluating it.
	 */
	Expression useValuesForPreviousMessages(Expression expansion,
			Expression msgValues, RewritingProcess process);
}
