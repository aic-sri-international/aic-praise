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
public interface MessageExpansions {
	/**
	 * <pre>
	 * get_msg_expansions(msg_sets)
	 * msg_sets a union of sets, each set containing the instances of one message (destination, origin) pair.
	 * Returns a union of intensional sets contaning tuples of the form
	 * (Destination, Origin, Expansion), where Expansion is a basic expression
	 * possibly containing "previous message" expressions, representing their value in terms of messages
	 * from the previous loopy BP iteration.
	 * 
	 * msg_expansions        <- empty set
	 * msgs_already_expanded <- empty set
	 * msgs_to_be_expanded   <- msg_sets
	 * 
	 * while msgs_to_be_expanded is not empty
	 *     msg_set <- remove a union argument from msgs_to_be_expanded
	 *     msg_set <- R_set_diff(msg_set minus msgs_already_expanded)
	 *     msg_set <- R_complete_normalize(msg_set)
	 *     if msg_set is not {}
	 *         represent msg_set as { (on I) (Destination, Origin) | C }
	 *         expansion <- compute message to Destination from Origin with
	 *                              beingComputed = empty set and contextual constraint C
	 *         // the above will use either R_m_to_v_from_f or R_m_to_f_from_v as needed
	 *         
	 *         expansion <- R_complete_normalize(expansion)
	 *         
	 *         index <- size of msg_expansions
	 *         cache index as msg_value_index 
	 *                        corresponding to 'previous message to Destination from Origin'
	 *                        under constraining condition C
	 *                        
	 *         msg_expansions        <- msg_expansions 
	 *                                      union { (on I) (Destination, Origin, expansion) | C }
	 *         msgs_already_expanded <- msgs_already_expanded 
	 *                                      union { (on I) (Destination, Origin) | C }
	 *         msgs_to_be_expanded   <- msgs_to_be_expanded 
	 *                                      union R_extract_previous_msg_sets(expansion)
	 * return msg_expansions
	 * </pre>
	 * 
	 * @param msgSets
	 *            a union of sets, each set containing the instances of one
	 *            message (destination, origin) pair.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return a union of intensional sets contaning tuples of the form
	 *         (Destination, Origin, Expansion).
	 */
	Expression getMessageExpansions(Expression msgSets, RewritingProcess process);
}
