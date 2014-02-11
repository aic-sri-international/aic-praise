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

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.AbstractReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.library.CommutativeAssociative;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPRewriter;

/**
 * Default implementation of {@link LBPRewriter#R_extract_previous_msg_sets}.
 * 
 * @author oreilly
 * 
 */
@Beta
public class ExtractPreviousMessageSets extends AbstractLBPHierarchicalRewriter implements LBPRewriter {	
	public ExtractPreviousMessageSets() {
		
	}
	
	@Override
	public String getName() {
		return R_extract_previous_msg_sets;
	}
	
	
	/**
	 * @see LBPRewriter#R_extract_previous_msg_sets
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression expressionE = null;
		
		Justification.begin("Going to extract messages embedded in {}", expression);
		
		// Assert input arguments
		// It can be either
		// a tuple of the form: (E, tuple(I1, ..., In))
		// where E is expected to be a basic expression and I1, ..., In are scoping variables
		// or E alone
		// We may want to enforce the two-argument form down the line.
		if (Tuple.isTuple(expression) && Tuple.size(expression) == 2) {
			expressionE      = Tuple.get(expression, 0);
		}
		else {
			expressionE = expression;
		}		
		
		Expression result = null;

		Set<Expression> extractedPreviousMessages = new LinkedHashSet<Expression>();
			
		extractedPreviousMessages = extractPreviousMessages(expressionE, process);

		result = CommutativeAssociative.make(FunctorConstants.UNION, new ArrayList<Expression>(extractedPreviousMessages), Sets.EMPTY_SET);
		
		Justification.end("Embedded messages are {}", result);
		return result;
	}

	private Set<Expression> extractPreviousMessages(Expression expressionE, RewritingProcess process) {
		ScanningFunction scanningFunction = new ScanningFunction();
		expressionE.replaceAllOccurrences(scanningFunction, process);
		Set<Expression> result = scanningFunction.extractedPreviousMessages;
		return result;
	}
	
	/** A "replacement" function that is not used for replacement, but for scanning expressions in search of previous iteration messages. */
	private static class ScanningFunction extends AbstractReplacementFunctionWithContextuallyUpdatedProcess {

		public Set<Expression> extractedPreviousMessages = new LinkedHashSet<Expression>();
		
		@Override
		public Expression apply(Expression expression, RewritingProcess process) {
			if (LPIUtil.isPreviousMessage(expression)) {
				Expression tuplePair = Tuple.make(expression.get(0), expression.get(1));
				List<Expression> indexExpressions = IndexExpressions.getIndexExpressionsFromVariablesAndDomains(process.getContextualVariablesAndDomains());
				Expression set = IntensionalSet.makeUniSetFromIndexExpressionsList(indexExpressions, tuplePair, process.getContextualConstraint());
				// at some point I tried simplifying the set here, but because its condition is the contextual constraint in the process,
				// this would cause the condition to be always simplified to true (since it implies itself).
				// if we attempt simplification here, it needs to be with a process with contextual constraint equal to true.
				extractedPreviousMessages.add(set);
			} 
			
			return expression; // does not replace anything; just used for going over expression and collecting sets of previous messages.
		}
	};
}
