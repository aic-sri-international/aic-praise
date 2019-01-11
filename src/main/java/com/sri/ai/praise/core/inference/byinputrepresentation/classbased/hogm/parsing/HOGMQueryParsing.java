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
package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.parsing;

import static com.sri.ai.util.Util.isNullOrEmptyString;
import static com.sri.ai.util.Util.myAssert;

import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.solver.HOGMProblemResult;
import com.sri.ai.praise.core.representation.classbased.hogm.HOGModel;
import com.sri.ai.praise.core.representation.classbased.hogm.parsing.HOGMParserWrapper;

@Beta
public class HOGMQueryParsing {
	
	private HOGMParserWrapper parser = new HOGMParserWrapper();
	private List<HOGMProblemError> errors;
	private Expression queryExpression;
	private HOGMProblemResult parsingErrorResult = null;
	private HOGModel hogmModel;
	
	public HOGMQueryParsing(String query, HOGModel hogmModel, List<? extends HOGMProblemError> modelErrors) {
		this.errors = new LinkedList<>(modelErrors);
		this.hogmModel = hogmModel;
		parseQuery(query);
	}

	private void parseQuery(String query) {
		long queryProcessingStartingTime = System.currentTimeMillis();
		if (isNullOrEmptyString(query)) {
			registerEmptyQueryError();
		}
		else {
			makeQueryExpressionFromNonEmptyQueryString(query);
		}
		long queryProcessingEndingTime = System.currentTimeMillis();
		long time = queryProcessingEndingTime - queryProcessingStartingTime;
		makeQueryResult(query, time);
	}

	private void registerEmptyQueryError() {
		queryExpression = null;
		HOGMProblemError error = new HOGMProblemError(HOGMProblemError.Scope.QUERY, "Query not specified");
		errors.add(error);
	}

	private void makeQueryExpressionFromNonEmptyQueryString(String query) {
		queryExpression = parser.parseTerm(query, new HOGMParserErrorListener(HOGMProblemError.Scope.QUERY, errors));
	}
	
	private void makeQueryResult(String query, long time) {
		if (errors.size() != 0) {
			parsingErrorResult = new HOGMProblemResult(query, hogmModel, errors, time);
		}
		else {
			parsingErrorResult = null;
		}
	}

	public Expression getQueryExpression() {
		return queryExpression;
	}
	
	public HOGMProblemResult getParsingErrorProblemResult() {
		myAssert(parsingErrorResult != null, () -> getClass() + "'s parsing error problem result requested, but there were no parsing errors; test with method 'succeeded' first.");
		return parsingErrorResult;
	}
	
	public boolean succeeded() {
		boolean result = parsingErrorResult == null;
		return result;
	}
}
