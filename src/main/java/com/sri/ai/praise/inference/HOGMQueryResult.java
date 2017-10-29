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
package com.sri.ai.praise.inference;

import static com.sri.ai.grinder.sgdpllt.core.solver.AbstractQuantifierEliminationStepSolver.getNumberOfIntegrationsOverGroup;
import static com.sri.ai.grinder.sgdpllt.core.solver.AbstractQuantifierEliminationStepSolver.isCountingIntegrationsOverGroups;

import java.util.ArrayList;
import java.util.List;
import java.util.StringJoiner;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.group.Sum;
import com.sri.ai.grinder.sgdpllt.group.SumProduct;
import com.sri.ai.praise.model.v1.hogm.antlr.ParsedHOGModel;
import com.sri.ai.util.base.Pair;

@Beta
public class HOGMQueryResult {
	private String               queryString           = null;
	private Expression           queryExpression       = null;
	private ParsedHOGModel       parsedModel           = null;
	private Expression           result                = null;
	private List<HOGMQueryError> errors                = new ArrayList<>();
	private long                 millisecondsToCompute = 0L;
	private int                  numberOfSummations    = -1;
	
	public HOGMQueryResult(String queryString, Expression queryExpression, ParsedHOGModel parsedModel, Pair<Expression, Long> resultAndTime) {
		this(queryString, queryExpression, parsedModel, resultAndTime.first, resultAndTime.second);
	}

	public HOGMQueryResult(String queryString, Expression queryExpression, ParsedHOGModel parsedModel, Expression result, long millisecondsToCompute) {
		this.queryString           = queryString;
		this.queryExpression       = queryExpression;
		this.parsedModel           = parsedModel;
		this.result                = result;
		this.millisecondsToCompute = millisecondsToCompute;
	}
	
	public HOGMQueryResult(String queryString, ParsedHOGModel parsedModel, List<HOGMQueryError> errors, long millisecondsToCompute) {
		this.queryString = queryString;
		this.queryExpression = null;
		this.parsedModel = parsedModel;
		this.errors.addAll(errors);
		this.millisecondsToCompute = millisecondsToCompute;
	}
	
	public int getNumberOfSummations() {
		return numberOfSummations;
	}

	public void recordNumberOfSummations() {
		if (isCountingIntegrationsOverGroups()) {
			this.numberOfSummations = getNumberOfIntegrationsOverGroup(new Sum()) + getNumberOfIntegrationsOverGroup(new SumProduct());
		}
	}

	public boolean isErrors() {
		boolean result = errors.size() > 0;
		return result;
	}
	
	public String getQueryString() {
		return queryString;
	}
	
	public Expression getQueryExpression() {
		return queryExpression;
	}
	
	public ParsedHOGModel getParsedModel() {
		return parsedModel;
	}
	
	public Expression getResult() {
		return result;
	}
	
	public List<HOGMQueryError> getErrors() {
		return errors;
	}
	
	public long getMillisecondsToCompute() {
		return millisecondsToCompute;
	}
	
	@Override
	public String toString() {
		String result = null;
		
		if (isErrors()) {
			StringJoiner sj = new StringJoiner("\n", "Query Errors:\n", "\n");
			errors.forEach(error -> sj.add(error.toString()));
			result = sj.toString();
		}
		else {
			result = this.result.toString();
		}
		
		return result;
	}
}
