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
package com.sri.ai.praise.pimt;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.antlr.v4.runtime.RecognitionException;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Parser;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.praise.model.v1.HOGMSortDeclaration;
import com.sri.ai.praise.model.v1.HOGModelException;
import com.sri.ai.praise.model.v1.hogm.antlr.HOGMParserWrapper;
import com.sri.ai.praise.model.v1.hogm.antlr.ParsedHOGModel;
import com.sri.ai.praise.model.v1.hogm.antlr.UnableToParseAllTheInputError;
import com.sri.ai.praise.pimt.ExpressionFactorsAndTypes;
import com.sri.ai.praise.pimt.FactorsAndTypes;
import com.sri.ai.praise.pimt.InferenceForFactorGraphAndEvidence;

@Beta
public class HOGMQueryRunner {
	private String        model;
	private List<String>  queries  = new ArrayList<>();
	private boolean       canceled = false;
	private Theory        optionalTheory = null;
	//
	private InferenceForFactorGraphAndEvidence inferencer = null;
	
	public HOGMQueryRunner(String model, String query) {
		this(model, Collections.singletonList(query));
	}
	
	public HOGMQueryRunner(String model, List<String> queries) {
		this.model = model;
		this.queries.addAll(queries);
	}
	
	public Theory getOptionalTheory() {
		return optionalTheory;
	}
	
	public void setOptionTheory(Theory theory) {
		this.optionalTheory = theory;
	}
	
	public List<HOGMQueryResult> query() {
    	List<HOGMQueryResult> result = new ArrayList<>();
    	Expression queryExpr = null;
    	//
    	ParsedHOGModel parsedModel = null;
        for (String query : queries) {
        	long startQuery = System.currentTimeMillis();
        	List<HOGMQueryError> errors = new ArrayList<>();
        	try {
	    		if (model == null || model.trim().equals("")) {
	    			errors.add(new HOGMQueryError(HOGMQueryError.Context.MODEL, "Model not specified", 0, 0, 0));
	    		}
	        	  
	    		if (query == null || query.trim().equals("")) {
	    			errors.add(new HOGMQueryError(HOGMQueryError.Context.QUERY, "Query not specified", 0, 0, 0));
	    		}
	    		   		
	    		if (errors.size() == 0) {
	    	   		HOGMParserWrapper parser = new HOGMParserWrapper();
	        		if (parsedModel == null) {
	        			parsedModel = parser.parseModel(model, new QueryErrorListener(HOGMQueryError.Context.MODEL, errors));
	        		}
	        		queryExpr = parser.parseTerm(query, new QueryErrorListener(HOGMQueryError.Context.QUERY, errors));
		
	        		if (errors.size() == 0) {
	        			FactorsAndTypes factorsAndTypes = new ExpressionFactorsAndTypes(parsedModel);
	        			if (!canceled) {
			    			inferencer = new InferenceForFactorGraphAndEvidence(factorsAndTypes, false, null, true, getOptionalTheory());
			    			
			    			startQuery = System.currentTimeMillis();
			    			Expression marginal = inferencer.solve(queryExpr); 			
			    			
			    			result.add(new HOGMQueryResult(query, queryExpr, parsedModel, marginal, System.currentTimeMillis() - startQuery));
	        			}
	        		}
	    		}
	    	}
	    	catch (RecognitionException re) {
	    		errors.add(new HOGMQueryError(HOGMQueryError.Context.MODEL, re.getMessage(), re.getOffendingToken().getLine(), re.getOffendingToken().getStartIndex(), re.getOffendingToken().getStopIndex()));
	    	}
	    	catch (UnableToParseAllTheInputError utpai) {
	    		errors.add(new HOGMQueryError(utpai));
	    	}
	    	catch (HOGModelException me) {
	    		me.getErrors().forEach(modelError -> {
	    			String inStatement    = modelError.getInStatementInfo().statement.toString();
	    			String inSource       = modelError.getInStatementInfo().sourceText;
	    			String inSubStatement = modelError.getMessage(); 
	    			String inInfo = "";
	    			if (inSubStatement.equals("") || inSubStatement.equals(inSource)) {
	    				inInfo = " in '"+inStatement+"'";
	    			}
	    			else {
	    				inInfo = " ('"+inSubStatement+"') in '"+inStatement+"'";
	    			}
	    			if (!inSource.replaceAll(" ", "").replaceAll(";", "").equals(inStatement.replaceAll(" ", ""))) {
	    				inInfo = inInfo + " derived from '"+inSource+"'";
	    			}
	    			errors.add(new HOGMQueryError(HOGMQueryError.Context.MODEL,
	    					modelError.getErrorType().formattedMessage()+inInfo, 
	    					modelError.getInStatementInfo().line,
	    					modelError.getInStatementInfo().startIndex, 
	    					modelError.getInStatementInfo().endIndex));
	    		});
	    	}
	    	catch (Throwable t) {
	    		// Unexpected
	    		errors.add(new HOGMQueryError(t));
	    	}
	    	
	    	if (errors.size() > 0) {
				result.add(new HOGMQueryResult(query, queryExpr, parsedModel, errors, System.currentTimeMillis() - startQuery));
			}
        }

        return result;
    }
	
	public Expression simplifyAnswer(Expression answer, Expression forQuery) {
		Expression result  = answer;
		Context    context = getQueryContext();
		if (HOGMSortDeclaration.IN_BUILT_BOOLEAN.getName().equals(GrinderUtil.getTypeExpressionOfExpression(forQuery, context))) {
			result = result.replaceAllOccurrences(forQuery, Expressions.TRUE, context);
			result = simplifyWithinQueryContext(result);
			answer = Expressions.parse(result.toString()); // This ensures numeric values have the correct precision
		}
		return result;
	}
	
	public Context getQueryContext() {
		return inferencer.makeContextWithTypeInformation();
	}
	
	public Expression simplifyWithinQueryContext(Expression expr) {
		return inferencer.simplify(expr);
	}
	
	public void cancelQuery() {
		canceled = true;
		if (inferencer != null) {
			inferencer.interrupt();
		}
	}
	
	protected class QueryErrorListener implements Parser.ErrorListener {
		HOGMQueryError.Context context;
		List<HOGMQueryError> errors;
		QueryErrorListener(HOGMQueryError.Context context, List<HOGMQueryError> errors) {
			this.context = context;
			this.errors  = errors;
		}
		
		@Override
		public void parseError(Object offendingSymbol, int line, int charPositionInLine, String msg, Exception e) {
			int start = 0; 
			int end   = 0; 
			if (e != null && e instanceof RecognitionException) {
				RecognitionException re = (RecognitionException) e;
				if (re.getOffendingToken() != null) {
					start = re.getOffendingToken().getStartIndex();
					end   = re.getOffendingToken().getStopIndex();
				}
			}
			if (start > end) {
				start = end;
			}
			errors.add(new HOGMQueryError(context,
					    "Error at line " + line + " column "+ charPositionInLine + " - " + msg,
					    line,
					   	start,
					   	end
					));
		}
	} 
}
