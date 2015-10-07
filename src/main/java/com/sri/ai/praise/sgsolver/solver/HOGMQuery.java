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
package com.sri.ai.praise.sgsolver.solver;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.model.v1.HOGModelException;
import com.sri.ai.praise.model.v1.hogm.antlr.ErrorListener;
import com.sri.ai.praise.model.v1.hogm.antlr.HOGMParserWrapper;
import com.sri.ai.praise.model.v1.hogm.antlr.ParsedHOGModel;
import com.sri.ai.praise.model.v1.hogm.antlr.UnableToParseAllTheInputError;
import com.sri.ai.praise.sgsolver.solver.ExpressionFactorsAndTypes;
import com.sri.ai.praise.sgsolver.solver.FactorsAndTypes;
import com.sri.ai.praise.sgsolver.solver.InferenceForFactorGraphAndEvidence;

@Beta
public class HOGMQuery {
	private String        model;
	private List<String>  queries  = new ArrayList<>();
	private boolean       canceled = false;
	//
	private InferenceForFactorGraphAndEvidence inferencer = null;
	
	public HOGMQuery(String model, String query) {
		this(model, Collections.singletonList(query));
	}
	
	public HOGMQuery(String model, List<String> queries) {
		this.model = model;
		this.queries.addAll(queries);
	}
	
	public List<HOGMQueryResult> query() {
    	List<HOGMQueryResult> result = new ArrayList<>();
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
	        			parsedModel = parser.parseModel(model, new LexerErrorListener(HOGMQueryError.Context.MODEL, errors), new ParserErrorListener(HOGMQueryError.Context.MODEL, errors));
	        		}
	        		Expression queryExpr = parser.parseTerm(query, new LexerErrorListener(HOGMQueryError.Context.QUERY, errors), new ParserErrorListener(HOGMQueryError.Context.QUERY, errors));
		
	        		if (errors.size() == 0) {
	        			FactorsAndTypes factorsAndTypes = new ExpressionFactorsAndTypes(parsedModel);
	        			if (!canceled) {
			    			inferencer = new InferenceForFactorGraphAndEvidence(factorsAndTypes, false, null, true);
			    			
			    			startQuery = System.currentTimeMillis();
			    			Expression marginal = inferencer.solve(queryExpr); 			
			    			
			    			result.add(new HOGMQueryResult(query, parsedModel, marginal.toString(), System.currentTimeMillis() - startQuery));
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
				result.add(new HOGMQueryResult(query, parsedModel, errors, System.currentTimeMillis() - startQuery));
			}
        }

        return result;
    }
	
	public void cancelQuery() {
		canceled = true;
		if (inferencer != null) {
			inferencer.interrupt();
		}
	}
	
	protected class QueryErrorListener extends ErrorListener {
		HOGMQueryError.Context context;
		List<HOGMQueryError> errors;
		QueryErrorListener(String name, HOGMQueryError.Context context, List<HOGMQueryError> errors) {
			super(name);
			this.context = context;
			this.errors  = errors;
		}
		
		@Override
		public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine, String msg, RecognitionException e) {
			int start = 0; 
			int end   = 0; 
			if (e != null && e.getOffendingToken() != null) {
				start = e.getOffendingToken().getStartIndex();
				end   = e.getOffendingToken().getStopIndex();
			}
			if (start > end) {
				start = end;
			}
			errors.add(new HOGMQueryError(context,
					    name +" error at line " + line + " column "+ charPositionInLine + " - " + msg,
					    line,
					   	start,
					   	end
					));
			errorsDetected = true;
		}
	} 
	
	private class LexerErrorListener extends QueryErrorListener {
		LexerErrorListener(HOGMQueryError.Context context, List<HOGMQueryError> errors) {
			super("Lexer", context, errors);
		}
	}
	
	private class ParserErrorListener extends QueryErrorListener {
		ParserErrorListener(HOGMQueryError.Context context, List<HOGMQueryError> errors) {
			super("Lexer", context, errors);
		}
	}
}
