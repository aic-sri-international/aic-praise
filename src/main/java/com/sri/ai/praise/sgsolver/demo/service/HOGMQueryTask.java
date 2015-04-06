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
package com.sri.ai.praise.sgsolver.demo.service;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.model.RandomVariableDeclaration;
import com.sri.ai.praise.model.SortDeclaration;
import com.sri.ai.praise.sgsolver.hogm.antlr.ErrorListener;
import com.sri.ai.praise.sgsolver.hogm.antlr.HOGMParserWrapper;
import com.sri.ai.praise.sgsolver.hogm.antlr.UnableToParseAllTheInputError;
import com.sri.ai.praise.sgsolver.model.HOGModelException;
import com.sri.ai.praise.sgsolver.solver.InferenceForFactorGraphAndEvidence;

import javafx.concurrent.Task;

@Beta
public class HOGMQueryTask extends Task<QueryResult> {
	private String query;
	private String model;
	//
	private List<QueryError> errors = new ArrayList<>();
	//
	private InferenceForFactorGraphAndEvidence inferencer = null;
	
	public HOGMQueryTask(String query, String model) {
		this.query = query;
		this.model = model;
	}
	
	@Override
	public QueryResult call() {
    	QueryResult result = null; 
    	long start = System.currentTimeMillis();
    	try {
    		HOGMParserWrapper parser = new HOGMParserWrapper();
    		Expression queryExpr      = parser.parseTerm(query, new LexerErrorListener(QueryError.Context.QUERY), new ParserErrorListener(QueryError.Context.QUERY));
    		Expression modelTupleExpr = parser.parse(model, new LexerErrorListener(QueryError.Context.MODEL), new ParserErrorListener(QueryError.Context.MODEL));
    		
    		if (errors.size() == 0) {
    			List<SortDeclaration>           sorts               = extractSorts(Tuple.get(modelTupleExpr, 0));
    			List<RandomVariableDeclaration> randoms             = extractRandom(Tuple.get(modelTupleExpr, 1));
    			List<Expression>                conditiedPotentials = extractConditionedPotentials(Tuple.get(modelTupleExpr, 2));
    			
    			Map<String, String> mapFromTypeNameToSizeString   = new LinkedHashMap<>();
    			sorts.forEach(sort -> {
    				if (!sort.getSize().equals(SortDeclaration.UNKNOWN_SIZE)) {
    					mapFromTypeNameToSizeString.put(sort.getName().toString(), sort.getSize().toString());
    				}
    			});
    			mapFromTypeNameToSizeString.put("Boolean", "2");
    			Map<String, String> mapFromVariableNameToTypeName = new LinkedHashMap<>();
    			randoms.forEach(random -> {
    				mapFromVariableNameToTypeName.put(random.getName().toString(), random.getRangeSort().toString());
    			});
    			
    			Expression markovNetwork = Times.make(conditiedPotentials);
    			inferencer = new InferenceForFactorGraphAndEvidence(markovNetwork, false, null, true, mapFromTypeNameToSizeString, mapFromVariableNameToTypeName);
    			
    			Expression marginal = inferencer.solve(queryExpr); 			
    			
    			result = new QueryResult(query, model, marginal.toString(), System.currentTimeMillis() - start);
    		}
    	}
    	catch (RecognitionException re) {
    		errors.add(new QueryError(QueryError.Context.MODEL, re.getMessage(), re.getOffendingToken().getStartIndex(), re.getOffendingToken().getStopIndex()));
    	}
    	catch (UnableToParseAllTheInputError utpai) {
    		errors.add(new QueryError(utpai));
    	}
    	catch (HOGModelException me) {
    		me.getErrors().forEach(modelError -> {
    			errors.add(new QueryError(QueryError.Context.MODEL,
    					""+modelError.getErrorType()+": "+modelError.getMessage()+" "+modelError.getInStatementInfo().statement, 
    					modelError.getInStatementInfo().startIndex, modelError.getInStatementInfo().endIndex));
    		});
    	}
    	catch (Throwable t) {
    		// Unexpected
    		errors.add(new QueryError(t));
    	}
    	
    	if (errors.size() > 0) {
			result = new QueryResult(query, model, errors, System.currentTimeMillis() - start);
		}

        return result;
    }
	
	@Override
	protected void cancelled() {
		if (inferencer != null) {
			inferencer.interrupt();
		}
	}
	
	protected List<SortDeclaration> extractSorts(Expression sortsTuple) {
		List<SortDeclaration> result = new ArrayList<>();
		
		Tuple.getElements(sortsTuple).forEach(sortExpr -> result.add(SortDeclaration.makeSortDeclaration(sortExpr)));
		
		return result;
	}
	
	protected List<RandomVariableDeclaration> extractRandom(Expression randomsTuple) {
		List<RandomVariableDeclaration> result = new ArrayList<>();
		
		Tuple.getElements(randomsTuple).forEach(randomExpr -> result.add(RandomVariableDeclaration.makeRandomVariableDeclaration(randomExpr)));
		
		return result;
	}
	
	protected List<Expression> extractConditionedPotentials(Expression conditionedPotentialsTuple) {
		List<Expression> result = Tuple.getElements(conditionedPotentialsTuple);
		return result;
	}
	
	protected class QueryErrorListener extends ErrorListener {
		QueryError.Context context;
		QueryErrorListener(String name, QueryError.Context context) {
			super(name);
			this.context = context;
		}
		
		@Override
		public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine, String msg, RecognitionException e) {
			errors.add(new QueryError(context,
					    name +"[" + line + ":"+ charPositionInLine + "] " + msg,
					   	e.getOffendingToken().getStartIndex(),
					   	e.getOffendingToken().getStopIndex()
					));
			errorsDetected = true;
		}
	} 
	
	private class LexerErrorListener extends QueryErrorListener {
		LexerErrorListener(QueryError.Context context) {
			super("Lexer", context);
		}
	}
	
	private class ParserErrorListener extends QueryErrorListener {
		ParserErrorListener(QueryError.Context context) {
			super("Lexer", context);
		}
	}
}
