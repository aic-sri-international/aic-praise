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

import static com.sri.ai.grinder.sgdpllt.core.solver.AbstractQuantifierEliminationStepSolver.startCountingOfIntegrationsOverGroups;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.time;

import java.util.ArrayList;
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
import com.sri.ai.praise.model.v1.HOGModelError;
import com.sri.ai.praise.model.v1.HOGModelException;
import com.sri.ai.praise.model.v1.hogm.antlr.HOGMParserWrapper;
import com.sri.ai.praise.model.v1.hogm.antlr.ParsedHOGModel;
import com.sri.ai.praise.model.v1.hogm.antlr.UnableToParseAllTheInputError;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;

@Beta
public class HOGMQueryRunner {
	
	private String model;
	private HOGMParserWrapper parser = new HOGMParserWrapper();
	private ParsedHOGModel parsedModel = null;
	private List<HOGMQueryResult> results = new ArrayList<>();
	private List<HOGMQueryError> errors = new ArrayList<>();
	private boolean canceled = false;
	private Theory optionalTheory = null;
	private InferenceForFactorGraphAndEvidence inferencer = null;
	
	public HOGMQueryRunner(String model, String query) {
		this(model, list(query));
	}
	
	public HOGMQueryRunner(String model, List<String> queries) {
		initializeModel(model);
        processAllQueries(queries);
	}

	private void initializeModel(String model) {
		try {
			this.model = model;
			this.parsedModel = parseModel();
		}
		catch (RecognitionException recognitionError) {
			collectQueryError(recognitionError);
		}
		catch (UnableToParseAllTheInputError unableToParseAllTheInputError) {
			collectQueryError(unableToParseAllTheInputError);
		}
		catch (HOGModelException modelException) {
			collectQueryErrors(modelException);
		}
		catch (Throwable throwable) {
			collectQueryError(throwable);
		}
	}

	private void processAllQueries(List<String> queries) {
		for (String query : queries) {
        	processQuery(query);
        }
	}

	public List<HOGMQueryResult> getResults() {
        return results;
    }

	public Theory getOptionalTheory() {
		return optionalTheory;
	}
	
	public void setOptionalTheory(Theory theory) {
		this.optionalTheory = theory;
	}
	
	private void processQuery(String query) {
		long queryProcessingStartingTime = System.currentTimeMillis();
		collectQueryResults(query, parsedModel);
		collectQueryResultBasedInErrors(query, parsedModel, queryProcessingStartingTime);
	}

	private ParsedHOGModel parseModel() {
		ParsedHOGModel parsedModel = null;
    	if (isEmpty(model)) {
			HOGMQueryError error = new HOGMQueryError(HOGMQueryError.Context.MODEL, "Model not specified");
			errors.add(error);
		}
    	else {
    		parsedModel = parser.parseModel(model, new ParserErrorListener(HOGMQueryError.Context.MODEL, errors));
    	}
		return parsedModel;
	}

	private void collectQueryResults(String query, ParsedHOGModel parsedModel) {
		
		collectErrorIfQueryIsEmpty(query);
		   		
		if (errors.size() == 0) {
			Expression queryExpression = parser.parseTerm(query, new ParserErrorListener(HOGMQueryError.Context.QUERY, errors));
			if (errors.size() == 0) {
				runInference(query, queryExpression, parsedModel);
			}
		}
	}

	private void collectErrorIfQueryIsEmpty(String query) {
		if (isEmpty(query)) {
			HOGMQueryError error = new HOGMQueryError(HOGMQueryError.Context.QUERY, "Query not specified");
			errors.add(error);
		}
	}

	private void runInference(String query, Expression queryExpression, ParsedHOGModel parsedModel) {
		if (!canceled) {
			startCountingOfIntegrationsOverGroups();
			FactorsAndTypes factorsAndTypes = new ExpressionFactorsAndTypes(parsedModel);
			inferencer = new InferenceForFactorGraphAndEvidence(factorsAndTypes, false, null, true, getOptionalTheory());
			Pair<Expression, Long> inferenceResultAndTime = time(inference(queryExpression)); 			
			HOGMQueryResult queryResult = new HOGMQueryResult(query, queryExpression, parsedModel, inferenceResultAndTime);
			queryResult.recordNumberOfSummations();
			results.add(queryResult);
		}
	}

	private NullaryFunction<Expression> inference(Expression queryExpression) {
		final Expression finalQueryExpression = queryExpression;
		NullaryFunction<Expression> inference = () -> inferencer.solve(finalQueryExpression);
		return inference;
	}

	private void collectQueryError(RecognitionException recognitionError) {
		HOGMQueryError error = new HOGMQueryError(HOGMQueryError.Context.MODEL, recognitionError);
		errors.add(error);
	}

	private void collectQueryError(UnableToParseAllTheInputError unableToParseAllTheInputError) {
		HOGMQueryError error = new HOGMQueryError(unableToParseAllTheInputError);
		errors.add(error);
	}

	private void collectQueryErrors(HOGModelException modelException) {
		modelException.getErrors().forEach(modelError ->  collectQueryError(modelError));
	}

	private void collectQueryError(HOGModelError modelError) {
		HOGMQueryError error = makeHOGMQueryError(modelError);
		errors.add(error);
	}

	private HOGMQueryError makeHOGMQueryError(HOGModelError modelError) {
		String statement    = modelError.getStatementInfo().statement.toString();
		String source       = modelError.getStatementInfo().sourceText;
		String subStatement = modelError.getMessage(); 
		String info = makeInfo(statement, subStatement, source);
		HOGMQueryError error = makeHOGMQueryError(modelError, info);
		return error;
	}

	private String makeInfo(String statement, String subStatement, String source) {
		String info = makeInfoWithStatementAndSubstatementIfNeeded(statement, subStatement, source);
		info = addSourceAndStatementToInfoIfNeeded(info, statement, source);
		return info;
	}

	private String makeInfoWithStatementAndSubstatementIfNeeded(String statement, String subStatement, String source) {
		String info;
		if (subStatement.equals("") || subStatement.equals(source)) {
			info = " in '" + statement + "'";
		}
		else {
			info = " ('" + subStatement + "') in '" + statement + "'";
		}
		return info;
	}

	private String addSourceAndStatementToInfoIfNeeded(String info, String statement, String source) {
		String sourceMinusSpacesAndCommas = source.replaceAll(" ", "").replaceAll(";", "");
		String statementMinusSpaces = statement.replaceAll(" ", "");
		String newInfo;
		if (!sourceMinusSpacesAndCommas.equals(statementMinusSpaces)) {
			newInfo = info + " derived from '" + source + "'";
		}
		else {
			newInfo = info;
		}
		return newInfo;
	}

	private HOGMQueryError makeHOGMQueryError(HOGModelError modelError, String info) {
		LinePortion linePortion = new LinePortion(modelError.getStatementInfo().startIndex, modelError.getStatementInfo().endIndex);
		HOGMQueryError error = 
				new HOGMQueryError(
						HOGMQueryError.Context.MODEL, 
						modelError.getErrorType().formattedMessage() + info, 
						modelError.getStatementInfo().line, 
						linePortion);
		return error;
	}

	private void collectQueryError(Throwable throwable) {
		HOGMQueryError error = new HOGMQueryError(throwable);
		errors.add(error);
	}

	private void collectQueryResultBasedInErrors(String query, ParsedHOGModel parsedModel, long queryProcessingStartingTime) {
		if (errors.size() > 0) {
			long queryProcessingEndingTime = System.currentTimeMillis();
			long time = queryProcessingEndingTime - queryProcessingStartingTime;
			HOGMQueryResult queryResult = new HOGMQueryResult(query, parsedModel, errors, time);
			results.add(queryResult);
			errors.clear();
		}
	}

	private boolean isEmpty(String string) {
		boolean result = string == null || string.trim().equals("");
		return result;
	}
	
	public void cancelQuery() {
		canceled = true;
		if (inferencer != null) {
			inferencer.interrupt();
		}
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
	
	protected class ParserErrorListener implements Parser.ErrorListener {
		
		HOGMQueryError.Context context;
		List<HOGMQueryError> errors;
		
		ParserErrorListener(HOGMQueryError.Context context, List<HOGMQueryError> errors) {
			this.context = context;
			this.errors  = errors;
		}

		@Override
		public void parseError(Object offendingSymbol, int line, int charPositionInLine, String message, Exception exception) {
			LinePortion portion = new LinePortion(exception); 
			String messageWithLineInformation = addLineInformation(line, charPositionInLine, message);
			HOGMQueryError error = new HOGMQueryError(context, messageWithLineInformation, line, portion);
			errors.add(error);
		}

		private String addLineInformation(int line, int charPositionInLine, String message) {
			return "Error at line " + line + " column " + charPositionInLine + " - " + message;
		}
	} 
}
