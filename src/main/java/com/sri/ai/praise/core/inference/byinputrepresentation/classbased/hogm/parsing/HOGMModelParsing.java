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

import java.util.List;

import org.antlr.v4.runtime.RecognitionException;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.core.representation.classbased.hogm.HOGModel;
import com.sri.ai.praise.core.representation.classbased.hogm.parsing.HOGMParserWrapper;
import com.sri.ai.praise.core.representation.classbased.hogm.parsing.UnableToParseAllTheInputError;
import com.sri.ai.praise.core.representation.classbased.hogm.validation.HOGModelError;
import com.sri.ai.praise.core.representation.classbased.hogm.validation.HOGModelException;

@Beta
public class HOGMModelParsing {
	
	private HOGModel model = null;
	private List<HOGMProblemError> errors;
	
	public HOGMModelParsing(String model, List<HOGMProblemError> errors) {
		this.errors = errors;
		initializeModel(model);
	}

	public HOGModel getModel() {
		return model;
	}

	private void initializeModel(String modelString) {
		try {
			this.model = parse(modelString);
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

	private HOGModel parse(String modelString) {
		HOGModel model;
    	if (isNullOrEmptyString(modelString)) {
    		model = null;
			collectEmptyModelStringError();
		}
    	else {
    		model = parseNonEmptyModelString(modelString);
    	}
		return model;
	}

	private void collectEmptyModelStringError() {
		HOGMProblemError error = new HOGMProblemError(HOGMProblemError.Scope.MODEL, "Model not specified");
		errors.add(error);
	}

	private HOGModel parseNonEmptyModelString(String nonEmptyModelString) {
		HOGMParserWrapper parser = new HOGMParserWrapper();
		HOGModel model = parser.parseModel(nonEmptyModelString, makeParserErrorListener());
		parser.close();
		return model;
	}

	private HOGMParserErrorListener makeParserErrorListener() {
		return new HOGMParserErrorListener(HOGMProblemError.Scope.MODEL, errors);
	}

	private void collectQueryError(RecognitionException recognitionError) {
		HOGMProblemError error = new HOGMProblemError(HOGMProblemError.Scope.MODEL, recognitionError);
		errors.add(error);
	}

	private void collectQueryError(UnableToParseAllTheInputError unableToParseAllTheInputError) {
		HOGMProblemError error = new HOGMProblemError(unableToParseAllTheInputError);
		errors.add(error);
	}

	private void collectQueryErrors(HOGModelException modelException) {
		modelException.getErrors().forEach(modelError ->  collectQueryError(modelError));
	}

	private void collectQueryError(HOGModelError modelError) {
		HOGMProblemError error = makeHOGMQueryError(modelError);
		errors.add(error);
	}

	private HOGMProblemError makeHOGMQueryError(HOGModelError modelError) {
		String statement    = modelError.getStatementInfo().statement.toString();
		String source       = modelError.getStatementInfo().sourceText;
		String subStatement = modelError.getMessage(); 
		String info = makeInfo(statement, subStatement, source);
		HOGMProblemError error = makeHOGMQueryError(modelError, info);
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

	private HOGMProblemError makeHOGMQueryError(HOGModelError modelError, String info) {
		HOGMLinePortion linePortion = new HOGMLinePortion(modelError.getStatementInfo().startIndex, modelError.getStatementInfo().endIndex);
		HOGMProblemError error = 
				new HOGMProblemError(
						HOGMProblemError.Scope.MODEL, 
						modelError.getErrorType().formattedMessage() + info, 
						modelError.getStatementInfo().line, 
						linePortion);
		return error;
	}

	private void collectQueryError(Throwable throwable) {
		HOGMProblemError error = new HOGMProblemError(throwable);
		errors.add(error);
	}
}
