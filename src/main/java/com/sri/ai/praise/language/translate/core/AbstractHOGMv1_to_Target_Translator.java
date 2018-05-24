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
package com.sri.ai.praise.language.translate.core;

import java.io.PrintWriter;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.praise.inference.HOGMExpressionBasedModel;
import com.sri.ai.praise.inference.ExpressionBasedModel;
import com.sri.ai.praise.language.ModelLanguage;
import com.sri.ai.praise.model.v1.hogm.antlr.HOGMParserWrapper;
import com.sri.ai.praise.model.v1.hogm.antlr.ParsedHOGModel;
import com.sri.ai.util.Util;

/**
 * Abstract base class for HOGMv1->[some target] translations.
 * 
 * @author oreilly
 *
 */
@Beta
public abstract class AbstractHOGMv1_to_Target_Translator extends AbstractTranslator {
	//
	// START-Translator
	@Override
	public ModelLanguage getSource() {
		return ModelLanguage.HOGMv1;
	}
	// END-Translator
	//
	
	@Override
	protected void translate(String inputIdentifier, Reader[] inputModelReaders, PrintWriter[] translatedOutputs) throws Exception {	
		//
		// 1. Get the HOGM FactorNetwork Definition and Parse It
		String hogmv1Model = Util.readAll(inputModelReaders[0]);
		HOGMParserWrapper parser          = new HOGMParserWrapper();
		ParsedHOGModel    parsedModel     = parser.parseModel(hogmv1Model);
		ExpressionBasedModel   factorsAndTypes = new HOGMExpressionBasedModel(parsedModel);
		
		// Each additional input is treated as an evidence expression
		List<Expression> evidence = new ArrayList<>();
		if (inputModelReaders.length > 1) {
			for (int i = 1; i < inputModelReaders.length; i++) {
				evidence.add(Expressions.parse(Util.readAll(inputModelReaders[i])));
			}
		}
		
		translate(inputIdentifier, factorsAndTypes, evidence, translatedOutputs);
	}
	
	protected abstract void translate(String identifier, ExpressionBasedModel hogmv1FactorsAndTypes, List<Expression> evidence, PrintWriter[] translatedOutputs) throws Exception;
}
