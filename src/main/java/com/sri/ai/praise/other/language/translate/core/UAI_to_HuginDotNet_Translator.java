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
package com.sri.ai.praise.other.language.translate.core;

import java.io.PrintWriter;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.core.model.core.hogm.export.HuginOutput;
import com.sri.ai.praise.core.model.core.uai.UAIModel;
import com.sri.ai.praise.other.language.ModelLanguage;
import com.sri.ai.praise.other.language.grounded.transform.TransformMarkovToBayes;

/**
 * Translator: UAI->HuginDotNet
 * 
 * @author oreilly
 *
 */
@Beta
public class UAI_to_HuginDotNet_Translator extends AbstractUAI_to_Target_Translator {
	//
	// START-Translator	
	@Override 
	public ModelLanguage getTarget() {
		return ModelLanguage.HuginDotNet;
	}
	// END-Translator
	//
	
	@Override
	protected void translate(String inputIdentifier, UAIModel uaiModel, PrintWriter[] translatedOutputs) throws Exception {	
		PrintWriter huginDotNetModelWriter = translatedOutputs[0];
		
		// 
		// 1. Collect the data required by the Hugin Output utility.
		Map<Integer, String>       varIdxToName        = new LinkedHashMap<>();
		Map<Integer, List<String>> varIdxToRangeValues = new LinkedHashMap<>();
		for (int i = 0; i < uaiModel.numberVariables(); i++) {
			final String varName = "v"+i;
			varIdxToName.put(i, varName);
			varIdxToRangeValues.put(i, IntStream.range(0, uaiModel.cardinality(i)).boxed().map(cValue -> varName+"c"+cValue).collect(Collectors.toList()));
		}
		
		//
		// 2. Transform the UAI Markov Network representation to the Hugin dot Net Bayesian Network format.
		TransformMarkovToBayes.transform(uaiModel, new HuginOutput(huginDotNetModelWriter, varIdxToName, varIdxToRangeValues));
	}
}
