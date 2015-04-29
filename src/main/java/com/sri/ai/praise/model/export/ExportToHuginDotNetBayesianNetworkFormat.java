/*
 * Copyright (c) 2014, SRI International
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
package com.sri.ai.praise.model.export;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.ModelGrounding;
import com.sri.ai.praise.model.ModelGrounding.GroundedModelResult;
import com.sri.ai.praise.sgsolver.model.export.HuginOutput;
import com.sri.ai.praise.sgsolver.model.grounded.transform.XFormMarkovToBayes;
import com.sri.ai.util.base.Pair;

/**
 * Exports an LBP Model, which is grounded first, to <a href="http://www.hugin.com/">Hugin's</a>
 * Bayesian Network <a href="http://download.hugin.com/webdocs/manuals/api-manual.pdf">.net</a> format.
 * 
 * @author oreilly
 *
 */
@Beta
public class ExportToHuginDotNetBayesianNetworkFormat {	
	/**
	 * Export to Hugin .net Bayesian Network format, given a HOGM or low level model, with optionally provided sort sizes (
	 * for those not already explicitly set in the model).
	 * 
	 * @param args
	 *        [0] the path to the model file
	 *        [1] 'true' if the model files is a high level model (HOGM) or 'false' if a low level model 
	 *            (i.e. what the HOGM is translated to in order to perform lifted inference).
	 *        [2] the path to the output export file to be generated (if exists will be overwritten).
	 *        [3+] optional pairs of arguments in order where each pair represents a (sort name, sort size)
	 *             to be used when grounding the model as part of the export process.
	 *             
	 * @throws IOException
	 */
	public static void main(String[] args) throws IOException {
		if (args.length < 3 || args.length % 2 != 1) {
			throw new IllegalArgumentException("Invalid # of arguments, must be 1=model file, 2=hogm model (true or false), 3=output file, and the optional 4 to n pairs of sort name followed by size to use when grounding the model");
		}
		
		// Read in the model declaration
		StringJoiner modelDeclarationSJ = new StringJoiner("\n");
		try (Stream<String> declarationLines = Files.lines(Paths.get(args[0]))) {
			declarationLines.sequential().forEachOrdered(line -> modelDeclarationSJ.add(line));
		}
		boolean isHOGModel = Boolean.valueOf(args[1]);
		String modelDeclaration = null;
		if (isHOGModel) {
			modelDeclaration = Model.fromRules(modelDeclarationSJ.toString()).getModelDeclaration();
		} 
		else {
			modelDeclaration = modelDeclarationSJ.toString();
		}
	
		Pair[] explicitSortSizes = new Pair[(args.length-3)/2];
		for (int i = 3, v = 0; i < args.length; i+=2, v++) {
			explicitSortSizes[v] = new Pair<String, Integer>(args[i], Integer.valueOf(args[i+1]));
		}
		
		try (final Writer writer = new BufferedWriter(new FileWriter(args[2]))) {
			final GroundedModelResult groundedModelResult = ModelGrounding.groundModel(modelDeclaration, explicitSortSizes);
		
			export(groundedModelResult, writer);
			
		} catch (ModelGrounding.ModelGroundingException mge) {
			System.err.println("Model Ground Exception: "+mge.getMessage());
			for (ModelGrounding.ModelGroundingError error : mge.getErrors()) {
				System.err.println(error.getErrorType());
				System.err.println(error.getInExpression());
			}
		}
	}
	
	public static void export(ModelGrounding.GroundedModelResult groundedModelResult, final Writer writer) 
			throws IOException {
		
		RewritingProcess process = groundedModelResult.getRewritingProcess();
		GroundedModelMarkovNetwork groundedMarkovNetwork = new GroundedModelMarkovNetwork(groundedModelResult);
		
		// Collect the data required by the Hugin Output utility.
		Map<Integer, String>       varIdxToName        = new LinkedHashMap<>();
		Map<Integer, List<String>> varIdxToRangeValues = new LinkedHashMap<>();
		
		for (int i = 0; i < groundedMarkovNetwork.numberVariables(); i++) {
			Expression rvExpr = groundedMarkovNetwork.getRandomVariable(i);
			String     rvName = rvExpr.toString();
			
			varIdxToName.put(i, rvName);
			varIdxToRangeValues.put(i, Model.range(rvExpr, process).stream().map(e -> e.toString()).collect(Collectors.toList()));
		}
		
		// Output the CPTs
		XFormMarkovToBayes.transform(groundedMarkovNetwork, new HuginOutput(writer, varIdxToName, varIdxToRangeValues));
	}
}