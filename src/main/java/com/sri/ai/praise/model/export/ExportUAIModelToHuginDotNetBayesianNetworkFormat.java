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
package com.sri.ai.praise.model.export;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.sgsolver.model.grounded.transform.XFormMarkovToBayes;
import com.sri.ai.praise.model.imports.uai.UAIModel;
import com.sri.ai.praise.model.imports.uai.UAIModelReader;

/**
 * Utility class for converting UAI Models to Hugin Dot Net Models.
 * 
 * @author oreilly
 */
@Beta
public class ExportUAIModelToHuginDotNetBayesianNetworkFormat {
	
	public static void main(String[] args) throws IOException {	
		if (args.length != 2) {
			throw new IllegalArgumentException("Must specify UAI model directory or model file to solve and the directory the Hugin files should be output to.");
		}
		File uaiInput = new File(args[0]);
		if (!uaiInput.exists()) {
			throw new IllegalArgumentException("UAI model directory or file does not exist: "+uaiInput.getAbsolutePath());
		}
		File outputDir = new File(args[1]);
		if (!outputDir.isDirectory()) {
			throw new IllegalArgumentException("Hugin output directory does not exists: "+outputDir.getAbsolutePath());
		}
		
		if (uaiInput.isDirectory()) {
			for (File uaiFile : uaiInput.listFiles((dir, name) -> name.endsWith(".uai"))) {
				export(uaiFile, outputDir);
			}
		}
		else {
			export(uaiInput, outputDir);
		}
	}
	
	public static void export(File uaiFile, File outputDirectory) throws IOException {
		UAIModel model = UAIModelReader.read(uaiFile);
		
		System.out.println(model.toString());
		
		// Collect the data required by the Hugin Output utility.
		Map<Integer, String>       varIdxToName        = new LinkedHashMap<>();
		Map<Integer, List<String>> varIdxToRangeValues = new LinkedHashMap<>();
		for (int i = 0; i < model.numberVariables(); i++) {
			final String varName = "v"+i;
			varIdxToName.put(i, varName);
			varIdxToRangeValues.put(i, IntStream.range(0, model.cardinality(i)).boxed().map(cValue -> varName+"c"+cValue).collect(Collectors.toList()));
		}
		
		File outFile = new File(outputDirectory, uaiFile.getName()+".net");
		try (final Writer writer = new BufferedWriter(new FileWriter(outFile))) {
			XFormMarkovToBayes.transform(model, new HuginOutput(writer, varIdxToName, varIdxToRangeValues));
		}
		catch (Throwable t) {
			System.err.println("Failed to export "+uaiFile.getName()+", "+t.getMessage());
			System.err.flush();
			outFile.delete(); // Remove partial output
		}
	}
}