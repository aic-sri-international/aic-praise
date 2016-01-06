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
package com.sri.ai.praise.sgsolver.solver.experiment;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import com.google.common.annotations.Beta;
import com.google.common.base.Stopwatch;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.praise.lang.ModelLanguage;
import com.sri.ai.praise.lang.translate.Translator;
import com.sri.ai.praise.lang.translate.TranslatorFactory;
import com.sri.ai.praise.lang.translate.util.InputModelReaders;
import com.sri.ai.praise.lang.translate.util.TranslatedOutputs;
import com.sri.ai.praise.sgsolver.solver.ExpressionFactorsAndTypes;
import com.sri.ai.praise.sgsolver.solver.FactorsAndTypes;
import com.sri.ai.praise.sgsolver.solver.InferenceForFactorGraphAndEvidence;

/**
 * TODO - re-factor logic in this class to become part of the more general com.sri.ai.praise.evaluate
 * tool set (once it is developed).
 * 
 * @author oreilly
 */
@Beta
public class CompareSGSolverToUAISolver {
	
	public static void main(String[] args) throws IOException {
		if (args.length != 1) {
			throw new IllegalArgumentException("Root Model directory must be specified");
		}
		File rootModelDirectory          = validateDirectory(args[0]);
		Translator hogmv1ToUAITranslator = TranslatorFactory.newTranslator(ModelLanguage.HOGMv1, ModelLanguage.UAI); 
		
		File sourceDirectory = new File(rootModelDirectory, hogmv1ToUAITranslator.getSource().getCode());
		if (!sourceDirectory.isDirectory()) {
			throw new IllegalArgumentException("Source Directory "+sourceDirectory+" does not exist");
		}
		File targetDirectory = new File(rootModelDirectory, hogmv1ToUAITranslator.getTarget().getCode());
		if (!targetDirectory.isDirectory()) {
			targetDirectory.mkdir();
		}
		
		System.out.println("Problem Name,Took");
		String sourceModelFileExtension = hogmv1ToUAITranslator.getInputFileExtensions()[0];
		for (File sourceModelFile : sourceDirectory.listFiles((dir, name) -> name.endsWith(sourceModelFileExtension))) {
			
			String          hogmv1ModelString          = new String(Files.readAllBytes(sourceModelFile.toPath()), hogmv1ToUAITranslator.getSourceCharset());
			FactorsAndTypes hogmv1ModelFactorsAndTypes = new ExpressionFactorsAndTypes(hogmv1ModelString);
			
			long startSGInference = System.currentTimeMillis();
			InferenceForFactorGraphAndEvidence inferencer = new InferenceForFactorGraphAndEvidence(hogmv1ModelFactorsAndTypes, false, null, true);
			
			for (String varName : hogmv1ModelFactorsAndTypes.getMapFromRandomVariableNameToTypeName().keySet()) {
				Expression queryExpr = Expressions.makeSymbol(varName);
				inferencer.solve(queryExpr);
			}
			
			long sgInferenceTook = System.currentTimeMillis() - startSGInference;
			
			System.out.println(sourceModelFile.getName().substring(0, sourceModelFile.getName().length() - sourceModelFileExtension.length()-1)+","+sgInferenceTook);
			
			System.out.println("Translating "+sourceModelFile.getName()+" from "+hogmv1ToUAITranslator.getSource().getCode()+" to "+hogmv1ToUAITranslator.getTarget().getCode());
			String sourceModelFileNameWithNoExtension = hogmv1ToUAITranslator.getInputModelFileNameWithNoExtension(sourceModelFile);
			Stopwatch translateStopWatch = Stopwatch.createStarted();
			try (InputModelReaders inputModelReaders = new InputModelReaders(hogmv1ToUAITranslator, sourceModelFile, sourceModelFileExtension);
				 TranslatedOutputs translatedOutputs = new TranslatedOutputs(hogmv1ToUAITranslator, sourceModelFile, sourceModelFileExtension)) {
				hogmv1ToUAITranslator.translate(sourceModelFileNameWithNoExtension, inputModelReaders.readers, translatedOutputs.writers);
			}
			catch (Exception ex) {
				System.err.println("Error during translation");
				ex.printStackTrace();
				System.exit(-1);
			}
			translateStopWatch.stop();
			System.out.println("Translation took "+translateStopWatch);
		}
	}
	
	private static File validateDirectory(String directoryName) {
		File result = new File(directoryName);
		if (!result.exists()) {
			throw new IllegalArgumentException("Root model directory does not exist");
		}
		if (!result.isDirectory()) {
			throw new IllegalArgumentException("Specified root model directory is not a directory: "+result.getAbsolutePath());
		}
		
		return result;
	}
}