/*
 * Copyright (c) 2016, SRI International
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
package com.sri.ai.praise.evaluate.solver.impl.vec;

import java.io.File;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.List;

import com.sri.ai.praise.evaluate.solver.SolverEvaluatorProbabilityEvidenceResult;
import com.sri.ai.praise.evaluate.solver.impl.AbstractSolverEvaluator;
import com.sri.ai.praise.lang.ModelLanguage;
import com.sri.ai.praise.lang.translate.Translator;
import com.sri.ai.praise.lang.translate.TranslatorFactory;
import com.sri.ai.util.math.Rational;

/**
 * Wrapper around Vibhav's UAI 2014 Solver, available from:<br>
 * http://www.hlt.utdallas.edu/~vgogate/vec-uai14.tar.gz
 * 
 * @author oreilly
 *
 */
public class VECSolverEvaluator extends AbstractSolverEvaluator {
	
	private static final String _vecProgramName           = "vec-uai14";
	private static final String _probabilityEvidenceQuery = "PR";

	@Override
	public SolverEvaluatorProbabilityEvidenceResult solveProbabilityEvidence(String solveRequestId, ModelLanguage modelLanguage, String model, String evidenceQuery) 
		throws Exception {
		
		if (modelLanguage != ModelLanguage.HOGMv1) {
			throw new UnsupportedOperationException(modelLanguage.name() + " is currently not supported by this solver.");
		}
		
		Translator inputToUAITranslator = TranslatorFactory.newTranslator(modelLanguage, ModelLanguage.UAI);
		
		// NOTE: This trick is dependent on the input model being HOGMv1
		String hogmv1Model = model + "\nrandom UAIQuery : Boolean;\nif "+evidenceQuery+" then UAIQuery else not UAIQuery;\n";
	
		VECCallResult partitionResult = callVECPR("Partition Function "+solveRequestId, inputToUAITranslator, new Reader[] {new StringReader(hogmv1Model)});
		VECCallResult evidenceResult  = callVECPR("Evidence "+solveRequestId, inputToUAITranslator, new Reader[] {new StringReader(hogmv1Model), new StringReader("UAIQuery")});
		
		Double probabilityResult = Math.pow(10, evidenceResult.resultLog10) / Math.pow(10, partitionResult.resultLog10);
		SolverEvaluatorProbabilityEvidenceResult result = new SolverEvaluatorProbabilityEvidenceResult(
					Math.max(partitionResult.translationTookMS, evidenceResult.translationTookMS),
					Math.max(partitionResult.vecProcessTookMS, evidenceResult.vecProcessTookMS),
					probabilityResult.isNaN() ? null : new Rational(probabilityResult)
				);
		
		return result;
	}
	
	@Override
	public ModelLanguage getExpectedModelLanguage() {
		return ModelLanguage.UAI;
	}
	
	//
	// PRIVATE
	private VECCallResult callVECPR(String identifier, Translator inputToUAITranslator, Reader[] input) throws Exception {
		long translationStart = System.currentTimeMillis();
		File tempUAI    = File.createTempFile("vec", ".uai", getConfiguration().getWorkingDirectory());
		File tempEvid   = File.createTempFile("vec", ".uai.evid", getConfiguration().getWorkingDirectory());
		File tempResult = new File(getConfiguration().getWorkingDirectory(), tempUAI.getName()+"."+_probabilityEvidenceQuery);
		//
		File tempSTDERR = File.createTempFile("vec", ".stderr", getConfiguration().getWorkingDirectory());
		File tempSTDOUT = File.createTempFile("vec", ".stdout", getConfiguration().getWorkingDirectory());
		
		PrintWriter pwUAIModel    = new PrintWriter(tempUAI);
		PrintWriter pwUAIEvidence = new PrintWriter(tempEvid);
		inputToUAITranslator.translate(identifier, input, new PrintWriter[] {pwUAIModel, pwUAIEvidence});
		pwUAIModel.flush();
		pwUAIEvidence.flush();
		
		long translationEnd = System.currentTimeMillis();
		
		ProcessBuilder processBuilder = new ProcessBuilder();
		processBuilder.environment().put("INF_TIME", ""+getConfiguration().getTotalCPURuntimeLimitSecondsPerSolveAttempt());
		// Note: VEC's memory limit is specified in GB
		processBuilder.environment().put("INF_MEMORY", ""+(getConfiguration().getTotalMemoryLimitInMegabytesPerSolveAttempt() / 1024.0));
		processBuilder.directory(getConfiguration().getWorkingDirectory());
		processBuilder.command(_vecProgramName, tempUAI.getName(), tempEvid.getName(), "dummy", _probabilityEvidenceQuery);
		processBuilder.redirectError(ProcessBuilder.Redirect.to(tempSTDERR));
		processBuilder.redirectOutput(ProcessBuilder.Redirect.to(tempSTDOUT));
		
		
		long vecStart = System.currentTimeMillis();
		Process vecProcess = processBuilder.start();
		vecProcess.waitFor();
		long vecEnd = System.currentTimeMillis();
		
		List<String> results = Files.readAllLines(tempResult.toPath(), StandardCharsets.UTF_8);
		
		tempUAI.delete();
		tempEvid.delete();
		tempResult.delete();
		//
		tempSTDOUT.delete();
		tempSTDERR.delete();
		
		VECCallResult result = new VECCallResult();
		result.translationTookMS = translationEnd - translationStart;
		result.vecProcessTookMS  = vecEnd - vecStart;
		try {
			result.resultLog10  = new Double(results.get(results.size()-1));
		}
		catch (Throwable t) {
			result.resultLog10 = Double.NaN;
		}
		
		return result;
	}
	
	class VECCallResult {
		public long translationTookMS;
		public long vecProcessTookMS;
		public double resultLog10;
	}
}