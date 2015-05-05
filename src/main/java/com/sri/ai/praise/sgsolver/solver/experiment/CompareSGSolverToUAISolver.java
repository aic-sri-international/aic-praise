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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Random;
import java.util.StringJoiner;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.AbstractRandomDPLLProblemGenerator;
import com.sri.ai.praise.sgsolver.hogm.antlr.HOGMParserWrapper;
import com.sri.ai.praise.sgsolver.hogm.antlr.ParsedHOGModel;
import com.sri.ai.praise.sgsolver.model.export.UAIHOGModelGroundingListener;
import com.sri.ai.praise.sgsolver.model.grounded.model.HOGModelGrounding;
import com.sri.ai.praise.sgsolver.model.imports.uai.UAICompare;
import com.sri.ai.praise.sgsolver.solver.ExpressionFactorsAndTypes;
import com.sri.ai.praise.sgsolver.solver.FactorsAndTypes;
import com.sri.ai.praise.sgsolver.solver.InferenceForFactorGraphAndEvidence;

@Beta
public class CompareSGSolverToUAISolver {

	static int [] _domainSizes       = new int[] {2, 4, 8, 16, 32, 64, 128, 254, 1000, 10000};
	static int    _numberFactors     = 5;
	static int    _numberOfVariables = 10;
	static int    _depth             = 2;
	static int    _breadth           = 2;
	static long   _seed              = 4;
	//
	static final String _variablePrefix = "X";
	static final String _constantPrefix = "a";
	
	public static void main(String[] args) {
		if (args.length != 2) {
			throw new IllegalArgumentException("UAI problem and solution output directory must be specified");
		}
		File uaiProblemDirectory  = validateDirectory(args[0]);
		File uaiSolutionDirectory = validateDirectory(args[1]);
		
		ConditionalGenerator iterator = new ConditionalGenerator(new Random(_seed), _numberOfVariables, 0, _depth, _breadth);
		StringJoiner factors = new StringJoiner("\n");
		for (int f = 0; f != _numberFactors; f++) {
			factors.add(""+iterator.next()+";");	
		}
		
		System.out.println("Generating SG to UAI models");
		System.out.println("#variables, #factors, domainsize, time in milliseconds for sg solver to compute solution, time in milliseconds to ground to UAI problem");
		for (int i = 0; i < _domainSizes.length; i++) {			
			StringJoiner model = new StringJoiner("");
			model.add("sort TestDomain : "+_domainSizes[i]);
			for (int c = 0; c < _domainSizes[i]; c++) {				
				model.add(", ");				
				model.add(_constantPrefix+c);
			}
			model.add(";\n");
			
			StringJoiner randoms = new StringJoiner("\n");
			for (int r = 0; r < _numberOfVariables; r++) {
				randoms.add("random X"+r+" : TestDomain;");
			}
			
			model.add(randoms.toString());
			model.add("\n");
			model.add(factors.toString());
			
			HOGMParserWrapper parser          = new HOGMParserWrapper();
			ParsedHOGModel    parsedModel     = parser.parseModel(model.toString());
			FactorsAndTypes   factorsAndTypes = new ExpressionFactorsAndTypes(parsedModel);
System.out.println("model=\n"+model);
System.out.println("f&t  =\n"+factorsAndTypes);
			long startSGInference = System.currentTimeMillis();
			File uaiSolution = new File(uaiSolutionDirectory, "sgproblem_"+_domainSizes[i]+".uai.MAR");	
			InferenceForFactorGraphAndEvidence inferencer = new InferenceForFactorGraphAndEvidence(factorsAndTypes, false, null, true);
			try (
				OutputStream os             = new FileOutputStream(uaiSolution);
				Writer       solutionWriter = new OutputStreamWriter(os, StandardCharsets.UTF_8);	
			) {
				solutionWriter.write("MAR\n");
				solutionWriter.write(""+_numberOfVariables);
				for (int q = 0; q < _numberOfVariables; q++) {
					solutionWriter.write(" "+_domainSizes[i]);
					Expression queryExpr = Expressions.makeSymbol(_variablePrefix+q);
					Expression marginal  = inferencer.solve(queryExpr);
System.out.println("query   ="+queryExpr);
System.out.println("marginal="+marginal);
					for (int c = 0; c <  _domainSizes[i]; c++) {
						solutionWriter.write(" "+UAICompare.roundToUAIOutput(extractValue(queryExpr, marginal)));
					}
				}
				solutionWriter.flush();
			}
			catch (IOException ioe) {
				throw new RuntimeException(ioe);
			}
			long endSGInference = System.currentTimeMillis() - startSGInference;
			
			long startGroundingToUAI = System.currentTimeMillis();
			HOGModelGrounding.ground(factorsAndTypes, new UAIHOGModelGroundingListener(new File(uaiProblemDirectory, "sgproblem_"+_domainSizes[i]+".uai")));
			long endGroundingToUAI = System.currentTimeMillis() - startGroundingToUAI;
			
			System.out.println(""+_numberOfVariables+", "+_numberFactors+", "+_domainSizes[i]+", "+endSGInference+", "+endGroundingToUAI);
		}
	}
	
	private static File validateDirectory(String directoryName) {
		File result = new File(directoryName);
		if (!result.exists()) {
			throw new IllegalArgumentException("UAI output directory does not exist");
		}
		if (!result.isDirectory()) {
			throw new IllegalArgumentException("UAI output directory is not a directory: "+result.getAbsolutePath());
		}
		
		return result;
	}
	
	private static double extractValue(Expression query, Expression marginal) {
		double result = 0;
// TODO - throw exception if can't extract result		
		if (Expressions.isNumber(marginal)) {
			result = marginal.doubleValue();
		}
		else if (IfThenElse.isIfThenElse(marginal)) {
			Expression value = null;
			if (IfThenElse.condition(marginal).equals(query)) {
				value = IfThenElse.thenBranch(marginal);
			}
			
			if (value != null && Expressions.isNumber(value)) {
				result = value.doubleValue();
			}
		}
		
		return result;
	}
}

class ConditionalGenerator extends AbstractRandomDPLLProblemGenerator {
	private Random random;
	public ConditionalGenerator(Random random, int numberOfVariables, int numberOfConstants, int depth, int breadth) {
		super(random, numberOfVariables, numberOfConstants, numberOfVariables, depth, breadth);
		this.random = random;
	}
	
	@Override
	protected Expression makeProblem(Expression formula, List<Expression> indices) {
		
		double thenValue = random.nextDouble();
		double elseValue = 1.0 - thenValue;
		
		Expression problem = IfThenElse.make(formula, Expressions.makeSymbol(thenValue), Expressions.makeSymbol(elseValue));
		return problem;
	}

	
}
