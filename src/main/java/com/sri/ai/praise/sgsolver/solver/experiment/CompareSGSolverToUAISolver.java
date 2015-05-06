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
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.AbstractRandomDPLLProblemGenerator;
import com.sri.ai.praise.sgsolver.model.export.UAIHOGModelGroundingListener;
import com.sri.ai.praise.sgsolver.model.grounded.model.HOGModelGrounding;
import com.sri.ai.praise.sgsolver.model.imports.uai.UAICompare;
import com.sri.ai.praise.sgsolver.solver.ExpressionFactorsAndTypes;
import com.sri.ai.praise.sgsolver.solver.FactorsAndTypes;
import com.sri.ai.praise.sgsolver.solver.InferenceForFactorGraphAndEvidence;

@Beta
public class CompareSGSolverToUAISolver {

	static long    _seed   = 3;
	static boolean _ground = false;
	//
	static int [] _domainSizes = new int[] {2, 4, 8, 16, 32, 64, 128, 254, 1000, 10000, 100000, 1000000, 10000000};
	//
	static final int _factorIdx   = 0;
	static final int _variableIdx = 1;
	static final int _constantIdx = 2;
	static final int _depthIdx    = 3;
	static final int _breadthIdx  = 4;
	static int [][] _params = new int[][] {
// TODO - add support for extracting marginal values when > 0 constants		
		// #factors, #variables, #constants, _depth, _breadth
		{         5,         10,          0,      2,        2},
		{         5,         10,          0,      2,        4},
		{         5,         10,          0,      2,        8},
		{         5,         10,          0,      2,       16},
		{         5,         10,          0,      4,        4},
		{         5,         10,          0,      4,        8},
		{         5,         10,          0,      4,       16},
		{         5,         10,          2,      2,        2},
		{         5,         10,          4,      4,        4},
		{         5,         10,          8,      4,        8},
		{         5,         10,         16,      4,       16},
	};
	//
	static final String _variablePrefix = "X";
	static final String _constantPrefix = "a";
	
	public static void main(String[] args) {
		if (args.length != 2) {
			throw new IllegalArgumentException("UAI problem and solution output directory must be specified");
		}
		File uaiProblemDirectory  = validateDirectory(args[0]);
		File uaiSolutionDirectory = validateDirectory(args[1]);

		System.out.println("Generating SG to UAI models");
		System.out.println("domainsize, #factors, #variables, #constants, expression depth, expression breadth, time in milliseconds for sg solver to compute solution, time in milliseconds to ground to UAI problem");

		for (int p = 0; p < _params.length; p++) {
			int numberOfFactors   = _params[p][_factorIdx];
			int numberOfVariables = _params[p][_variableIdx];			
			int depth             = _params[p][_depthIdx];
			int breadth           = _params[p][_breadthIdx];
			
			for (int i = 0; i < _domainSizes.length; i++) {
				int cardinality = _domainSizes[i];
				
				int numberOfConstants = _params[p][_constantIdx];
				// #constants must be <= domain size
				if (numberOfConstants > cardinality) {
					numberOfConstants = cardinality;
				}
				
				String uaiFileSuffix = "_"+cardinality+"_f"+numberOfFactors+"_v"+numberOfVariables+"_c"+numberOfConstants+"_d"+depth+"_b"+breadth;
				
				List<Expression>    factors                                       = new ArrayList<>();
				Map<String, String> mapFromRandomVariableNameToTypeName           = new LinkedHashMap<>();
				Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName = new LinkedHashMap<>();
				Map<String, String> mapFromUniquelyNamedConstantNameToTypeName    = new LinkedHashMap<>();
				Map<String, String> mapFromTypeNameToSizeString                   = new LinkedHashMap<>();
				
				ConditionalGenerator iterator = new ConditionalGenerator(new Random(_seed), numberOfVariables, numberOfConstants, depth, breadth);
				for (int f = 0; f != numberOfFactors; f++) {
					Expression condition = iterator.next();
					// Ensure we have variables in the condition
					if (Expressions.freeVariables(condition, new DefaultRewritingProcess(null)).size() == 0) {							
						condition = iterator.next();
					}
					factors.add(condition);	
				}
				
				for (int r = 0; r < numberOfVariables; r++) {
					mapFromRandomVariableNameToTypeName.put(_variablePrefix+r, "TestDomain");
				}
				
				for (int c = 0; c < numberOfConstants; c++) {	
					mapFromUniquelyNamedConstantNameToTypeName.put(_constantPrefix+c, "TestDomain");
				}
				mapFromUniquelyNamedConstantNameToTypeName.put("false", "Boolean");
				mapFromUniquelyNamedConstantNameToTypeName.put("true",  "Boolean");
				
				mapFromTypeNameToSizeString.put("Boolean",    "2");
				mapFromTypeNameToSizeString.put("TestDomain", ""+cardinality);				
				
				FactorsAndTypes   factorsAndTypes = new ExpressionFactorsAndTypes(factors,
						mapFromRandomVariableNameToTypeName,
						mapFromNonUniquelyNamedConstantNameToTypeName,
						mapFromUniquelyNamedConstantNameToTypeName,
						mapFromTypeNameToSizeString);
	
//System.out.println("f&t  =\n"+factorsAndTypes);
	
				long startSGInference = System.currentTimeMillis();
				File uaiSolution = new File(uaiSolutionDirectory, "sgproblem_"+uaiFileSuffix+".uai.MAR");	
				InferenceForFactorGraphAndEvidence inferencer = new InferenceForFactorGraphAndEvidence(factorsAndTypes, false, null, true);
				try (
					OutputStream os             = new FileOutputStream(uaiSolution);
					Writer       solutionWriter = new OutputStreamWriter(os, StandardCharsets.UTF_8);	
				) {
					if (_ground) {
						solutionWriter.write("MAR\n");
						solutionWriter.write(""+numberOfVariables);
					}
					for (int q = 0; q < numberOfVariables; q++) {
						Expression queryExpr = Expressions.makeSymbol(_variablePrefix+q);
						Expression marginal  = inferencer.solve(queryExpr);
//System.out.println("query   ="+queryExpr);
//System.out.println("marginal="+marginal);
						if (_ground) {
							solutionWriter.write(" "+cardinality);					
							for (int c = 0; c < cardinality; c++) {
								solutionWriter.write(" "+UAICompare.roundToUAIOutput(extractValue(queryExpr, marginal)));
							}
						}
					}
					if (_ground) {
						solutionWriter.flush();
					}
				}
				catch (IOException ioe) {
					throw new RuntimeException(ioe);
				}
				long endSGInference = System.currentTimeMillis() - startSGInference;
				
				long startGroundingToUAI = System.currentTimeMillis();
				if (_ground) {
					HOGModelGrounding.ground(factorsAndTypes, new UAIHOGModelGroundingListener(new File(uaiProblemDirectory, "sgproblem_"+uaiFileSuffix+".uai")));
				}
				long endGroundingToUAI = System.currentTimeMillis() - startGroundingToUAI;
				
				System.out.println(""+cardinality+", "+numberOfFactors+", "+numberOfVariables+", "+numberOfConstants+", "+depth+", "+breadth+", "+endSGInference+", "+endGroundingToUAI);
			}
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
		Double result = null;	
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
		
		if (result == null) {
			throw new IllegalArgumentException("Unable to extract result from marginal: "+marginal);
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
