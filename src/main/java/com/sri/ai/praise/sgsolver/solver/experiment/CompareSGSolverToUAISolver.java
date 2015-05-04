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
import com.sri.ai.praise.sgsolver.solver.ExpressionFactorsAndTypes;
import com.sri.ai.praise.sgsolver.solver.FactorsAndTypes;
import com.sri.ai.praise.sgsolver.solver.InferenceForFactorGraphAndEvidence;

@Beta
public class CompareSGSolverToUAISolver {

	static int [] _domainSizes       = new int[] {2, 4};// 8, 16, 32, 64, 128, 254, 1000, 10000};
	static int    _numberFactors     = 5;
	static int    _numberOfVariables = 2;
	static long   _seed              = 3;
	
	public static void main(String[] args) {
		ConditionalGenerator iterator = new ConditionalGenerator(new Random(_seed), _numberOfVariables, 2, 2);
		
		StringJoiner factors = new StringJoiner("\n");
		for (int i = 0; i != _numberFactors; i++) {
			factors.add(""+iterator.next()+";");	
		}
		StringJoiner randoms = new StringJoiner("\n");
		for (int i = 0; i < _numberOfVariables; i++) {
			randoms.add("random X"+i+" : TestDomain;");
		}
		
		for (int i = 0; i < _domainSizes.length; i++) {
			StringJoiner model = new StringJoiner("\n");
			model.add("sort TestDomain : "+_domainSizes[i]+";");
			model.add(randoms.toString());
			model.add(factors.toString());
			
			HOGMParserWrapper parser          = new HOGMParserWrapper();
			ParsedHOGModel    parsedModel     = parser.parseModel(model.toString());
			FactorsAndTypes   factorsAndTypes = new ExpressionFactorsAndTypes(parsedModel);
System.out.println("model="+model);
System.out.println("factors and types="+factorsAndTypes);
			InferenceForFactorGraphAndEvidence inferencer = new InferenceForFactorGraphAndEvidence(factorsAndTypes, false, null, true);
			for (int q = 0; q < _numberOfVariables; q++) {
				Expression queryExpr = Expressions.makeSymbol("X"+q);
				Expression marginal = inferencer.solve(queryExpr);
System.out.println("marginal="+marginal);
			}
		}
	}
}

class ConditionalGenerator extends AbstractRandomDPLLProblemGenerator {
	private Random random;
	public ConditionalGenerator(Random random, int numberOfVariables, int depth, int breadth) {
		super(random, numberOfVariables, 0, numberOfVariables, depth, breadth);
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
