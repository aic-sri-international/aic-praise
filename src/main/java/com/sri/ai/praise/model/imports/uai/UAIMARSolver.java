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
package com.sri.ai.praise.model.imports.uai;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.ProbabilisticInference;
import com.sri.ai.grinder.library.number.Times;

import static com.sri.ai.praise.model.imports.uai.UAIUtil.constructGenericTableExpression;
import static com.sri.ai.praise.model.imports.uai.UAIUtil.convertGenericTableToInstance;

/**
 * 
 * @author oreilly
 *
 */
@Beta
public class UAIMARSolver {
	public static void main(String[] args) throws IOException {
		
		if (args.length != 2) {
			throw new IllegalArgumentException("Must specify UAI model directory or model file to solve and the directory containing the corresponding solutions");
		}
		
		File uaiInput = new File(args[0]);
		if (!uaiInput.exists()) {
			throw new IllegalArgumentException("File or directory specified does not exist: "+uaiInput.getAbsolutePath());
		}
		File solutionDir = new File(args[1]);
		if (!solutionDir.exists() || !solutionDir.isDirectory()) {
			throw new IllegalArgumentException("Solution directory is invalid: "+solutionDir.getAbsolutePath());
		}
		
		List<UAIModel> models = new ArrayList<>();
		
		if (uaiInput.isDirectory()) {
			for (File uaiFile : uaiInput.listFiles((dir, name) -> name.endsWith(".uai"))) {
				models.add(read(uaiFile, solutionDir));
			}
		}
		else {
			models.add(read(uaiInput, solutionDir));
		}
		
		// Sort based on what we consider to be the simplest to hardest
		//Collections.sort(models, (model1, model2) -> Double.compare(model1.ratioUniqueFunctionTableToCliques(), model2.ratioUniqueFunctionTableToCliques()));
		//Collections.sort(models, (model1, model2) -> Integer.compare(model1.largestNumberOfFunctionTableEntries(), model2.largestNumberOfFunctionTableEntries()));
		Collections.sort(models, (model1, model2) -> Integer.compare(model1.totalNumberEntriesForAllFunctionTables(), model2.totalNumberEntriesForAllFunctionTables()));
		//Collections.sort(models, (model1, model2) -> Integer.compare(model1.numberCliques(), model2.numberCliques()));
		
		System.out.println("#models read="+models.size());
		final AtomicInteger cnt = new AtomicInteger(1);
		models.stream().forEach(model -> {
			System.out.println("Starting to Solve: "+model.getFile().getName()+" ("+cnt.getAndAdd(1)+" of "+models.size()+")");
			long start = System.currentTimeMillis();
			solve(model);
			System.out.println("---- Took "+(System.currentTimeMillis() - start)+"ms.");
		});
	}
	
	public static void solve(UAIModel model) {
		System.out.println("#variables="+model.numberVars());
		System.out.println("#cliques="+model.numberCliques());
		System.out.println("#unique function tables="+model.numberUniqueFunctionTables());
		System.out.println("Largest variable cardinality="+model.largestCardinality());
		System.out.println("Largest # entries="+model.largestNumberOfFunctionTableEntries());
		System.out.println("Total #entries across all function tables="+model.totalNumberEntriesForAllFunctionTables());
	
//// TODO - remove		
//if (true) {
//	return;
//}
		List<Expression> factors = new ArrayList<Expression>();
		for (Map.Entry<FunctionTable, List<Integer>> tableToCliques : model.getTableToCliques()) {	
			Expression genericTableExpression  = constructGenericTableExpression(tableToCliques.getKey());	
			for (int cliqueIdx : tableToCliques.getValue()) {			
				Expression instanceTableExpression = convertGenericTableToInstance(tableToCliques.getKey(), genericTableExpression, model.getVariableIdxsForClique(cliqueIdx));
				factors.add(instanceTableExpression);
			}
		}
		
		Map<String, String> mapFromTypeNameToSizeString   = new LinkedHashMap<>();
		Map<String, String> mapFromVariableNameToTypeName = new LinkedHashMap<>();
		for (int i = 0; i < model.numberVars(); i++) {
			int varCardinality = model.cardinality(i);
			String varTypeName = UAIUtil.instanceTypeNameForVariable(i, varCardinality);
			mapFromTypeNameToSizeString.put(varTypeName, Integer.toString(varCardinality));
			mapFromVariableNameToTypeName.put(UAIUtil.instanceVariableName(i), varTypeName);
		}
		
		Expression markovNetwork = Times.make(factors);

		Expression evidence = null; 
		List<Expression> conjuncts = new ArrayList<Expression>();
		for (Map.Entry<Integer, Integer> entry : model.getEvidence()) {
			int varIdx = entry.getKey();
			int valIdx = entry.getValue();
			Expression varExpr   = Expressions.makeSymbol(UAIUtil.instanceVariableName(varIdx));
			Expression valueExpr = Expressions.makeSymbol(UAIUtil.instanceConstantValueForVariable(valIdx, varIdx, model.cardinality(varIdx)));
			conjuncts.add(Equality.make(varExpr, valueExpr));
		}
		if (conjuncts.size() > 0) {
			evidence = And.make(conjuncts);
		}
		System.out.println("mapFromTypeNameToSizeString="+mapFromTypeNameToSizeString);
		System.out.println("mapFromVariableNameToTypeName="+mapFromVariableNameToTypeName);
		System.out.println("Markov Network=\n"+markovNetwork);
		
		Map<Integer, List<Double>> computed = new LinkedHashMap<>();
		for (int i = 0; i < model.numberVars(); i++) {
			int varCardinality = model.cardinality(i);
			List<Integer> remainingQueryValueIdxs = IntStream.range(0, varCardinality).boxed().collect(Collectors.toList());
			double[] values = new double[varCardinality];
			while (remainingQueryValueIdxs.size() > 0) {
				int queryValueIdx = remainingQueryValueIdxs.get(0);
				Expression varExpr   = Expressions.makeSymbol(UAIUtil.instanceVariableName(i));
				Expression valueExpr = Expressions.makeSymbol(UAIUtil.instanceConstantValueForVariable(queryValueIdx, i, varCardinality));
				Expression queryExpression = Equality.make(varExpr, valueExpr);	
				Expression marginal = ProbabilisticInference.solveFactorGraph(markovNetwork, false, queryExpression, evidence, mapFromTypeNameToSizeString, mapFromVariableNameToTypeName);
				
				if (evidence == null) {
					System.out.println("Query marginal probability P(" + queryExpression + ") is: " + marginal);
				}
				else {
					System.out.println("Query posterior probability P(" + queryExpression + " | " + evidence + ") is: " + marginal);
				}
				
				Map<Expression, Integer> possibleValueExprToIndex = new LinkedHashMap<>();
				possibleValueExprToIndex.put(valueExpr, queryValueIdx);
				if (IfThenElse.isIfThenElse(marginal)) {
					for (Integer c : remainingQueryValueIdxs) {
						possibleValueExprToIndex.put(Expressions.makeSymbol(UAIUtil.instanceConstantValueForVariable(c, i, varCardinality)), c);
					}
				}
				
				assignComputedValues(varExpr, marginal, possibleValueExprToIndex, remainingQueryValueIdxs, values);
			}
			computed.put(i, Arrays.stream(values).boxed().collect(Collectors.toList()));
		}
		
		List<Integer> diffs = UAICompare.compareMAR(model.getMARSolution(), computed);
		System.out.println("----");
		if (diffs.size() == 0) {
			System.out.println("Computed values match solution: "+computed);
		}
		else {
			System.err.println("These variables "+diffs+" did not match the solution.");
			System.err.println("solution="+model.getMARSolution());
			System.err.println("computed="+computed);
		}
	}
	
	//
	// PRIVATE
	//
	private static UAIModel read(File uaiFile, File solutionDir) throws IOException {
		UAIModel model = UAIModelReader.read(uaiFile);
		
		UAIEvidenceReader.read(model);
		
		// Result is specified in a separate file. This file has the same name as the original network 
		// file but with an added .MAR suffix. For instance, problem.uai will have a MAR result file problem.uai.MAR. 
		File marResultFile = new File(solutionDir, uaiFile.getName()+".MAR");
		Map<Integer, List<Double>> marResult = UAIResultReader.readMAR(marResultFile);
		if (marResult.size() != model.numberVars()) {
			throw new IllegalArgumentException("Number of variables in result file, "+marResult.size()+", does not match # in model, which is "+model.numberVars());
		}
		for (Map.Entry<Integer, List<Double>> entry : marResult.entrySet()) {
			model.addMARSolution(entry.getKey(), entry.getValue());
		}
		
		return model;
	}
	
	private static void assignComputedValues(Expression varExpr, Expression marginal, Map<Expression, Integer> possibleValueExprToIndex, List<Integer> remainingQueryValueIdxs, double[] values) {
		
		if (IfThenElse.isIfThenElse(marginal)) {
			Expression condExpr = IfThenElse.getCondition(marginal);
			int valueIdx = identifyValueIdx(varExpr, condExpr, possibleValueExprToIndex);
			Expression thenExpr = IfThenElse.getThenBranch(marginal);
			for (Map.Entry<Expression, Integer> entry : possibleValueExprToIndex.entrySet()) {
				if (entry.getValue() == valueIdx) {
					possibleValueExprToIndex.remove(entry.getKey());
					break;
				}
			}
			remainingQueryValueIdxs.remove(remainingQueryValueIdxs.indexOf(valueIdx));
			values[valueIdx] = thenExpr.rationalValue().doubleValue();
			Expression elseExpr = IfThenElse.getElseBranch(marginal);
			assignComputedValues(varExpr, elseExpr, possibleValueExprToIndex, remainingQueryValueIdxs, values);
		}
		else {
			if (possibleValueExprToIndex.size() != 1) {
				throw new IllegalStateException("Unable to identify what value index to assing the marginal too: "+marginal+" to "+possibleValueExprToIndex);
			}
			int valueIdx = possibleValueExprToIndex.values().iterator().next();
			possibleValueExprToIndex.clear();
			remainingQueryValueIdxs.remove(remainingQueryValueIdxs.indexOf(valueIdx));
			values[valueIdx] = marginal.rationalValue().doubleValue();
		}
	}
	
	private static int identifyValueIdx(Expression varExpr, Expression condExpr,  Map<Expression, Integer> possibleValueExprToIndex) {
		int result = -1;
		
		if (!Equality.isEquality(condExpr)) {
			throw new IllegalStateException("Currently unable to handle non equalities :"+condExpr);
		}
	
		for (int i = 0; i < 2; i++) {
			Integer v = possibleValueExprToIndex.get(condExpr.get(i));
			if (v != null) {
				result = v;
				break;
			}
		}
		
		if (result == -1) {
			throw new IllegalStateException("Unable to identify value idex for "+varExpr+" "+condExpr+" "+possibleValueExprToIndex);
		}
				
		return result;
	}
}
