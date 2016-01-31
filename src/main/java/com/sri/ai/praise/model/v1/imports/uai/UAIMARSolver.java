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
package com.sri.ai.praise.model.v1.imports.uai;

import static com.sri.ai.praise.model.v1.imports.uai.UAIUtil.constructGenericTableExpression;
import static com.sri.ai.praise.model.v1.imports.uai.UAIUtil.convertGenericTableToInstance;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.google.common.annotations.Beta;
import com.google.common.util.concurrent.AtomicDouble;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.QuantifierEliminatorWithSetup;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.praise.lang.grounded.common.FunctionTable;
import com.sri.ai.praise.lang.grounded.common.GraphicalNetwork;
import com.sri.ai.praise.sgsolver.solver.FactorsAndTypes;
import com.sri.ai.praise.sgsolver.solver.InferenceForFactorGraphAndEvidence;

/**
 * 
 * @author oreilly
 *
 */
@Beta
public class UAIMARSolver {
	
	private static final boolean DO_NOT_SOLVE = Boolean.getBoolean("uai.mar.solver.do.not.solve");
	
	public static void main(String[] args) throws IOException {
		
		if (args.length != 3) {
			throw new IllegalArgumentException("Must specify UAI model directory or model file to solve, the directory containing the corresponding solutions, and the max time in seconds to solve a problem");
		}
		
		File uaiInput = new File(args[0]);
		if (!uaiInput.exists()) {
			throw new IllegalArgumentException("File or directory specified does not exist: "+uaiInput.getAbsolutePath());
		}
		File solutionDir = new File(args[1]);
		if (!solutionDir.exists() || !solutionDir.isDirectory()) {
			throw new IllegalArgumentException("Solution directory is invalid: "+solutionDir.getAbsolutePath());
		}
		int maxSolverTimeInSeconds = Integer.parseInt(args[2]);
		
		List<UAIModel> models           = new ArrayList<>();
		Map<UAIModel, File> modelToFile = new HashMap<>();
		
		if (uaiInput.isDirectory()) {
			for (File uaiFile : uaiInput.listFiles((dir, name) -> name.endsWith(".uai"))) {
				UAIModel model = read(uaiFile, solutionDir);
				models.add(model);
				modelToFile.put(model, uaiFile);
			}
		}
		else {
			UAIModel model = read(uaiInput, solutionDir);
			models.add(model);
			modelToFile.put(model, uaiInput);
		}
		
		// Sort based on what we consider to be the simplest to hardest
		//Collections.sort(models, (model1, model2) -> Double.compare(model1.ratioUniqueTablesToTables(), model2.ratioUniqueTablesToTables()));
		//Collections.sort(models, (model1, model2) -> Integer.compare(model1.largestNumberOfFunctionTableEntries(), model2.largestNumberOfFunctionTableEntries()));
		Collections.sort(models, (model1, model2) -> Integer.compare(model1.totalNumberEntriesForAllFunctionTables(), model2.totalNumberEntriesForAllFunctionTables()));
		//Collections.sort(models, (model1, model2) -> Integer.compare(model1.numberTables(), model2.numberTables()));
		
		Map<String, Boolean> modelSolvedStatus = new LinkedHashMap<>();
		Map<String, Long>    modelSolvedTime   = new LinkedHashMap<>();
		
		System.out.println("#models read="+models.size());
		final AtomicInteger cnt = new AtomicInteger(1);
		models.stream().forEach(model -> {
			System.out.println("Starting to Solve: "+modelToFile.get(model).getName()+" ("+cnt.getAndAdd(1)+" of "+models.size()+")");
			long start = System.currentTimeMillis();
			boolean solved = solve(model, model.getEvidence(), model.getMARSolution(), maxSolverTimeInSeconds);
			long took = (System.currentTimeMillis() - start);
			System.out.println("---- Took "+took+"ms. solved="+solved);
			
			modelSolvedStatus.put(modelToFile.get(model).getName(), solved);
			modelSolvedTime.put(modelToFile.get(model).getName(), took);
		});
		
		System.out.println("MODELS SOLVE STATUS");
		modelSolvedStatus.entrySet().stream().forEach(e -> System.out.printf("%-25s %-5b %12sms.\n", e.getKey(), e.getValue(), modelSolvedTime.get(e.getKey())));	
		System.out.println("SUMMARY");
		System.out.println("#models   solved="+modelSolvedStatus.values().stream().filter(status -> status == true).count());
		System.out.println("#models unsolved="+modelSolvedStatus.values().stream().filter(status -> status == false).count());
	}
	
	public static boolean solve(GraphicalNetwork model, Map<Integer, Integer> evidence,  Map<Integer, List<Double>> solution, int maxSolverTimeInSeconds) {
		boolean result = false;
		
		ExecutorService executor = Executors.newSingleThreadExecutor();
		SolverTask      solver   = new SolverTask(model, evidence, solution);
		Future<Boolean> future   = executor.submit(solver);  
		
		try {
            System.out.println("Started..");
            result = future.get(maxSolverTimeInSeconds, TimeUnit.SECONDS);
            System.out.println("Finished!");
        }
		catch (TimeoutException toe) {
			System.out.println("Timeout occurred, interrupting solver.");
			solver.interrupt();
			try {
				// Wait until the solver shuts down properly from the interrupt
				System.out.println("Waiting for interrupted result");
				result = future.get();
				System.out.println("Finished waiting for interrupted result");
			}
			catch (Throwable t) {
				System.out.println("Finished waiting for interrupted result : "+(t.getMessage() == null ? t.getClass().getName() : t.getMessage()));
			}
		}
		catch (Throwable t) {
            System.out.println("Terminated! : "+(t.getMessage() == null ? t.getClass().getName() : t.getMessage()));
            t.printStackTrace();
        }

        executor.shutdown();
        System.out.println("executor is shutdown:"+executor.isShutdown());
        
		return result;		
	}
	
	//
	// PRIVATE
	//
	static class SolverTask implements Callable<Boolean> {
		private GraphicalNetwork           model;
		private Map<Integer, Integer>      evidence;
		private Map<Integer, List<Double>> solution;
		//
		private InferenceForFactorGraphAndEvidence inferencer;
		boolean interrupted = false;
		private QuantifierEliminatorWithSetup genericTableSolver = null;
		
		SolverTask(GraphicalNetwork model, Map<Integer, Integer> evidence,  Map<Integer, List<Double>> solution) {
			this.model    = model;
			this.evidence = evidence;
			this.solution = solution;
		}
		
		public QuantifierEliminatorWithSetup checkInterruption(QuantifierEliminatorWithSetup solver) {
			this.genericTableSolver = solver;
			if (interrupted) {
				interrupt();
			}
			return solver;
		}
		
		public void interrupt() {
			interrupted = true;
			
			if (genericTableSolver != null) {
				try {
					genericTableSolver.interrupt();
					System.out.println("Generic Table Compression Solver interrupted (c).");
				}
				catch (Throwable t) {
					System.out.println("Generic Table Compression Solver interrupted (e) : "+(t.getMessage() == null ? t.getClass().getName() : t.getMessage()));
				}
			}
			
			if (inferencer != null) {
				try {
					inferencer.interrupt();
					System.out.println("Solver interrupted (c).");
				}
				catch (Throwable t) {
					System.out.println("Solver interrupted (e) : "+(t.getMessage() == null ? t.getClass().getName() : t.getMessage()));					
				}
			}	
		}
		
		@Override
		public Boolean call() throws Exception {
			System.out.println("#variables="+model.numberVariables());
			System.out.println("#tables="+model.numberTables());
			System.out.println("#unique function tables="+model.numberUniqueFunctionTables());
			System.out.println("Largest variable cardinality="+model.largestCardinality());
			System.out.println("Largest # entries="+model.largestNumberOfFunctionTableEntries());
			System.out.println("Total #entries across all function tables="+model.totalNumberEntriesForAllFunctionTables());

			double totalNumberUniqueEntries        = 0;
			double totalCompressedEntries          = 0;
			double bestIndividualCompressionRatio  = 100; // i.e. none at all
			double worstIndividualCompressionRatio = 0;
			List<Expression> tables = new ArrayList<>();
			for (int i = 0; i < model.numberUniqueFunctionTables(); i++) {
				FunctionTable table = model.getUniqueFunctionTable(i);
				
				totalNumberUniqueEntries += table.numberEntries();
				
				if (interrupted) {
					System.out.println("Solver Interrupted (t).");
					return false;
				}
				
				Expression genericTableExpression = constructGenericTableExpression(table, this::checkInterruption);
				
				double compressedEntries = calculateCompressedEntries(genericTableExpression);
				
				double compressedRatio = compressedEntries / table.numberEntries();
				if (compressedRatio < bestIndividualCompressionRatio) {
					bestIndividualCompressionRatio = compressedRatio;
				}
				if (compressedRatio > worstIndividualCompressionRatio) {
					worstIndividualCompressionRatio = compressedRatio;
				}
				
				totalCompressedEntries += compressedEntries;
				
				for (int tableIdx : model.getTableIndexes(i)) {
					Expression instanceTableExpression = convertGenericTableToInstance(table, genericTableExpression, model.getVariableIndexesForTable(tableIdx));
					tables.add(instanceTableExpression);
				}
			}
			
			System.out.println("Table compression ratio            = " + (totalCompressedEntries/totalNumberUniqueEntries));
			System.out.println("Best individual compression ratio  = " + bestIndividualCompressionRatio);
			System.out.println("Worst individual compression ratio = " + worstIndividualCompressionRatio);
			
			// If Solving not to actually be performed (i.e. just getting a summary of the models) then 
			// indicate failed to solve
			if (DO_NOT_SOLVE) {
				return false;
			}
			
			FactorsAndTypes factorsAndTypes = new UAIFactorsAndTypes(tables, model);

			Expression evidenceExpr = null; 
			List<Expression> conjuncts = new ArrayList<Expression>();
			for (Map.Entry<Integer, Integer> entry : evidence.entrySet()) {
				int varIdx = entry.getKey();
				int valIdx = entry.getValue();
				Expression varExpr   = Expressions.makeSymbol(UAIUtil.instanceVariableName(varIdx));
				Expression valueExpr = Expressions.makeSymbol(UAIUtil.instanceConstantValueForVariable(valIdx, varIdx, model.cardinality(varIdx)));
				if (valueExpr.equals(Expressions.TRUE)) {
					conjuncts.add(varExpr);
				}
				else if (valueExpr.equals(Expressions.FALSE)) {
					conjuncts.add(Not.make(varExpr));
				}
				else {
					conjuncts.add(Equality.make(varExpr, valueExpr));
				}
			}
			if (conjuncts.size() > 0) {
				evidenceExpr = And.make(conjuncts);
			}
			
			//System.out.println("mapFromCategoricalTypeNameToSizeString="+mapFromCategoricalTypeNameToSizeString);
			//System.out.println("mapFromVariableNameToTypeName="+mapFromVariableNameToTypeName);
			//System.out.println("Markov Network=\n"+markovNetwork);
			
			if (interrupted) {
				System.out.println("Solver Interrupted (b).");
				return false;
			}
			
			inferencer = new InferenceForFactorGraphAndEvidence(factorsAndTypes, false, evidenceExpr, true, null);
			
			Map<Integer, List<Double>> computed = new LinkedHashMap<>();
			for (int i = 0; i < model.numberVariables(); i++) {
				int varCardinality = model.cardinality(i);
				List<Integer> remainingQueryValueIdxs = IntStream.range(0, varCardinality).boxed().collect(Collectors.toList());
				double[] values = new double[varCardinality];
				while (remainingQueryValueIdxs.size() > 0) {
					int queryValueIdx = remainingQueryValueIdxs.get(0);
					Expression varExpr   = Expressions.makeSymbol(UAIUtil.instanceVariableName(i));
					Expression valueExpr = Expressions.makeSymbol(UAIUtil.instanceConstantValueForVariable(queryValueIdx, i, varCardinality));
					Expression queryExpression = Equality.make(varExpr, valueExpr);	
					Expression marginal;
					if (interrupted) {
						System.out.println("Solver Interrupted (l).");
						return false;
					}
					marginal = inferencer.solve(queryExpression);
					
					if (evidenceExpr == null) {
						System.out.println("Query marginal probability P(" + queryExpression + ") is: " + marginal);
					}
					else {
						System.out.println("Query posterior probability P(" + queryExpression + " | " + evidenceExpr + ") is: " + marginal);
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
			
			List<Integer> diffs = UAICompare.compareMAR(solution, computed);
			System.out.println("----");
			boolean result = true;
			if (diffs.size() == 0) {
				System.out.println("Computed values match solution: "+computed);
			}
			else {
				result = false; // Failed to solve correctly
				System.err.println("These variables "+diffs+" did not match the solution.");
				System.err.println("solution="+solution);
				System.err.println("computed="+computed);
			}
			
			return result;
		}
	}
	
	private static UAIModel read(File uaiFile, File solutionDir) throws IOException {
		UAIModel model = UAIModelReader.read(uaiFile);
		
		UAIEvidenceReader.read(uaiFile, model);
		
		// Result is specified in a separate file. This file has the same name as the original network 
		// file but with an added .MAR suffix. For instance, problem.uai will have a MAR result file problem.uai.MAR. 
		File marResultFile = new File(solutionDir, uaiFile.getName()+".MAR");
		Map<Integer, List<Double>> marResult = UAIResultReader.readMAR(marResultFile);
		if (marResult.size() != model.numberVariables()) {
			throw new IllegalArgumentException("Number of variables in result file, "+marResult.size()+", does not match # in model, which is "+model.numberVariables());
		}
		for (Map.Entry<Integer, List<Double>> entry : marResult.entrySet()) {
			model.addMARSolution(entry.getKey(), entry.getValue());
		}
		
		return model;
	}
	
	private static double calculateCompressedEntries(Expression compressedTableExpression) {
		AtomicDouble count = new AtomicDouble(0);
		
		visitCompressedTableEntries(compressedTableExpression, count);
		
		return count.doubleValue();
	}
	
	private static void visitCompressedTableEntries(Expression compressedTableExpression, AtomicDouble count) {
		if (IfThenElse.isIfThenElse(compressedTableExpression)) {
			visitCompressedTableEntries(IfThenElse.thenBranch(compressedTableExpression), count);
			visitCompressedTableEntries(IfThenElse.elseBranch(compressedTableExpression), count);
		}
		else {
			// We are at a leaf node, therefore increment the count
			count.addAndGet(1);
		}
	}
	
	private static void assignComputedValues(Expression varExpr, Expression marginal, Map<Expression, Integer> possibleValueExprToIndex, List<Integer> remainingQueryValueIdxs, double[] values) {
		boolean leafValue = true;
		if (IfThenElse.isIfThenElse(marginal)) {
			leafValue = false;
			Expression condExpr = IfThenElse.condition(marginal);
			int valueIdx = identifyValueIdx(varExpr, condExpr, possibleValueExprToIndex);
			Expression thenExpr = IfThenElse.thenBranch(marginal);
			for (Map.Entry<Expression, Integer> entry : possibleValueExprToIndex.entrySet()) {
				if (entry.getValue() == valueIdx) {
					possibleValueExprToIndex.remove(entry.getKey());
					break;
				}
			}
			remainingQueryValueIdxs.remove(remainingQueryValueIdxs.indexOf(valueIdx));
			values[valueIdx] = thenExpr.rationalValue().doubleValue();
			Expression elseExpr = IfThenElse.elseBranch(marginal);
			assignComputedValues(varExpr, elseExpr, possibleValueExprToIndex, remainingQueryValueIdxs, values);
		}
		else if (Expressions.hasFunctor(marginal, FunctorConstants.DIVISION)) {
			marginal = Division.simplify(marginal);
		}
		
		
		if (leafValue) {
			if (possibleValueExprToIndex.size() != 1) {
				throw new IllegalStateException("Unable to identify what value index to assing the marginal : "+marginal+" to "+possibleValueExprToIndex);
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
