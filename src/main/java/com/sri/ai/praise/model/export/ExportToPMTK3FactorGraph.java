/*
 * Copyright (c) 2013, SRI International
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

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Parser;
import com.sri.ai.expresso.core.AbstractReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderParserWrapper;
import com.sri.ai.praise.BracketedExpressionSubExpressionsProvider;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.PRAiSEConfiguration;
import com.sri.ai.praise.lbp.LBPConfiguration;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.ModelGrounding;
import com.sri.ai.praise.model.ModelGrounding.GroundedModelResult;
import com.sri.ai.praise.model.RandomVariableDeclaration;
import com.sri.ai.praise.model.SortDeclaration;
import com.sri.ai.util.Configuration;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.CartesianProductEnumeration;
import com.sri.ai.util.math.MixedRadixNumber;
import com.sri.ai.util.math.Rational;

/**
 * Exports an LBP Model, which is grounded first, to <a
 * href="http://code.google.com/p/pmtk3/">PMTK3's</a> factor graph
 * representation.
 * 
 * @author oreilly
 * 
 */
@Beta
public class ExportToPMTK3FactorGraph {
	
	private static final String _indent    = "    ";
	private static final int    _precision = 6; 
	
	public static void main(String[] args) throws Exception {
		Parser  parser  = new AntlrGrinderParserWrapper();
		
		boolean outputLBPBeliefs = false;
		Integer size             = 12;
		
		String modelDeclaration =  Model.getModelDeclarationFromResource("Example4.model");

		Map<Expression, Expression> globalObjects = new LinkedHashMap<Expression, Expression>();
		globalObjects.put(parser.parse("| Object |"), Expressions.makeSymbol(size)); 
		// Ensure type sizes match up.
		Configuration.setProperty(PRAiSEConfiguration.KEY_MODEL_DEFAULT_SIZE_OF_ALL_TYPES, size.toString());
		// Increase past the default of 10,000
		Configuration.setProperty(PRAiSEConfiguration.KEY_MAX_ALLOWED_SIZE_FOR_GROUNDED_MODEL, new Integer(50000).toString());
		
		Expression modelDefinition = parser.parse(modelDeclaration);
	
		Set<String> emptySet = Collections.emptySet();
		Model modelToExport = new Model(modelDefinition, emptySet);
		
		RewritingProcess process = LBPFactory.newLBPProcess(modelDefinition);
		process.getGlobalObjects().putAll(globalObjects);
		
		process = Model.setRewritingProcessesModel(modelDefinition, modelToExport.getKnownRandomVariableNameAndArities(), process);
		
		GroundedModelResult groundedModelResult = null;
		try {
			groundedModelResult = ModelGrounding.groundModel(process);
		
			StringWriter stringWriter = new StringWriter();
			export(groundedModelResult, stringWriter, outputLBPBeliefs, process);
		
			System.out.print(stringWriter.toString());
		} catch (ModelGrounding.ModelGroundingException mge) {
			System.err.println("Model Ground Exception: "+mge.getMessage());
			for (ModelGrounding.ModelGroundingError error : mge.getErrors()) {
				System.err.println(error.getErrorType());
				System.err.println(error.getInExpression());
			}
		}
	}
	
	public static void export(ModelGrounding.GroundedModelResult groundedModelResult, Writer output, boolean outputLBPBeliefs, RewritingProcess process) 
		throws IOException {
		
		Model originalModel = groundedModelResult.getGroundedFrom();
		Model groundedModel = groundedModelResult.getGroundedModel();
		Map<Expression, Integer> randomVariableToPmtk3Id = new LinkedHashMap<Expression, Integer>();
		Map<Integer, Expression> pmtk3IdToRandomVariable = new LinkedHashMap<Integer, Expression>();
		
		// output the header comments for the file, detailing
		// the lpi model that has been grounded and translated
		// to a pmtk3 factor graph format.
		outputHeader(originalModel, output);
		
		// As pmtk3 does not support multiple factors on the same types, want to merge 
		// factors (by multiplying their potentials together) over the same types.
		// First we collect up all of the tabular factors.
		Map<List<Integer>, List<TabularFactor>> repeatedTypeFactors = new LinkedHashMap<List<Integer>, List<TabularFactor>>();
		// Now construct the tabular factors for the grounded model
		for (Pair<Expression, List<Expression>> parfactorToGroundFactors : groundedModelResult.getParfactorToGroundFactors()) {
			List<Expression> factors   = parfactorToGroundFactors.second;
			
			// Irrespective of whether or not the parfactor is intensionally or extensionally defined
			// we will create an explicit tabular factor for each as ordering of variables may differ.
			for (int i = 0; i < factors.size(); i++) {
				Expression factorValue = LPIUtil.getFactorValueExpression(factors.get(i), process);
				TabularFactor tabularFactor = new TabularFactor(groundedModel, factorValue, randomVariableToPmtk3Id, pmtk3IdToRandomVariable, process);
				List<TabularFactor> factorsOnSameDomains = repeatedTypeFactors.get(tabularFactor.types);
				if (factorsOnSameDomains == null) {
					factorsOnSameDomains = new ArrayList<TabularFactor>();
					repeatedTypeFactors.put(tabularFactor.types, factorsOnSameDomains);
				}
				factorsOnSameDomains.add(tabularFactor);
			}
		}
		output.write("%\n");
		output.write("% # of ground factors (Note can be merged) = "+ExtensionalSet.cardinality(groundedModelResult.getGroundedModel().getParfactorsDeclaration().getParfactors().get(0)));
		output.write("\n");
		// output LBP's beliefs for each random variable
		// in order to simplify comparisons.
		if (outputLBPBeliefs) {
			outputLBPBeliefs(originalModel, output, process);
		}
		// for convenience output all the type id to random variable ids
		output.write("%\n");
		for (Map.Entry<Integer, Expression> rvEntry : pmtk3IdToRandomVariable.entrySet()) {
			output.write("% ");
			output.write(rvEntry.getKey().toString());
			output.write(" = ");
			output.write(rvEntry.getValue().toString());
			output.write("\n");
		}
		
		// output the main mk...() function for the Matlab file
		outputMainFunction(groundedModel, randomVariableToPmtk3Id, output);
		
		// Now we want to create default node potentials (i.e. just 1 type in the tabular factor)
		// when translating to the pmtk3 format (otherwise if not specified these get defaulted
		// to 0 potentials, which is not what is wanted).
		Set<Integer> nodePotentials = new LinkedHashSet<Integer>(randomVariableToPmtk3Id.values());
		for (List<Integer> types : repeatedTypeFactors.keySet()) {
			if (types.size() == 1) {
				// I've already got a node potential for this
				nodePotentials.remove(types.get(0));
			}
		}
		// At this point I have nodes without an explicit node potential
		for (Integer typeId : nodePotentials) {
			TabularFactor tabularFactor = new TabularFactor(pmtk3IdToRandomVariable.get(typeId), typeId, process);
			List<TabularFactor>  factorsOnSameDomains = new ArrayList<TabularFactor>();
			repeatedTypeFactors.put(tabularFactor.types, factorsOnSameDomains);
			factorsOnSameDomains.add(tabularFactor);
		}
		
		int numberPmtk3Factors = repeatedTypeFactors.size();
		output.write("\n");
		output.write("function [cliques nstates] = mk");
		output.write(groundedModel.getName().toString());
		output.write("CliquesAndStates()\n\n");
		output.write(_indent);
		output.write("cliques = cell("+numberPmtk3Factors+", 1);\n");
		
		int cidx = 1;
		for (Map.Entry<List<Integer>, List<TabularFactor>> typeToFactors : repeatedTypeFactors.entrySet()) {
			TabularFactor pmtk3TabularFactor = new TabularFactor(typeToFactors.getValue());
			
			output.write("\n");
			int noMerged = typeToFactors.getValue().size();
			if (noMerged > 1) {
				output.write(_indent);
				output.write("% Merged "+noMerged+" ground factors:");
				output.write("\n");
			}
			// output the ground factors merged into this factor
			for (TabularFactor factor : typeToFactors.getValue()) {
				output.write(_indent);
				output.write("% ");
				output.write(factor.factorValue.toString());
				output.write("\n");
			}
			// output the random variable to type id map for this factor
			for (Integer typeId : typeToFactors.getKey()) {
				output.write(_indent);
				output.write("% ");
				output.write(typeId.toString());
				output.write(" = ");
				output.write(pmtk3IdToRandomVariable.get(typeId).toString());
				output.write("\n");
			}
			// output the pmtk3 merged tabular factor
			String tfId = outputTabularFactorCreate(pmtk3TabularFactor, output);
			// output the pmtk3 create statement
			// e.g. cliques{1} = tabularFactorCreate(T, [1 7]);
			output.write(_indent);
			output.write("cliques{"+(cidx)+"} = tabularFactorCreate(");
			output.write(tfId);
			output.write(", [");
			for (int idx = 0; idx < pmtk3TabularFactor.types.size(); idx++) {
				output.write(""+pmtk3TabularFactor.types.get(idx));
				if (idx < (pmtk3TabularFactor.types.size()-1)) {
					output.write(" ");
				}
			}
			output.write("]);\n");
			
			cidx++;
		}
 		
		output.write("\n");
		output.write(_indent);
		output.write("% Note: currently all LPI random variables only have 2 states (i.e. {false, true}).\n");
		output.write(_indent);
		output.write("nstates = 2*ones("+randomVariableToPmtk3Id.size()+", 1);\n");
		output.write("\n");
		output.write("end\n");
	}
	
	//
	// PRIVATE METHODS
	//
	private static void outputHeader(Model originalModel, Writer output) 
		throws IOException {
		//
		// Output the original model
		output.write("% LPI MODEL: ");
		output.write(originalModel.getName().toString());
		output.write("\n");
		output.write("% Description:\n");
		output.write("%{\n");
		output.write(originalModel.getDescription().toString());
		output.write("\n");
		output.write("%}\n");
		output.write("% Sort Declarations:\n");
		output.write("%{\n");
		for (SortDeclaration sort : originalModel.getSortDeclarations()) {
			output.write(sort.getSortDeclaration().toString());
			output.write("\n");
		}
		output.write("%}\n");
		output.write("% Random Variable Declarations:\n");
		output.write("%{\n");
		for (RandomVariableDeclaration rv : originalModel.getRandomVariableDeclarations()) {
			output.write(rv.getRandomVariableDeclaration().toString());
			output.write("\n");
		}
		output.write("%}\n");
		output.write("% Parfactor Declarations:\n");
		output.write("%{\n");
		for (Expression parfactor : originalModel.getParfactorsDeclaration().getParfactors()) {
			output.write(parfactor.toString());
			output.write("\n");
		}
		output.write("%}\n");
	}
	
	private static void outputLBPBeliefs(Model originalModel, Writer output, RewritingProcess process) 
		throws IOException {
		
		output.write("%\n% LBP Beliefs:\n");
		for (RandomVariableDeclaration rvd : originalModel.getRandomVariableDeclarations()) {
			int[] iterations = new int[] {10, 11, 12, 13, 14, 15, 
					500,  1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000,
					5500, 6000, 6500, 7000, 7500, 8000, 8500, 9000, 9500, 10000};
			
			LBPConfiguration lbpConfiguration = LBPFactory.newLBPConfiguration();
			// Want to be able to handle loopy models as well 
			lbpConfiguration.setBeliefPropagationUpdateSchedule(LBPConfiguration.BeliefPropagationUpdateSchedule.SYNCHRONOUS);
			// The number of iterations to run:
			lbpConfiguration.setMaxNumberOfIterationsForConvergence(iterations[iterations.length-1]);
			// Set up the belief value collector
			LBPConfiguration.BeliefValueAtIterationCollector beliefValueCollector = new LBPConfiguration.BeliefValueAtIterationCollector(iterations);
			lbpConfiguration.setBeliefValueAtIterationCollector(beliefValueCollector);
			
			List<Expression> queryVars = new ArrayList<Expression>();
			for (int i = 0; i < rvd.getArityValue(); i++) {
				queryVars.add(Expressions.makeSymbol("X"+i));
			}
			Expression randomVariable = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(BracketedExpressionSubExpressionsProvider.SYNTAX_TREE_LABEL,
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(rvd.getName(), queryVars));
			Expression query = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(LPIUtil.FUNCTOR_BELIEF, randomVariable);
			
			Expression belief = process.rewrite(LBPRewriter.R_belief, query);
			
			output.write("% Final Belief For: ");
			output.write(query.toString());
			output.write(" = \n");
			output.write("% ");
			output.write(belief.toString());
			output.write("\n%\n");
			for (Map.Entry<Integer, Expression> beliefValueAtIteration : beliefValueCollector.getCollectedBeliefValues().entrySet()) {
				output.write("% Belief Value at Iteration "+beliefValueAtIteration.getKey()+" = \n");
				output.write("% ");
				output.write(beliefValueAtIteration.getValue().toString());
				output.write("\n");
			}
			output.write("%\n");
		}
	}
	
	
	private static void outputMainFunction(Model groundedModel, Map<Expression, Integer> randomVariableToPmtk3Id, Writer output) 
		throws IOException {
		output.write("function [fg varNames] = mk");
		output.write(groundedModel.getName().toString());
		output.write("FactorGraph()\n");
		output.write(_indent);
		output.write("[cliques nstates] = mk"+groundedModel.getName().toString()+"CliquesAndStates();\n");
		output.write(_indent);
		output.write("fg = factorGraphCreate(cliques, nstates);\n");
		output.write("\n");
		output.write(_indent);
		output.write("varNames = cell("+randomVariableToPmtk3Id.size()+", 1);\n");
		for (Map.Entry<Expression, Integer> entry : randomVariableToPmtk3Id.entrySet()) {
			output.write(_indent);
			String varName = entry.getKey().toString();
			// Escape (i.e. Matlab) all quotes
			varName = varName.replaceAll("'", "''");
			output.write("varNames{"+entry.getValue()+"} = '"+varName+"';\n");
		}
		output.write("end\n");
	}
	
	private static String outputTabularFactorCreate(TabularFactor tabularFactor, Writer output)
			throws IOException {
		
		StringBuilder tout = new StringBuilder();
		
		String tfId = "T";
		// e.g. T = reshape([41 45 43 47 42 46 44 48], 2, 2, 2);
		tout.append(_indent);
		tout.append(tfId);
		if (tabularFactor.typeSizes.size() > 1) {
			tout.append(" = reshape(zeros(1, ");
			tout.append(tabularFactor.values.size());
			tout.append("), ");
			for (int i = 0; i < tabularFactor.typeSizes.size(); i++) {
				int typeSize = tabularFactor.typeSizes.get(i);
				tout.append(typeSize);
				if (i < (tabularFactor.typeSizes.size()-1)) {
					tout.append(", ");
				}
			}
		} 
		else {
			tout.append(" = zeros("+tabularFactor.typeSizes.get(0)+", 1");
		}

		tout.append(");\n");
		// Now output the sub2ind assignment to values
		// e.g. T(sub2ind(size(T), 1,1)) = 0.750000;
		int valIdx = 0;
		CartesianProductEnumeration<Expression> rangesCartesianProductEnumeration = new CartesianProductEnumeration<Expression>(tabularFactor.ranges, true);
		while (rangesCartesianProductEnumeration.hasMoreElements()) {
			List<Expression> rangeValues = rangesCartesianProductEnumeration.nextElement();
			tout.append(_indent);
			tout.append(tfId);
			tout.append("(sub2ind(size(");
			tout.append(tfId);
			tout.append("), ");
			for (int i = 0; i < rangeValues.size(); i++) {
				tout.append(tabularFactor.ranges.get(i).indexOf(rangeValues.get(i))+1);
				if (i < (rangeValues.size()-1)) {
					tout.append(",");
				}
			}
			tout.append(")) = ");
			tout.append(tabularFactor.values.get(valIdx).toStringDot(_precision));
			tout.append(";\n");
			valIdx++;
		}
		
		output.write(tout.toString());
		
		return tfId;
	}
	
	public static int getPmtk3RandomVariableId(Expression lpiRandomVariableValue, Map<Expression, Integer> randomVariableToPmtk3Id, Map<Integer, Expression> pmtk3IdToRandomVariable) {
		Integer id = randomVariableToPmtk3Id.get(lpiRandomVariableValue);
		if (null == id) {
			id = randomVariableToPmtk3Id.size()+1;
			randomVariableToPmtk3Id.put(lpiRandomVariableValue, id);
			pmtk3IdToRandomVariable.put(id, lpiRandomVariableValue);
		}
		
		return id;
	}
	
	static class TabularFactor {
		public Expression             factorValue = null;
		public List<List<Expression>> ranges      = new ArrayList<List<Expression>>();
		public List<Integer>          types       = new ArrayList<Integer>();
		public List<Integer>          typeSizes   = new ArrayList<Integer>();
		public List<Rational>         values      = new ArrayList<Rational>();
		//
		public Rewriter               rNormalize   = LBPFactory.newNormalize();
		
		
		// Create a default tabular factor with potentials = 1
		public TabularFactor(Expression randomVariableValueExpression, Integer typeId, RewritingProcess process) {
			factorValue = randomVariableValueExpression;
			List<Expression> range = Model.range(randomVariableValueExpression, process);
			ranges.add(range);
			types.add(typeId);
			typeSizes.add(range.size());
			int numDefaultValues = ranges.get(0).size();
			for (int i = 0; i < numDefaultValues; i++) {
				values.add(Rational.ONE);
			}
		}
		
		// Merge several tabular factors for the same types into one pmtk3 targeted factor
		public TabularFactor(List<TabularFactor> tabularFactors) {
			// Default everything to the first tabular factor initially
			TabularFactor firstFactor = tabularFactors.get(0);
			factorValue = firstFactor.factorValue;
			ranges.addAll(firstFactor.ranges);
			types.addAll(firstFactor.types);
			typeSizes.addAll(firstFactor.typeSizes);
			values.addAll(firstFactor.values);
			// Now merge in the remaining factor values
			for (int i = 1; i < tabularFactors.size(); i++) {
				List<Rational> otherValues = tabularFactors.get(i).values;
				for (int v = 0; v < values.size(); v++) {
					values.set(v, values.get(v).multiply(otherValues.get(v)));
				}
			}
		}
		
		public TabularFactor(Model groundedModel, Expression factorValue, 
				Map<Expression, Integer> randomVariableToPmtk3Id, Map<Integer, Expression> pmtk3IdToRandomVariable, 
				RewritingProcess process) {
			this.factorValue = factorValue;
			
			List<Expression> randomVariableValueExpressions = new ArrayList<Expression>();
			// Note: this just collects the random variable value expressions and doesn't replace them
			// however, need to use the replaceAllOccurrences() method in order to ensure traversal
			// order is consistent.
			factorValue.replaceAllOccurrences(new CollectRandomVariableValueExpressions(randomVariableValueExpressions), 
											process);
			
			for (Expression randomVariableValueExpression : randomVariableValueExpressions) {
				List<Expression> range = Model.range(randomVariableValueExpression, process);
				ranges.add(range);
				types.add(getPmtk3RandomVariableId(randomVariableValueExpression, randomVariableToPmtk3Id, pmtk3IdToRandomVariable));
				typeSizes.add(range.size());
			}
			
			// Now calculate the values for each entry in the tabular factor
			// Note: we will iterate the the ranges for the random variables, ordered left to right in terms of
			// how they are found in the parfactor via depth first. However, we will enumerate there values
			// in right to left order to be consistent with normal conventions.
			CartesianProductEnumeration<Expression> rangesCartesianProductEnumeration = new CartesianProductEnumeration<Expression>(ranges, true);
			while (rangesCartesianProductEnumeration.hasMoreElements()) {
				List<Expression> rangeValues = rangesCartesianProductEnumeration.nextElement();
				
				Expression parfactorWithRangeValues = factorValue.replaceAllOccurrences(new ValueReplacementFunction(randomVariableValueExpressions, rangeValues), process);
				
				// With values substituted (i.e. {false,true}) for each random variable value expression
				// this should simplify to a rational - if not something is wrong.
				Expression normalizedParfactorWithRangeValues = rNormalize.rewrite(parfactorWithRangeValues, process);
				Rational   rationalValue = normalizedParfactorWithRangeValues.rationalValue();
				values.add(rationalValue);
			}
			
			// Now ensure the type ids are sorted in order
			List<List<Expression>> sortedRanges      = new ArrayList<List<Expression>>(ranges);
			List<Integer>          sortedDomains     = new ArrayList<Integer>(types);
			List<Integer>          sortedTypeSizes = new ArrayList<Integer>(typeSizes);
			List<Rational>         sortedValues      = new ArrayList<Rational>(values); 
			Collections.sort(sortedDomains);
			int[] radices     = new int[sortedDomains.size()];
			int[] radixValues = new int[sortedDomains.size()];
			for (int i = 0; i < radices.length; i++) {
				int unsortedIndex = types.indexOf(sortedDomains.get(i));
				radices[i] = ranges.get(unsortedIndex).size();
				sortedRanges.set(i, ranges.get(unsortedIndex));
				sortedTypeSizes.set(i, typeSizes.get(unsortedIndex));
			}
			MixedRadixNumber sortedIndexer = new MixedRadixNumber(BigInteger.ZERO, radices);
			
			if (!sortedDomains.equals(types)) {
				int idx = 0;
				rangesCartesianProductEnumeration = new CartesianProductEnumeration<Expression>(ranges, true);
				while (rangesCartesianProductEnumeration.hasMoreElements()) {
					List<Expression> rangeValues = rangesCartesianProductEnumeration.nextElement();
					
					for (int i = 0; i < radixValues.length; i++) {
						int unsortedIndex = types.indexOf(sortedDomains.get(i));
						radixValues[unsortedIndex] = ranges.get(i).indexOf(rangeValues.get(i));
					}
					
					sortedValues.set(sortedIndexer.getValueFor(radixValues).intValue(), values.get(idx));
					
					idx++;
				}
			}
			
			ranges      = sortedRanges;
			types     = sortedDomains;
			typeSizes = sortedTypeSizes;
			values      = sortedValues;
		}
	}
	
	private static class CollectRandomVariableValueExpressions extends AbstractReplacementFunctionWithContextuallyUpdatedProcess { 
		private List<Expression> randomVariableValueExpressions = null;
		
		
		public CollectRandomVariableValueExpressions(List<Expression> randomVariableValueExpressions) {
			this.randomVariableValueExpressions = randomVariableValueExpressions;
		}
		
		//
		// START-ReplacementFunctionWithContextuallyUpdatedProcess
		@Override
		public Expression apply(Expression expressionE, RewritingProcess process) {
			
			// if a random variable value expression and we've not seen already.
			if (LPIUtil.isRandomVariableValueExpression(expressionE, process) &&
				!randomVariableValueExpressions.contains(expressionE)) {
				randomVariableValueExpressions.add(expressionE);
			}
			
			return expressionE;
		}
		
		// END-ReplacementFunctionWithContextuallyUpdatedProcess
		//
	}
	
	private static class ValueReplacementFunction extends AbstractReplacementFunctionWithContextuallyUpdatedProcess {
		private List<Expression> randomVariableValueExpressions = null;
		private List<Expression> rangeValues                    = null;
		
		public ValueReplacementFunction(List<Expression> randomVariableValueExpressions, List<Expression> rangeValues) {
			this.randomVariableValueExpressions = randomVariableValueExpressions;
			this.rangeValues                    = rangeValues;
		}
		
		//
		// START-ReplacementFunctionWithContextuallyUpdatedProcess
		@Override
		public Expression apply(Expression expressionE, RewritingProcess process) {
			Expression result = expressionE;
			
			if (LPIUtil.isRandomVariableValueExpression(expressionE, process)) {
				result = rangeValues.get(randomVariableValueExpressions.indexOf(expressionE));
			}
			
			return result;
		}
		
		// END-ReplacementFunctionWithContextuallyUpdatedProcess
		//
	}
}
