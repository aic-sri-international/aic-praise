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
package com.sri.ai.praise.core.translation.core.uai;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.util.concurrent.AtomicDouble;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.core.model.api.ModelLanguage;
import com.sri.ai.praise.core.model.classbased.table.core.data.FunctionTable;
import com.sri.ai.praise.core.model.classbased.table.core.uai.UAIModel;
import com.sri.ai.praise.core.model.classbased.table.core.uai.UAIUtil;

/**
 * Translator: UAI->HOGMv1 using equalities
 * 
 * @author oreilly
 *
 */
@Beta
public abstract class AbstractUAI_to_HOGMv1_Translator extends AbstractUAI_to_Target_Translator {
	//
	// START-Translator	
	@Override 
	public ModelLanguage getTarget() {
		return ModelLanguage.HOGMv1;
	}
	// END-Translator
	//
	
	@Override
	protected void translate(String inputIdentifier, UAIModel uaiModel, PrintWriter[] translatedOutputs) throws Exception {			
		PrintWriter hogmv1ModelWriter = translatedOutputs[0];
		
		//
		// 1. Output some comments with respect to the input model
		hogmv1ModelWriter.println("// IMPORT OF: "+inputIdentifier);
		hogmv1ModelWriter.println("//");
		hogmv1ModelWriter.println("// #variables                                = "+uaiModel.numberVariables());
		hogmv1ModelWriter.println("// #tables                                   = "+uaiModel.numberTables());
		hogmv1ModelWriter.println("// #unique function tables                   = "+uaiModel.numberUniqueFunctionTables());
		hogmv1ModelWriter.println("// Largest variable cardinality              = "+uaiModel.largestCardinality());
		hogmv1ModelWriter.println("// Largest # entries                         = "+uaiModel.largestNumberOfFunctionTableEntries());
		hogmv1ModelWriter.println("// Total #entries across all function tables = "+uaiModel.totalNumberEntriesForAllFunctionTables());
		
		//
		// 2. Output the sort and random variable declarations
		List<String> sorts   = new ArrayList<>();
		List<String> randoms = new ArrayList<>(); 
		for (int varIdx = 0; varIdx < uaiModel.numberVariables(); varIdx++) {
			int varCardinality = uaiModel.cardinality(varIdx);
			addSortAndRandomVariableDeclarationsRegarding(varIdx, varCardinality, sorts, randoms);
		}
		if (sorts.size() > 0) {
			hogmv1ModelWriter.println();
			hogmv1ModelWriter.println("// SORT DECLARATIONS:");
			sorts.forEach(sort -> hogmv1ModelWriter.println(sort));
		}
		hogmv1ModelWriter.println();
		hogmv1ModelWriter.println("// RANDOM VARIABLE DECLARATIONS:");
		randoms.forEach(random -> hogmv1ModelWriter.println(random));
		
		//
		// 3. Output the potentials
		hogmv1ModelWriter.println();
		hogmv1ModelWriter.println("// RULES:");
		
		double totalNumberUniqueEntries        = 0;
		double totalCompressedEntries          = 0;
		double bestIndividualCompressionRatio  = 100; // i.e. none at all
		double worstIndividualCompressionRatio = 0;
				
		for (int i = 0; i < uaiModel.numberUniqueFunctionTables(); i++) {
			FunctionTable table = uaiModel.getUniqueFunctionTable(i);
			
			totalNumberUniqueEntries += table.numberEntries();
			
			Expression genericTableExpression = convertToHOGMv1Expression(table);
			
			double compressedEntries = calculateCompressedEntries(genericTableExpression);
			
			double compressedRatio = compressedEntries / table.numberEntries();
			if (compressedRatio < bestIndividualCompressionRatio) {
				bestIndividualCompressionRatio = compressedRatio;
			}
			if (compressedRatio > worstIndividualCompressionRatio) {
				worstIndividualCompressionRatio = compressedRatio;
			}
			
			totalCompressedEntries += compressedEntries;
			
			for (int tableIdx : uaiModel.getTableIndexes(i)) {
				Expression instanceTableExpression = UAIUtil.convertGenericTableToInstance(table, genericTableExpression, uaiModel.getVariableIndexesForTable(tableIdx));
				// If just a number then table is just a constant and is irrelevant
				if (!Expressions.isNumber(instanceTableExpression)) {
					hogmv1ModelWriter.println(instanceTableExpression.toString()+";");
				}
			}
		}
		
		//
		// 4. Output some stats related to the translation to potentials
		hogmv1ModelWriter.println();
		hogmv1ModelWriter.println("// STATS: ");
		hogmv1ModelWriter.println("// Table compression ratio            = "+(totalCompressedEntries/totalNumberUniqueEntries));
		hogmv1ModelWriter.println("// Best individual compression ratio  = "+bestIndividualCompressionRatio);
		hogmv1ModelWriter.println("// Worst individual compression ratio = "+worstIndividualCompressionRatio);		
	}

	/**
	 * Adds sort and random variable declarations regarding the variable with given index to their respective given lists.
	 * @param varIdx
	 * @param varCardinality
	 * @param sorts
	 * @param randoms
	 */
	public abstract void addSortAndRandomVariableDeclarationsRegarding(int varIdx, int varCardinality, List<String> sorts, List<String> randoms);

	/**
	 * Provides the HOGMv1 expression for given {@link FunctionTable}.
	 * @param table
	 * @return
	 */
	public abstract Expression convertToHOGMv1Expression(FunctionTable table);

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
}