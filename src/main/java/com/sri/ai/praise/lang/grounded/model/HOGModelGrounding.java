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
package com.sri.ai.praise.lang.grounded.model;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.IntStream;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.praise.model.v1.HOGMSortDeclaration;
import com.sri.ai.praise.model.v1.hogm.antlr.HOGMParserWrapper;
import com.sri.ai.praise.model.v1.hogm.antlr.ParsedHOGModel;
import com.sri.ai.praise.sgsolver.solver.ExpressionFactorsAndTypes;
import com.sri.ai.praise.sgsolver.solver.FactorsAndTypes;
import com.sri.ai.praise.sgsolver.solver.InferenceForFactorGraphAndEvidence;
import com.sri.ai.util.base.Triple;
import com.sri.ai.util.math.MixedRadixNumber;
import com.sri.ai.util.math.Rational;

@Beta
public class HOGModelGrounding {
	
	public interface Listener {
		void numberGroundVariables(int number);
		void groundVariableCardinality(int variableIndex, int cardinality);
		void numberFactors(int number);
		void factorParticipants(int factorIndex, int[] variableIndexes);
		void factorValue(int factorIndex, int numberFactorValues, int valueIndex, Rational value);
		void groundingComplete();
	}
	
	private static String PARSED_NUMBER_RANGE_FUNCTOR = "..";
	
	public static void main(String[] args) {
		StringJoiner sj = new StringJoiner("\n");
		sj.add("sort People : 10, Putin;");
		sj.add("random president : People;");
		sj.add("random communism : Boolean;");
		sj.add("random votePutin : 2..11;");
		sj.add("if president = Putin then communism else not communism;");
		sj.add("if votePutin > 5 then president = Putin else not president = Putin;");
		
		HOGMParserWrapper parser          = new HOGMParserWrapper();
		ParsedHOGModel    parsedModel     = parser.parseModel(sj.toString());
		FactorsAndTypes   factorsAndTypes = new ExpressionFactorsAndTypes(parsedModel);
		
		ground(factorsAndTypes, new Listener() { // NOTE: an example listener that outputs in the UAI format
			int numberVariables;
			StringJoiner preamble       = new StringJoiner("");
			StringJoiner functionTables = new StringJoiner("");
			@Override
			public void numberGroundVariables(int number) { 
				this.numberVariables = number;
				preamble.add("MARKOV\n");
				preamble.add(""+number+"\n");
			}	
			@Override
			public void groundVariableCardinality(int variableIndex, int cardinality) {
				preamble.add(""+cardinality);
				if (variableIndex == (numberVariables-1)) {
					preamble.add("\n");
				}
				else {
					preamble.add(" ");
				}
			}
			@Override
			public void numberFactors(int number) {
				preamble.add(""+number+"\n");
			}
			@Override
			public void factorParticipants(int factorIndex, int[] variableIndexes) {
				preamble.add(""+variableIndexes.length);
				for (int i = 0; i < variableIndexes.length; i++) {
					preamble.add(" "+variableIndexes[i]);
				}
				preamble.add("\n");
			}
			@Override
			public void factorValue(int factorIndex, int numberFactorValues, int valueIndex, Rational value) {
				if (valueIndex == 0) {
					functionTables.add("\n"+numberFactorValues+"\n");
				}
				else {
					functionTables.add(" ");
				}
				
				functionTables.add(""+value.doubleValue());
				
				if (valueIndex == (numberFactorValues -1)) {
					functionTables.add("\n");
				}
			}	
			@Override
			public void groundingComplete() {
				System.out.print(preamble);
				System.out.print(functionTables);
			}
		});
	}
	
	public static void ground(FactorsAndTypes factorsAndTypes, Listener listener) {
		if (factorsAndTypes.getMapFromNonUniquelyNamedConstantNameToTypeName().size() > 0) {
			throw new IllegalArgumentException("Constants cannot be grounded");
		}
		Map<Expression, Triple<Expression, Integer, List<Expression>>> randomVariableNameToTypeSizeAndUniqueConstants = createRandomVariableNameToTypeSizeAndUnqiueConstantsMap(factorsAndTypes);
		Map<Expression, Integer> randomVariableIndexes = new LinkedHashMap<>();
		AtomicInteger variableIndex = new AtomicInteger(-1);
		listener.numberGroundVariables(randomVariableNameToTypeSizeAndUniqueConstants.size());
		randomVariableNameToTypeSizeAndUniqueConstants.entrySet().forEach(entry -> {
			randomVariableIndexes.put(entry.getKey(), variableIndex.addAndGet(1));
			listener.groundVariableCardinality(variableIndex.get(), entry.getValue().second);
		});
		
		Map<Expression, List<Expression>> typeToValues = createTypeToValuesMap(factorsAndTypes, randomVariableNameToTypeSizeAndUniqueConstants);
		Map<String, String> newUniqueConstantToTypeMap = createGroundedUnqiueConstantToTypeMap(typeToValues);
	    
		ExpressionFactorsAndTypes grounedFactorsAndTypes = new ExpressionFactorsAndTypes(Collections.emptyList(),
							factorsAndTypes.getMapFromRandomVariableNameToTypeName(),
							factorsAndTypes.getMapFromNonUniquelyNamedConstantNameToTypeName(),
							newUniqueConstantToTypeMap,
							factorsAndTypes.getMapFromTypeNameToSizeString());

		InferenceForFactorGraphAndEvidence inferencer = new InferenceForFactorGraphAndEvidence(grounedFactorsAndTypes, false, null, true);
		
		listener.numberFactors(factorsAndTypes.getFactors().size());
		int factorIndex = 0;
		for (Expression factor : factorsAndTypes.getFactors()) {
	    	List<Expression> randomVariablesInFactor = new ArrayList<>(Expressions.getSubExpressionsSatisfying(factor, randomVariableNameToTypeSizeAndUniqueConstants::containsKey));
	    	if (randomVariablesInFactor.size() == 0) {
	    		throw new IllegalArgumentException("Factor contains no random variables: "+factor);
	    	}
	    	
	    	int[] radices                    = new int[randomVariablesInFactor.size()];
	    	int[] particiapntVariableIndexes = new int[randomVariablesInFactor.size()];
	    	List<List<Expression>> factorRandomVariableTypeValues = new ArrayList<>();
	    	for (int i = 0; i < randomVariablesInFactor.size(); i++) {
	    		Expression randomVariable = randomVariablesInFactor.get(i);
	    		
	    		Expression type               = randomVariableNameToTypeSizeAndUniqueConstants.get(randomVariable).first;
	    		radices[i]                    = randomVariableNameToTypeSizeAndUniqueConstants.get(randomVariable).second;
	    		particiapntVariableIndexes[i] = randomVariableIndexes.get(randomVariable);
	    		
	    		factorRandomVariableTypeValues.add(typeToValues.get(type));
	    	}
	    	
	    	listener.factorParticipants(factorIndex, particiapntVariableIndexes);
	    	
	    	RewritingProcess process = new DefaultRewritingProcess(null);
	    	boolean didIncrement     = true;
	    	MixedRadixNumber mrn     = new MixedRadixNumber(BigInteger.ZERO, radices);
	    	int numberFactorValues   = mrn.getMaxAllowedValue().intValue()+1;
	    	do {
	    		Expression groundedFactor = factor;
	    		for (int i = 0; i < randomVariablesInFactor.size(); i++) {
	    			int valueIndex = mrn.getCurrentNumeralValue(i);
	    			groundedFactor = groundedFactor.replaceAllOccurrences(randomVariablesInFactor.get(i), factorRandomVariableTypeValues.get(i).get(valueIndex), process);
	    		}  		
				Expression value = inferencer.evaluate(groundedFactor);
				if (!Expressions.isNumber(value)) {
					throw new IllegalStateException("Unable to compute a number for the grounded factor ["+groundedFactor+"], instead got:"+value);
				}
				
				listener.factorValue(factorIndex, numberFactorValues, mrn.getValue().intValue(), value.rationalValue());
				
				if (didIncrement = mrn.canIncrement()) {
					mrn.increment();
				}				
	    	} while (didIncrement);
	    }
		
		listener.groundingComplete();
		
		factorIndex++;
	}
	
	//
	// PRIVATE
	//
	private static Map<Expression, Triple<Expression, Integer, List<Expression>>> createRandomVariableNameToTypeSizeAndUnqiueConstantsMap(FactorsAndTypes factorsAndTypes) {
		Map<Expression, Triple<Expression, Integer, List<Expression>>> result = new LinkedHashMap<>();
		factorsAndTypes.getMapFromRandomVariableNameToTypeName().entrySet().forEach(entry -> {
			Expression       randomVariableName = Expressions.parse(entry.getKey());
			Expression       type               = Expressions.parse(entry.getValue());
			int              size               = 0;			
			List<Expression> uniqueConstants    = new ArrayList<>();
			if (Expressions.hasFunctor(type, FunctorConstants.FUNCTION_TYPE)) {
				throw new UnsupportedOperationException("Relational random variables, "+randomVariableName+", are currently not supported.");
			}
			else if (Expressions.hasFunctor(type, PARSED_NUMBER_RANGE_FUNCTOR)) {
				size = (type.get(1).intValueExact() - type.get(0).intValueExact()) + 1;
			}
			else {
				String strSize = factorsAndTypes.getMapFromTypeNameToSizeString().get(type);
				if (strSize == null) {
					throw new IllegalArgumentException("Size of sort, "+type+", is unknown");
				}
				size = Integer.parseInt(strSize);
				factorsAndTypes.getMapFromUniquelyNamedConstantNameToTypeName()
					.entrySet().stream()
					.filter(uniqueConstantAndTypeEntry -> uniqueConstantAndTypeEntry.getValue().equals(entry.getValue()))
					.forEach(uniqueConstantAndTypeEntry -> uniqueConstants.add(Expressions.parse(uniqueConstantAndTypeEntry.getKey())));
			}
			result.put(randomVariableName, new Triple<>(type, size, uniqueConstants));
		});
		return result;
	}
	
	private static Map<Expression, List<Expression>> createTypeToValuesMap(FactorsAndTypes factorsAndTypes, Map<Expression, Triple<Expression, Integer, List<Expression>>> randomVariableNameToTypeSizeAndUniqueConstants) {
		Map<Expression, List<Expression>> result = new LinkedHashMap<>();
		
		randomVariableNameToTypeSizeAndUniqueConstants.values().forEach(typeSizeAndUnqiueConstants -> {
			Expression       type            = typeSizeAndUnqiueConstants.first;
		    Integer          size            = typeSizeAndUnqiueConstants.second;
		    List<Expression> uniqueConstants = typeSizeAndUnqiueConstants.third;
			// random variables can share type information
			if (!result.containsKey(type)) {
				List<Expression> values = new ArrayList<>();
				// Is a numeric range
				if (Expressions.hasFunctor(type, PARSED_NUMBER_RANGE_FUNCTOR)) {
					int startInclusive = type.get(0).intValueExact();
					int endInclusive   = type.get(1).intValueExact();
					IntStream.rangeClosed(startInclusive, endInclusive).sequential().forEach(value -> values.add(Expressions.makeSymbol(value)));
					
				}
				else {
					if (HOGMSortDeclaration.IN_BUILT_BOOLEAN.getName().equals(type)) {
						values.addAll(HOGMSortDeclaration.IN_BUILT_BOOLEAN.getAssignedConstants());
					}
					else {
						// Is a sort name
						values.addAll(uniqueConstants);
						for (int i = uniqueConstants.size()+1; i <= size; i++) {
							values.add(Expressions.makeSymbol(type.toString().toLowerCase()+"_"+i));
						}
					}
				}
				result.put(type, values);
			}
		});
		
		return result;
	}
	
	private static Map<String, String> createGroundedUnqiueConstantToTypeMap(Map<Expression, List<Expression>> typeToValues) {
		Map<String, String> result = new LinkedHashMap<>();
		
		typeToValues.entrySet().stream()
			.filter(entry -> Expressions.isSymbol(entry.getKey()))
			.forEach(sortEntry -> {
				sortEntry.getValue().forEach(constant -> result.put(constant.toString(), sortEntry.getKey().toString()));
			});
		
		return result;
	}
}
