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
package com.sri.ai.praise.language.grounded.model;

import static com.sri.ai.expresso.helper.Expressions.isNumber;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.myAssert;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.IntStream;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.praise.inference.HOGMExpressionBasedModel;
import com.sri.ai.praise.inference.ExpressionBasedModel;
import com.sri.ai.praise.inference.InferenceForFactorGraphAndEvidence;
import com.sri.ai.praise.model.v1.HOGMSortDeclaration;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.TernaryProcedure;
import com.sri.ai.util.base.Triple;
import com.sri.ai.util.math.MixedRadixNumber;
import com.sri.ai.util.math.Rational;

@Beta
public class HOGModelGrounding {

	// TODO: this class needs to be cleaned up to use the unified functionalities of expresso Types.
	
	public static boolean useContextSensitiveGrounding = true;
	
	public interface Listener {
		//
		// Preamble information
		void numberGroundVariables(int number);
		void groundVariableCardinality(int variableIndex, int cardinality);
		void numberFactors(int number);
		void factorParticipants(int factorIndex, int[] variableIndexes);
		//
		// Function tables
		void factorValue(int numberFactorValues, boolean isFirstValue, boolean isLastValue, Rational value);
		//
		// Evidence
		void evidence(int variableIndex, int valueIndex);
		//
		// Indicates grounding complete
		void groundingComplete();
	}
	
	public static void ground(ExpressionBasedModel factorsAndTypes, List<Expression> evidence, Listener listener) {
		if (factorsAndTypes.getMapFromNonUniquelyNamedConstantNameToTypeName().size() > 0) {
			throw new IllegalArgumentException("Constants cannot be grounded");
		}
		Map<Expression, Triple<Expression, Integer, List<Expression>>> randomVariableNameToTypeSizeAndUniqueConstants = createRandomVariableNameToTypeSizeAndUniqueConstantsMap(factorsAndTypes);
		Map<Expression, Integer> randomVariableIndexes = new LinkedHashMap<>();
		AtomicInteger atomicVariableIndex = new AtomicInteger(-1);
		listener.numberGroundVariables(randomVariableNameToTypeSizeAndUniqueConstants.size());
		randomVariableNameToTypeSizeAndUniqueConstants.entrySet().forEach(entry -> {
			randomVariableIndexes.put(entry.getKey(), atomicVariableIndex.addAndGet(1));
			listener.groundVariableCardinality(atomicVariableIndex.get(), entry.getValue().second);
		});
		
		Map<Expression, List<Expression>> typeToValues = createTypeToValuesMap(factorsAndTypes, randomVariableNameToTypeSizeAndUniqueConstants);
		Map<String, String> newUniqueConstantToTypeMap = createGroundedUniqueConstantToTypeMap(typeToValues);
	    
		InferenceForFactorGraphAndEvidence inferencer = makeInferencer(factorsAndTypes, newUniqueConstantToTypeMap);
		
		Context context = inferencer.makeContextWithTypeInformation();
		
		listener.numberFactors(factorsAndTypes.getFactors().size());
		int factorIndex = 0;
		for (Expression factor : factorsAndTypes.getFactors()) {
	    	ArrayList<Expression> randomVariablesInFactor = new ArrayList<>(Expressions.getSubExpressionsSatisfying(factor, randomVariableNameToTypeSizeAndUniqueConstants::containsKey));
	    	if (randomVariablesInFactor.size() == 0) {
	    		throw new IllegalArgumentException("ExpressionFactorNode contains no random variables: "+factor);
	    	}
	    	
	    	int[] participantVariableIndexes = new int[randomVariablesInFactor.size()];
	    	for (int i = 0; i < randomVariablesInFactor.size(); i++) {
	    		Expression randomVariable = randomVariablesInFactor.get(i);
	    		participantVariableIndexes[i] = randomVariableIndexes.get(randomVariable);
	    	}
	    	listener.factorParticipants(factorIndex, participantVariableIndexes);

	    	if (!useContextSensitiveGrounding) {
	    		fullGrounding(
	    				factor,
	    				randomVariablesInFactor,
	    				listener,
	    				randomVariableNameToTypeSizeAndUniqueConstants,
	    				typeToValues,
	    				inferencer,
	    				context);
	    	}
	    	else {
	    		contextSensitiveGrounding(
	    				factor,
	    				randomVariablesInFactor,
	    				listener,
	    				randomVariableNameToTypeSizeAndUniqueConstants,
	    				typeToValues,
	    				inferencer,
	    				context);
	    	}

	    	factorIndex++;
		}
		
		// Handle the evidence
		for (Expression evidenceAssignment : evidence) {
			if (Expressions.isFunctionApplicationWithArguments(evidenceAssignment)) {
				// TODO - add support for 'not <variable>' and 'variable = value' and 'value = variable'
				throw new UnsupportedOperationException("Function application of evidence currently not supported: "+evidenceAssignment);
			}
			else if (Expressions.isSymbol(evidenceAssignment)) {
				int evidenceVariableIndex = randomVariableIndexes.get(evidenceAssignment);
				int evidenceValueIndex    = typeToValues.get(randomVariableNameToTypeSizeAndUniqueConstants.get(evidenceAssignment).first).indexOf(Expressions.TRUE);
				listener.evidence(evidenceVariableIndex, evidenceValueIndex);
			}
		}
		
		listener.groundingComplete();
	}

	/**
	 * Provides an appropriate {@link InferenceForFactorGraphAndEvidence} object.
	 * @param factorsAndTypes
	 * @param newUniqueConstantToTypeMap
	 * @return
	 */
	private static InferenceForFactorGraphAndEvidence makeInferencer(ExpressionBasedModel factorsAndTypes, Map<String, String> newUniqueConstantToTypeMap) {
		HOGMExpressionBasedModel groundedFactorsAndTypesInformation = 
				new HOGMExpressionBasedModel(
						Collections.emptyList(), // factors
						factorsAndTypes.getMapFromRandomVariableNameToTypeName(),
						factorsAndTypes.getMapFromNonUniquelyNamedConstantNameToTypeName(),
						newUniqueConstantToTypeMap,
						factorsAndTypes.getMapFromCategoricalTypeNameToSizeString(),
						list()); // additional types
		InferenceForFactorGraphAndEvidence inferencer = new InferenceForFactorGraphAndEvidence(groundedFactorsAndTypesInformation, false, null, true, null);
		return inferencer;
	}

	/**
	 * @param randomVariableNameToTypeSizeAndUniqueConstants
	 * @param randomVariablesInFactor
	 * @return
	 */
	private static BinaryFunction<Integer, Integer, Expression> makeFunctionFromVariableIndexValueIndexToValue(
			Map<Expression, Triple<Expression, Integer, List<Expression>>> randomVariableNameToTypeSizeAndUniqueConstants,
			ArrayList<Expression> randomVariablesInFactor,
			Map<Expression, List<Expression>> typeToValues) {
		
		return (variableIndex, valueIndex)
		-> {
			Expression variable = randomVariablesInFactor.get(variableIndex.intValue());
			Expression type = randomVariableNameToTypeSizeAndUniqueConstants.get(variable).first;
			return typeToValues.get(type).get(valueIndex);
		};
	}

	/**
	 * @param randomVariableNameToTypeSizeAndUniqueConstants
	 * @param randomVariablesInFactor
	 * @return
	 */
	private static Function<Integer, Integer> makeFunctionFromVariableIndexToDomainSize(
			Map<Expression, Triple<Expression, Integer, List<Expression>>> randomVariableNameToTypeSizeAndUniqueConstants,
			ArrayList<Expression> randomVariablesInFactor) {
		
		return (variableIndex)
		-> {
			Expression variable = randomVariablesInFactor.get(variableIndex.intValue());
			Triple<Expression, Integer, List<Expression>>
			typeCardinalityAndConstants
			= randomVariableNameToTypeSizeAndUniqueConstants.get(variable);
			return typeCardinalityAndConstants.second;
		};
	}

	/**
	 * @param factor
	 * @param randomVariablesInFactor
	 * @param listener
	 * @param randomVariableNameToTypeSizeAndUniqueConstants
	 * @param typeToValues
	 * @param inferencer
	 * @param context
	 */
	private static void fullGrounding(Expression factor, List<Expression> randomVariablesInFactor, Listener listener, Map<Expression, Triple<Expression, Integer, List<Expression>>> randomVariableNameToTypeSizeAndUniqueConstants, Map<Expression, List<Expression>> typeToValues, InferenceForFactorGraphAndEvidence inferencer, Context context) {
		int[] radices                    = new int[randomVariablesInFactor.size()];
		List<List<Expression>> factorRandomVariableTypeValues = new ArrayList<>();
		for (int i = 0; i < randomVariablesInFactor.size(); i++) {
			Expression randomVariable = randomVariablesInFactor.get(i);
			Expression type               = randomVariableNameToTypeSizeAndUniqueConstants.get(randomVariable).first;
			radices[i]                    = randomVariableNameToTypeSizeAndUniqueConstants.get(randomVariable).second;
			factorRandomVariableTypeValues.add(typeToValues.get(type));
		}
		
		boolean didIncrement     = true;
		MixedRadixNumber mrn     = new MixedRadixNumber(BigInteger.ZERO, radices);
		int numberFactorValues   = mrn.getMaxAllowedValue().intValue()+1;
		do {
			Expression groundedFactor = factor;
			for (int i = 0; i < randomVariablesInFactor.size(); i++) {
				int valueIndex = mrn.getCurrentNumeralValue(i);
				groundedFactor = groundedFactor.replaceAllOccurrences(randomVariablesInFactor.get(i), factorRandomVariableTypeValues.get(i).get(valueIndex), context);
			}  		
			Expression value = inferencer.simplify(groundedFactor, context);
			//				Expression value = inferencer.evaluate(groundedFactor);
			if (!Expressions.isNumber(value)) {
				throw new IllegalStateException("Unable to compute a number for the grounded factor ["+groundedFactor+"], instead got:"+value);
			}

			boolean isFirstValue = mrn.getValue().intValue() == 0;
			boolean isLastValue = mrn.getValue().intValue() == numberFactorValues - 1;
			listener.factorValue(numberFactorValues, isFirstValue, isLastValue, value.rationalValue());

			if (didIncrement = mrn.canIncrement()) {
				mrn.increment();
			}				
		} while (didIncrement);
	}

	//
	// PRIVATE
	//
	private static Map<Expression, Triple<Expression, Integer, List<Expression>>> createRandomVariableNameToTypeSizeAndUniqueConstantsMap(ExpressionBasedModel factorsAndTypes) {
		Map<Expression, Triple<Expression, Integer, List<Expression>>> result = new LinkedHashMap<>();
		factorsAndTypes.getMapFromRandomVariableNameToTypeName().entrySet().forEach(entry -> {
			Expression       randomVariableName = Expressions.parse(entry.getKey());
			Expression       type               = Expressions.parse(entry.getValue());
			int              size               = 0;			
			List<Expression> uniqueConstants    = new ArrayList<>();
			if (Expressions.hasFunctor(type, FunctorConstants.FUNCTION_TYPE)) {
				throw new UnsupportedOperationException("Relational random variables, "+randomVariableName+", are currently not supported.");
			}
			else if (Expressions.hasFunctor(type, HOGMSortDeclaration.IN_BUILT_INTEGER.getName()) && type.numberOfArguments() == 2) {
				size = (type.get(1).intValueExact() - type.get(0).intValueExact()) + 1;
			}
			else if (type.hasFunctor(FunctorConstants.INTEGER_INTERVAL) && type.numberOfArguments() == 2) {
				size = (type.get(1).intValueExact() - type.get(0).intValueExact()) + 1;
			}
			else {
				String sizeString = factorsAndTypes.getMapFromCategoricalTypeNameToSizeString().get(type.toString());
				if (sizeString == null) {
					throw new IllegalArgumentException("Size of sort " + type + " is unknown");
				}
				size = Integer.parseInt(sizeString);
				factorsAndTypes.getMapFromUniquelyNamedConstantNameToTypeName()
					.entrySet().stream()
					.filter(uniqueConstantAndTypeEntry -> uniqueConstantAndTypeEntry.getValue().equals(entry.getValue()))
					.forEach(uniqueConstantAndTypeEntry -> uniqueConstants.add(Expressions.parse(uniqueConstantAndTypeEntry.getKey())));
			}
			result.put(randomVariableName, new Triple<>(type, size, uniqueConstants));
		});
		return result;
	}
	
	private static Map<Expression, List<Expression>> createTypeToValuesMap(ExpressionBasedModel factorsAndTypes, Map<Expression, Triple<Expression, Integer, List<Expression>>> randomVariableNameToTypeExpressionTypeSizeAndUniqueConstants) {
		Map<Expression, List<Expression>> typeToValuesMap = new LinkedHashMap<>();
		
		randomVariableNameToTypeExpressionTypeSizeAndUniqueConstants.values().forEach(typeSizeAndUniqueConstants -> {
			Expression       type            = typeSizeAndUniqueConstants.first;
		    Integer          size            = typeSizeAndUniqueConstants.second;
		    List<Expression> uniqueConstants = typeSizeAndUniqueConstants.third;
			// random variables can share type information
			if (!typeToValuesMap.containsKey(type)) {
				List<Expression> values = new ArrayList<>();
				// Is a numeric range
				if (Expressions.hasFunctor(type, HOGMSortDeclaration.IN_BUILT_INTEGER.getName()) && type.numberOfArguments() == 2) {
					int startInclusive = type.get(0).intValueExact();
					int endInclusive   = type.get(1).intValueExact();
					IntStream.rangeClosed(startInclusive, endInclusive).sequential().forEach(value -> values.add(Expressions.makeSymbol(value)));	
				}
				else {
					if (HOGMSortDeclaration.IN_BUILT_BOOLEAN.getName().equals(type)) {
						values.addAll(HOGMSortDeclaration.IN_BUILT_BOOLEAN.getAssignedConstants());
					}
					else if (type.hasFunctor(FunctorConstants.INTEGER_INTERVAL) && type.numberOfArguments() == 2) {
						int firstValue;
						int lastValue;
						try {
							firstValue = type.get(0).intValue();
							lastValue = type.get(1).intValue();
							for (int i = firstValue; i != lastValue + 1; i++) {
								values.add(makeSymbol(i));
							}
						}
						catch (Error e) {
							throw new Error("Integer interval can only be grounded if it has fixed bounds, but got " + type);
						}
					}
					else {
						// Is a sort name
						values.addAll(uniqueConstants);
						for (int i = uniqueConstants.size() + 1; i <= size; i++) {
							values.add(Expressions.makeSymbol(type.toString().toLowerCase() + "_" + i));
						}
					}
				}
				typeToValuesMap.put(type, values);
			}
		});
		
		return typeToValuesMap;
	}
	
	private static Map<String, String> createGroundedUniqueConstantToTypeMap(Map<Expression, List<Expression>> typeToValues) {
		Map<String, String> result = new LinkedHashMap<>();
		
		typeToValues.entrySet().stream()
			.filter(entry -> Expressions.isSymbol(entry.getKey()))
			.forEach(sortEntry -> {
				sortEntry.getValue().forEach(constant -> result.put(constant.toString(), sortEntry.getKey().toString()));
			});
		
		return result;
	}
	
	/**
	 * @param factor
	 * @param randomVariablesInFactor
	 * @param listener
	 * @param randomVariableNameToTypeSizeAndUniqueConstants
	 * @param typeToValues TODO
	 * @param inferencer
	 * @param context
	 */
	private static void contextSensitiveGrounding(Expression factor, ArrayList<Expression> randomVariablesInFactor, Listener listener, Map<Expression, Triple<Expression, Integer, List<Expression>>> randomVariableNameToTypeSizeAndUniqueConstants, Map<Expression, List<Expression>> typeToValues, InferenceForFactorGraphAndEvidence inferencer, Context context) {
		Function<Integer, Integer> fromVariableIndexToDomainSize = 
				makeFunctionFromVariableIndexToDomainSize(randomVariableNameToTypeSizeAndUniqueConstants, randomVariablesInFactor);
		int numberFactorValues = 
				numberOfAssignmentsForVariablesStartingAt(0, randomVariablesInFactor.size(), fromVariableIndexToDomainSize);
		
		contextSensitiveGroundingFrom(
				0,
				factor, // starting from first variable
				randomVariablesInFactor, // variables to be used
				makeFunctionFromVariableIndexValueIndexToValue(randomVariableNameToTypeSizeAndUniqueConstants, randomVariablesInFactor, typeToValues),
				fromVariableIndexToDomainSize,
				inferencer.getTheory(),
				true, // first time this variable is being iterated (it happens only once)
				true, // last time this variable is being iterated (it happens only once)
				(isFirstValue, isLastValue, value)
				-> listener.factorValue(numberFactorValues, isFirstValue, isLastValue, value.rationalValue()),
				context);
	}

	private static void contextSensitiveGroundingFrom(
			int variableIndex,
			Expression expression,
			ArrayList<Expression> variables,
			BinaryFunction<Integer, Integer, Expression> fromVariableIndexAndValueIndexToValue,
			Function<Integer, Integer> fromVariableIndexToDomainSize,
			Theory theory,
			boolean firstIterationForVariable,
			boolean lastIterationForVariable,
			TernaryProcedure<Boolean, Boolean, Expression> recordValue,
			Context context) {
		
		Expression variable = variables.get(variableIndex);
		boolean isLastVariable = variableIndex == variables.size() - 1;
		int numberOfVariableValues = fromVariableIndexToDomainSize.apply(variableIndex);
		
		for (int variableValueIndex = 0; variableValueIndex != numberOfVariableValues; variableValueIndex++) {
			boolean thisVariableIsAtItsFirstValue = variableValueIndex == 0;
			boolean thisVariableIsAtItsLastValue = variableValueIndex == numberOfVariableValues - 1;
			Expression value = fromVariableIndexAndValueIndexToValue.apply(variableIndex, variableValueIndex);
			Expression expressionWithReplacedValue = expression.replaceAllOccurrences(variable, value, context);
			Expression simplifiedExpression = theory.simplify(expressionWithReplacedValue, context);
			
			boolean expressionIsSimplifiedToConstant =
					isLastVariable || simplifiedExpression.getSyntacticFormType().equals(Symbol.SYNTACTIC_FORM_TYPE);

			if (expressionIsSimplifiedToConstant) {
				myAssert( () -> isNumber(simplifiedExpression) , () -> "Expression being grounded has been simplified to a symbol that is not a numerical constant: " + simplifiedExpression);

				int numberOfTimesThisValueMustBeWritten
				= numberOfAssignmentsForVariablesStartingAt(variableIndex + 1, variables.size(), fromVariableIndexToDomainSize);
				
				for (int i = 0; i != numberOfTimesThisValueMustBeWritten; i++) {
					boolean isFirstOverallValue = 
							firstIterationForVariable && 
							thisVariableIsAtItsFirstValue &&
							i == 0;
					
					boolean isLastOverallValue =
							lastIterationForVariable &&
							thisVariableIsAtItsLastValue &&
							i == numberOfTimesThisValueMustBeWritten - 1;
					
					recordValue.apply(isFirstOverallValue, isLastOverallValue, simplifiedExpression);
				}
			}
			else {
				boolean firstIterationForNextVariable
				= firstIterationForVariable && thisVariableIsAtItsFirstValue;
				
				boolean lastIterationForNextVariable
				= lastIterationForVariable && thisVariableIsAtItsLastValue;
				
				contextSensitiveGroundingFrom(
						variableIndex + 1,
						simplifiedExpression,
						variables,
						fromVariableIndexAndValueIndexToValue,
						fromVariableIndexToDomainSize,
						theory,
						firstIterationForNextVariable,
						lastIterationForNextVariable,
						recordValue,
						context);
			}
		}
	}

	private static int numberOfAssignmentsForVariablesStartingAt(int variableIndex, int numberOfVariables, Function<Integer, Integer> domainSize) {
		if (variableIndex == numberOfVariables) {
			return 1;
		}
		else {
			int result = 1;
			for (int i = variableIndex; i != numberOfVariables; i++) {
				Integer variableDomainSize = domainSize.apply(i);
				myAssert( () -> variableDomainSize != 0, () -> "ExpressionVariable domain size cannot be zero");
				result *= variableDomainSize;
			}
			return result;
		}
	}
}
