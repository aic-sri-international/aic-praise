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
package com.sri.ai.praise;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.lbp.core.DefaultLBPQueryEngine;
import com.sri.ai.util.Configuration;

/**
 * Configuration information specific to this project.
 * 
 * @author oreilly
 *
 */
@Beta
public class PRAiSEConfiguration extends Configuration {


	//
	public static final String  KEY_PERFORM_MAX_ALLOWED_SIZE_CHECK_FOR_GROUNDED_MODEL                      = "praise.perform.max.allowed.size.check.for.grounded.model";
	public static final Boolean DEFAULT_VALUE_PERFORM_MAX_ALLOWED_SIZE_CHECK_FOR_GROUNDED_MODEL            = Boolean.TRUE;
	//
	public static final String  KEY_MAX_ALLOWED_SIZE_FOR_GROUNDED_MODEL                                    = "praise.max.allowed.size.for.grounded.model";
	public static final Integer DEFAULT_VALUE_MAX_ALLOWED_SIZE_FOR_GROUNDED_MODEL                          = new Integer(10000);
	//
	public static final String  KEY_LBP_USE_SINGLETON_RELEVANT_RANGE_HEURISTIC                             = "praise.lbp.use.singleton.relevant.range.heuristic";
	public static final Boolean DEFAULT_VALUE_LBP_USE_SINGLETON_RELEVANT_RANGE_HEURISTIC                   = Boolean.TRUE;
	//
	public static final String  KEY_LBP_MAX_NUMBER_OF_ITERATIONS_FOR_CONVERGENCE                           = "praise.lbp.max.number.of.iterations.for.convergence";
	public static final Integer DEFAULT_VALUE_LBP_MAX_NUMBER_OF_ITERATIONS_FOR_CONVERGENCE                 = new Integer(10);
	//
	public static final String  KEY_LBP_BELIEF_USE_CACHE                                                   = "praise.lbp.belief.use.cache";
	public static final Boolean DEFAULT_VALUE_LBP_BELIEF_USE_CACHE                                         = new Boolean(true);
	//
	public static final String  KEY_LBP_BELIEF_PROPAGATION_UPDATE_SCHEDULE_NAME                            = "praise.lbp.belief.propagation.update.schedule.name";
	public static final String  DEFAULT_VALUE_LBP_BELIEF_PROPAGATION_UPDATE_SCHEDULE_NAME                  = "asynchronous_individual_based_cycle_detection";
	//
	public static final String  KEY_LBP_BELIEF_LIMIT_PRECISION_TO_NUMBER_OF_SIGNIFICANT_DECIMALS           = "praise.lbp.belief.limit.precision.to.number.significant.decimals";
	public static final Integer DEFAULT_VALUE_LBP_BELIEF_LIMIT_PRECISION_TO_NUMBER_OF_SIGNIFICANT_DECIMALS = new Integer(20);
	//
	public static final String  KEY_LBP_QUERY_ENGINE_ROUND_RESULT_TO                                       = "praise.lbp.query.engine.round.result.to";
	public static final Integer DEFAULT_VALUE_LBP_QUERY_ENGINE_ROUND_RESULT_TO                             = new Integer(6);
	//
	public static final String  KEY_LBP_QUERY_ENGINE_CLASS                                                 = "praise.lbp.query.engine.class";
	public static final String  DEFAULT_VALUE_LBP_QUERY_ENGINE_CLASS                                       = DefaultLBPQueryEngine.class.getName();
	//
	public static final String  KEY_MODEL_CARDINALITY_OF_TYPES_ALWAYS_GREATER_THAN_ZERO                    = "praise.model.cardinality.of.types.always.greater.than.zero";
	public static final Boolean DEFAULT_VALUE_MODEL_CARDINALITY_OF_TYPES_ALWAYS_GREATER_THAN_ZERO          = Boolean.TRUE;
	// Note: a value < 0 for this properties implies it is INFINITY and therefore unknown.
	public static final String  KEY_MODEL_DEFAULT_SIZE_OF_ALL_TYPES                                        = "praise.model.default.size.of.all.types";
	public static final Integer DEFAULT_VALUE_MODEL_DEFAULT_SIZE_OF_ALL_TYPES                              = new Integer(10);
	// Note: by default we assume true and use the default (above) if not explicitly specified.
	public static final String  KEY_MODEL_ALL_TYPE_SIZES_KNOWN                                             = "praise.model.all.type.sizes.known";
	public static final Boolean DEFAULT_VALUE_MODEL_ALL_TYPE_SIZES_KNOWN                                   = Boolean.TRUE; 
	
	public static boolean isPerformMaxAllowedSizeCheckForGroundedModel() {
		boolean result = getBoolean(KEY_PERFORM_MAX_ALLOWED_SIZE_CHECK_FOR_GROUNDED_MODEL, DEFAULT_VALUE_PERFORM_MAX_ALLOWED_SIZE_CHECK_FOR_GROUNDED_MODEL);
	
		return result;
	}
	
	public static int getMaxAllowedSizeForGroundedModel() {
		int result = getInt(KEY_MAX_ALLOWED_SIZE_FOR_GROUNDED_MODEL, DEFAULT_VALUE_MAX_ALLOWED_SIZE_FOR_GROUNDED_MODEL);
		
		return result;
	}
	
	public static boolean isLBPUseSingletonRelevantRangeHeuristic() {
		boolean result = getBoolean(KEY_LBP_USE_SINGLETON_RELEVANT_RANGE_HEURISTIC, DEFAULT_VALUE_LBP_USE_SINGLETON_RELEVANT_RANGE_HEURISTIC);
		
		return result;
	}
	
	public static int getLBPMaxNumberOfIterationsForConvergence() {
		int result = getInt(KEY_LBP_MAX_NUMBER_OF_ITERATIONS_FOR_CONVERGENCE, DEFAULT_VALUE_LBP_MAX_NUMBER_OF_ITERATIONS_FOR_CONVERGENCE);
		
		return result;
	}
	
	public static boolean getLBPBeliefUseCache() {
		boolean result = getBoolean(KEY_LBP_BELIEF_USE_CACHE, DEFAULT_VALUE_LBP_BELIEF_USE_CACHE);
		
		return result;
	}
	
	public static String getLBPBeliefPropagationUpdateScheduleName() {
		String result = getString(KEY_LBP_BELIEF_PROPAGATION_UPDATE_SCHEDULE_NAME, DEFAULT_VALUE_LBP_BELIEF_PROPAGATION_UPDATE_SCHEDULE_NAME);
		
		return result;
	}
	
	public static int getLBPBeliefLimitPrecisionToNumberOfSignificantDecimals() {
		int result = getInt(KEY_LBP_BELIEF_LIMIT_PRECISION_TO_NUMBER_OF_SIGNIFICANT_DECIMALS, DEFAULT_VALUE_LBP_BELIEF_LIMIT_PRECISION_TO_NUMBER_OF_SIGNIFICANT_DECIMALS);
		
		return result;
	}
	
	public static int getLBPQueryEngineRoundResultTo() {
		int result = getInt(KEY_LBP_QUERY_ENGINE_ROUND_RESULT_TO, DEFAULT_VALUE_LBP_QUERY_ENGINE_ROUND_RESULT_TO);
		
		return result;
	}
	
	public static String getLBPQueryEngineClass() {
		String result = getString(KEY_LBP_QUERY_ENGINE_CLASS, DEFAULT_VALUE_LBP_QUERY_ENGINE_CLASS);
		
		return result;
	}
	
	public static boolean isCardinalityOfTypesAlwaysGreaterThanZero() {
		boolean result = getBoolean(KEY_MODEL_CARDINALITY_OF_TYPES_ALWAYS_GREATER_THAN_ZERO, DEFAULT_VALUE_MODEL_CARDINALITY_OF_TYPES_ALWAYS_GREATER_THAN_ZERO);
		
		return result;
	}
	
	public static int getModelDefaultSizeOfAllTypes() {
		int result = getInt(KEY_MODEL_DEFAULT_SIZE_OF_ALL_TYPES, DEFAULT_VALUE_MODEL_DEFAULT_SIZE_OF_ALL_TYPES);
		
		return result;
	}
	
	public static boolean isAllTypeSizesKnownInModel() {
		boolean result = getBoolean(KEY_MODEL_ALL_TYPE_SIZES_KNOWN, DEFAULT_VALUE_MODEL_ALL_TYPE_SIZES_KNOWN);
	
		return result;
	}
}
