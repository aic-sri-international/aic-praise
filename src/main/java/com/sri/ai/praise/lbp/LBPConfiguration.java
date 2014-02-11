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
package com.sri.ai.praise.lbp;

import java.io.Serializable;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;

/**
 * Lifted Belief Propagation (LBP) configurable parameters.
 * 
 * @author oreilly
 *
 */
@Beta
public interface LBPConfiguration extends Serializable {
	/**
	 * Different types of Belief Propagation Update Schedules supported.
	 *  
	 * @author oreilly
	 *
	 */
	enum BeliefPropagationUpdateSchedule {
		SYNCHRONOUS,
		ASYNCHRONOUS_INDIVIDUAL_BASED_CYCLE_DETECTION,
		ASYNCHRONOUS_GROUP_BASED_CYCLE_DETECTION;
		
		public static BeliefPropagationUpdateSchedule getUpdateSchedule(String scheduleName) {
			if (scheduleName.equals("synchronous")) {
				return SYNCHRONOUS;
			} 
			else if (scheduleName.equals("asynchronous_individual_based_cycle_detection")) {
				return ASYNCHRONOUS_INDIVIDUAL_BASED_CYCLE_DETECTION;
			} 
			else if (scheduleName.equals("asynchronous_group_based_cycle_detection")) {
				return ASYNCHRONOUS_GROUP_BASED_CYCLE_DETECTION;
			}
			
			throw new IllegalArgumentException("Not a legal update schedule name:"+scheduleName);
		}
		
		public static boolean isAsynchronous(BeliefPropagationUpdateSchedule bpUpdateSchedule) {
			return (bpUpdateSchedule == ASYNCHRONOUS_INDIVIDUAL_BASED_CYCLE_DETECTION || 
				    bpUpdateSchedule == ASYNCHRONOUS_GROUP_BASED_CYCLE_DETECTION);
		}
	}
	
	class BeliefValueAtIterationCollector {
		private Set<Integer>             collectAtIterations     = new LinkedHashSet<Integer>();
		private Map<Integer, Expression> beliefValuesAtIteration = new LinkedHashMap<Integer, Expression>();
		
		/**
		 * Constructor.
		 * 
		 * @param collectAtIterations
		 *            the iteration numbers at which belief values should be
		 *            collected.
		 */
		public BeliefValueAtIterationCollector(int[] collectAtIterations) {
			for (int i = 0; i < collectAtIterations.length; i++) {
				this.collectAtIterations.add(collectAtIterations[i]);
			}
		}
		
		/**
		 * 
		 * @return an ordered list (based on the order of the
		 *         collectAtIterations list given on construction) of the
		 *         collected belief values.
		 */
		public Map<Integer, Expression> getCollectedBeliefValues() {
			return beliefValuesAtIteration;
		}
		
		/**
		 * Determine whether or not a belief value is to be collected for this
		 * iteration of LBP (permits calling code to determine whether or not to
		 * normalize the value).
		 * 
		 * @param iteration
		 *            the current iteration of LBP.
		 * @return true if it should be collected, false otherwise.
		 */
		public boolean isThisIterationWanted(int iteration) {
			return collectAtIterations.contains(iteration);
		}
		
		/**
		 * Set the belief value as calculated at a specific iteration of LBP.
		 * 
		 * @param iteration
		 *            the iteration
		 * @param beliefValue
		 *            the belief value at that iteration (should be normalized).
		 */
		public void setBeliefValueAtIteration(int iteration, Expression beliefValue) {
			if (isThisIterationWanted(iteration)) {
				beliefValuesAtIteration.put(iteration, beliefValue);
			}
		}
	}
	
	//
	// START - Configuration Options
	/**
	 * 
	 * @return true if the underlying R_belief implementation is using the caching
	 *         mechanisms outlined in the pseudo-code, false otherwise.
	 */
	boolean isBeliefUseCache();

	/**
	 * Set whether or not the underlying implementation should use the caching
	 * mechanisms outlined in the R_belief pseudo-code.
	 * 
	 * @param useCache
	 *            true if the pseudo-code caching mechanisms are to be used
	 *            false otherwise.
	 */
	void setBeliefUseCache(boolean useCache);
	
	/**
	 * 
	 * @return the belief propagation update schedule to be used by the
	 *         algorithm.
	 */
	BeliefPropagationUpdateSchedule getBeliefPropagationUpdateSchedule();

	/**
	 * Set the belief propagation update schedule to be used by the algorithm.
	 * 
	 * @param beliefPropagationUpdateSchedule
	 *            the belief propagation update schedule to be used by the
	 *            algorithm.
	 */
	void setBeliefPropagationUpdateSchedule(
			BeliefPropagationUpdateSchedule beliefPropagationUpdateSchedule);
	
	/**
	 * 
	 * @return the maximum number of iterations to run loopy lifted belief
	 *         propagation for when trying to achieve convergence.
	 */
	int getMaxNumberOfIterationsForConvergence();

	/**
	 * Set the maximum number of iterations to run loopy lifted belief
	 * propagation for when trying to achieve convergence.
	 * 
	 * @param maxNumberOfIterationsForConvergence
	 *            the maximum number of iterations to run loopy lifted belief
	 *            propagation for when trying to achieve convergence.
	 */
	void setMaxNumberOfIterationsForConvergence(
			int maxNumberOfIterationsForConvergence);
	
	/**
	 * @return the number of significant decimal places that normalized values
	 *         should be rounded to.
	 */
	int getLimitPrecisionToNumberOfSignificantDecimals();

	/**
	 * Set the number of significant decimal places that normalized values
	 * should be rounded to.
	 * 
	 * @param precision
	 */
	void setLimitPrecisionToNumberOfSignificantDecimals(int precision);
	
	/**
	 * @return an optional BeliefValueAtIterationCollector.
	 */
	BeliefValueAtIterationCollector getBeliefValueAtIterationCollector();
	
	/**
	 * Set the belief value at iteration collector to be used during LBP
	 * iteration.
	 * 
	 * @param beliefValueAtIterationCollector
	 *            the belief value iteration collector to be used during LBP
	 *            iteration.
	 */
	void setBeliefValueAtIterationCollector(BeliefValueAtIterationCollector beliefValueAtIterationCollector);
	
	/**
	 * 
	 * @return A testing-only field incremented every time a message is computed
	 *         by R_sum. This allows tests to ascertain that only the necessary
	 *         number of messages have been computed, which will be less than
	 *         the present number of messages when there are deterministic
	 *         functions involved. This needs to be explicitly set before use.
	 */
	int getSumRewriterTestMessageCounter();
	
	/**
	 * A testing-only field incremented every time a message is computed by
	 * R_sum. This allows tests to ascertain that only the necessary number of
	 * messages have been computed, which will be less than the present number
	 * of messages when there are deterministic functions involved. This needs
	 * to be explicitly set before use.
	 * 
	 * @param count
	 *            the count to set the counter to.
	 */
	void setSumRewriterTestMessageCounter(int count);

	/**
	 * 
	 * @return whether or not the relevant range is singleton heuristic detailed
	 *         in the R_sum method should be used.
	 */
	boolean isSumRewriterUsingSingletonRelevantRangeHeuristic();

	/**
	 * Set this in order to/not use the relevant range is singleton heuristic
	 * detailed in the compute method.
	 * 
	 * @param useHeuristic
	 *            true if the heuristic should be used, false otherwise.
	 */
	void setSumRewriterUsingSingletonRelevantRangeHeuristic(boolean useHeuristic);
	
	// END - Configuration Options
	//
}
