package com.sri.ai.praise.model.imports.uai;

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
import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;

/**
 * Utility routine for comparing different UAI results.
 * 
 * @author oreilly
 *
 */
@Beta
public class UAICompare {
	// Appears to be the precision used in the solution files provided.
	public static final int          UAI_PRECISON      = 6; 
	public static final RoundingMode UAI_ROUNDING_MODE = RoundingMode.HALF_UP;
	
	/**
	 * Compare two MAR results.
	 * 
	 * @param solution
	 * @param computed
	 * @return a list of the variable indexes whose results do not match.
	 */
	public static List<Integer> compareMAR(Map<Integer, List<Double>> solution, Map<Integer, List<Double>> computed) {
		List<Integer> result = new ArrayList<>();
		
		if (solution.size() != computed.size()) {
			throw new IllegalArgumentException("Solution size of "+solution.size()+" != Computed size of "+computed.size());
		}
		
		MathContext mc = new MathContext(UAI_PRECISON, UAI_ROUNDING_MODE);
		for (int varIdx = 0; varIdx < solution.size(); varIdx++) {
			List<Double> solutionValues = solution.get(varIdx);
			List<Double> computedValues = computed.get(varIdx);
			
			if (solutionValues.size() != computedValues.size()) {
				throw new IllegalArgumentException("Solution values size of "+solutionValues.size()+" does not match the computed values size of "+computedValues.size()+" for var "+varIdx);
			}
			
			for (int valueIdx = 0; valueIdx < solutionValues.size(); valueIdx++) {
				BigDecimal sol = new BigDecimal(solutionValues.get(valueIdx), mc);
				BigDecimal com = new BigDecimal(computedValues.get(valueIdx), mc);
				
				double diff = sol.doubleValue() - com.doubleValue();
				
				if (diff != 0.0) {
					result.add(varIdx);
					break;
				}
			}
		}
		
		
		return result;
	}
}
