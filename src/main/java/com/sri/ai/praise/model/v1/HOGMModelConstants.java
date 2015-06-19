package com.sri.ai.praise.model.v1;

import java.util.Collections;
import java.util.LinkedHashSet;
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
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.library.FunctorConstants;

@Beta
public class HOGMModelConstants {
	public final static Set<String> KNOWN_BOOLEAN_FUNCTORS;
	static {
		Set<String> knownBooleanFunctors = new LinkedHashSet<String>();
		knownBooleanFunctors.add(FunctorConstants.NOT);
		knownBooleanFunctors.add(FunctorConstants.AND);
		knownBooleanFunctors.add(FunctorConstants.OR);
		knownBooleanFunctors.add(FunctorConstants.IMPLICATION);
		knownBooleanFunctors.add(FunctorConstants.EQUIVALENCE);
		knownBooleanFunctors.add(FunctorConstants.LESS_THAN);
		knownBooleanFunctors.add(FunctorConstants.LESS_THAN_OR_EQUAL_TO);
		knownBooleanFunctors.add(FunctorConstants.EQUALITY);
		knownBooleanFunctors.add(FunctorConstants.DISEQUALITY);
		knownBooleanFunctors.add(FunctorConstants.GREATER_THAN_OR_EQUAL_TO);
		knownBooleanFunctors.add(FunctorConstants.GREATER_THAN);
		
		KNOWN_BOOLEAN_FUNCTORS = Collections.unmodifiableSet(knownBooleanFunctors);
	}
	
	public final static Set<String> KNOWN_NUMERIC_FUNCTORS;
	static {
		Set<String> knownNumericFunctors = new LinkedHashSet<String>();
		knownNumericFunctors.add(FunctorConstants.CARDINALITY);
		knownNumericFunctors.add(FunctorConstants.EXPONENTIATION);
		knownNumericFunctors.add(FunctorConstants.DIVISION);
		knownNumericFunctors.add(FunctorConstants.TIMES);
		knownNumericFunctors.add(FunctorConstants.PLUS);
		knownNumericFunctors.add(FunctorConstants.MINUS);
		
		KNOWN_NUMERIC_FUNCTORS = Collections.unmodifiableSet(knownNumericFunctors);
	}
	
	public final static Set<String> KNOWN_UNDETERMINED_FUNCTORS;
	static {
		Set<String> undetermined = new LinkedHashSet<>();
		undetermined.add(FunctorConstants.IF_THEN_ELSE);
		
		KNOWN_UNDETERMINED_FUNCTORS = Collections.unmodifiableSet(undetermined);
	}
	
	public final static Set<String> KNOWN_FUNCTORS;
	static {
		Set<String> knownFunctors = new LinkedHashSet<>();
		knownFunctors.addAll(KNOWN_BOOLEAN_FUNCTORS);
		knownFunctors.addAll(KNOWN_NUMERIC_FUNCTORS);
		knownFunctors.addAll(KNOWN_UNDETERMINED_FUNCTORS);
		
		KNOWN_FUNCTORS = Collections.unmodifiableSet(knownFunctors);
	}
	
	public final static Set<String> KNOWN_ARITY_1_FUNCTORS;
	static {
		Set<String> arity1 = new LinkedHashSet<>();
		arity1.add(FunctorConstants.CARDINALITY);
		arity1.add(FunctorConstants.NOT);
		
		KNOWN_ARITY_1_FUNCTORS = Collections.unmodifiableSet(arity1);
	}
	
	public final static Set<String> KNOWN_ARITY_2_FUNCTORS;
	static {
		Set<String> arity2 = new LinkedHashSet<>();
		arity2.add(FunctorConstants.IMPLICATION);
		arity2.add(FunctorConstants.EQUIVALENCE);
		arity2.add(FunctorConstants.LESS_THAN);
		arity2.add(FunctorConstants.LESS_THAN_OR_EQUAL_TO);
		arity2.add(FunctorConstants.DISEQUALITY);
		arity2.add(FunctorConstants.GREATER_THAN_OR_EQUAL_TO);
		arity2.add(FunctorConstants.GREATER_THAN);
		arity2.add(FunctorConstants.EXPONENTIATION);
		arity2.add(FunctorConstants.DIVISION);
		
		KNOWN_ARITY_2_FUNCTORS = Collections.unmodifiableSet(arity2);
	}
	
	public final static Set<String> KNOWN_ARITY_3_FUNCTORS;
	static {
		Set<String> arity3 = new LinkedHashSet<>();
		arity3.add(FunctorConstants.IF_THEN_ELSE);
		
		KNOWN_ARITY_3_FUNCTORS = Collections.unmodifiableSet(arity3);
	}
	
	public final static Set<String> KNOWN_ARITY_1_OR_2_FUNCTORS;
	static {
		Set<String> arity1or2 = new LinkedHashSet<>();
		// i.e. unary minus or normal subtraction
		arity1or2.add(FunctorConstants.MINUS); 
		KNOWN_ARITY_1_OR_2_FUNCTORS = Collections.unmodifiableSet(arity1or2);
	}
	
	public final static Set<String> KNOWN_ARITY_0_OR_MORE_FUNCTORS;
	static {
		Set<String> arity0Plus = new LinkedHashSet<>();
		arity0Plus.add(FunctorConstants.AND);
		arity0Plus.add(FunctorConstants.OR);
		arity0Plus.add(FunctorConstants.EQUALITY);	
		
		KNOWN_ARITY_0_OR_MORE_FUNCTORS = Collections.unmodifiableSet(arity0Plus);
	}
	
	public final static Set<String> KNOWN_ARITY_GREATER_THAN_1_FUNCTORS;
	static {
		Set<String> arity2Plus = new LinkedHashSet<>();
		
		arity2Plus.add(FunctorConstants.TIMES);
		arity2Plus.add(FunctorConstants.PLUS);		
		
		KNOWN_ARITY_GREATER_THAN_1_FUNCTORS = Collections.unmodifiableSet(arity2Plus);
	}
}
