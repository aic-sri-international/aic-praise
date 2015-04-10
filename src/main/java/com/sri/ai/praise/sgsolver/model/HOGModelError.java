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
package com.sri.ai.praise.sgsolver.model;

import com.google.common.annotations.Beta;

@Beta
public class HOGModelError {
	public static enum Type {
		//
		// SORT RELATED ERRORS
		SORT_DECLARATION_IS_NOT_LEGAL,
		SORT_NAME_PREDEFINED,
		SORT_NAME_NOT_UNIQUE,
		CONSTANT_NAME_NOT_UNIQUE,
		//
		// RANDOM RELATED ERRORS
		RANDOM_VARIABLE_IS_NOT_LEGAL,
		RANDOM_VARIABLE_NAME_NOT_UNIQUE,
		RANDOM_VARIABLE_NAME_SAME_AS_CONSTANT,
		RANDOM_VARIABLE_NAME_SAME_AS_IN_BUILT_FUNCTOR,	
		RANDOM_VARIABLE_NAME_SAME_AS_QUANTIFIER,
		RANDOM_VARIABLE_SORT_ARGUMENT_NOT_DECLARED,
		RANDOM_VARIABLE_ARGUMENT_IS_OF_THE_INCORRENT_TYPE,
		//
		// TERM RELATED ERRORS
		TERM_TYPE_OF_FUNCTOR_NOT_DECLARED,
		TERM_ARITY_OF_FUNCTOR_DOES_NOT_MATCH_DECLARATION,
		TERM_CONSTANT_NOT_DEFINED,
		TERM_ARGUMENT_IS_OF_THE_INCORRENT_TYPE,
		TERM_ARGUMENTS_MUST_ALL_BE_OF_THE_SAME_TYPE,
		TERM_SORT_CANNOT_BE_DETERMINED,
		TERM_CONDITONAL_STATEMENT_MUST_BE_OF_TYPE_NUMERIC,
		TERM_NON_CONDITIONAL_STATEMENT_MUST_BE_OF_TYPE_BOOLEAN,
	}
	
	//
	private Type          errorType   = null;
	private String        message     = null;
	private StatementInfo inStatement = null;
	
	public HOGModelError(Type errorType, String message, StatementInfo inStatement) {
		this.errorType   = errorType;
		this.message     = message;
		this.inStatement = inStatement;
	}
	
	public Type getErrorType() {
		return errorType;
	}
	
	public String getMessage() {
		return message;
	}
	
	public StatementInfo getInStatementInfo() {
		return inStatement;
	}
	
	@Override
	public String toString() {
		return errorType.name()+":["+message+"] - "+inStatement;
	}
}
