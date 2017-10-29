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
package com.sri.ai.praise.inference;

import java.util.StringJoiner;

import org.antlr.v4.runtime.RecognitionException;
import org.apache.commons.lang3.exception.ExceptionUtils;

import com.google.common.annotations.Beta;

@Beta
public class HOGMQueryError {
	
	public enum Context {
		MODEL, QUERY, UNKNOWN
	}
	
	private Context   context         = Context.UNKNOWN;
	private int       line            = -1;
	private LinePortion   portion;
	private String    errorMessage    = "";
	private Throwable throwable       = null;
	
	public HOGMQueryError(Throwable t) {
		this.errorMessage = t.getMessage() == null ? t.toString() : t.getMessage();
		this.throwable    = t;
	}
	
	public HOGMQueryError(Context context, RecognitionException recognitionException) {
		this(context, recognitionException.getMessage(), recognitionException.getOffendingToken().getLine(), new LinePortion(recognitionException));
	}

	public HOGMQueryError(Context context, String errorMessage) {
		this(context, errorMessage, 0, new LinePortion(), null);
	}
	
	public HOGMQueryError(Context context, String errorMessage, int line, LinePortion linePortion) {
		this(context, errorMessage, line, linePortion, null);
	}
	
	public HOGMQueryError(Context context, String errorMessage, int line, LinePortion linePortion, Throwable t) {
		if (context == Context.UNKNOWN) {
			throw new IllegalArgumentException("Context cannot be set to UNKNOWN when providing context start and end infofrmation.");
		}
		this.context         = context;
		this.line            = line;
		this.portion         = linePortion;
		this.errorMessage    = errorMessage;
		this.throwable       = t;
	}
	
	public Context getContext() {
		return context;
	}
	
	public int getStartContextIndex() {
		return portion.start;
	}
	
	public int getEndContextIndex() {
		return portion.end;
	}
	
	public String getErrorMessage() {
		return errorMessage;
	}
	
	public Throwable getThrowable() {
		return throwable;
	}
	
	@Override
	public String toString() {
		StringJoiner sj = new StringJoiner("");
		
		if (context == Context.UNKNOWN) {
			sj.add("General Error: ");
		}
		else if (context == Context.QUERY) {
			sj.add("Error in Query ");
		}
		else if (context == Context.MODEL) {
			sj.add("Error in Model ");
		}
		
		if (context != Context.UNKNOWN) {
			sj.add("at Line " + line + ": ");
		}
		sj.add(errorMessage);
		
		if (throwable != null) {			
			sj.add("\n");
			sj.add(ExceptionUtils.getStackTrace(throwable));
		}
		
		return sj.toString();
	}
}
