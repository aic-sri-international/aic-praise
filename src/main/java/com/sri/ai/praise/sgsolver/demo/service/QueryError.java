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
package com.sri.ai.praise.sgsolver.demo.service;

import java.util.StringJoiner;

import com.google.common.annotations.Beta;

@Beta
public class QueryError {
	public enum Context {
		MODEL, QUERY, UNKNOWN
	}
	
	private Context   context         = Context.UNKNOWN;
	private int       startContextIdx = -1;
	private int       endContextIdx   = -1;
	private String    errorMessage    = "";
	private Throwable throwable       = null;
	
	public QueryError(Throwable t) {
		this.errorMessage = t.getMessage();
		this.throwable    = t;
	}
	
	public QueryError(Context context, String errorMessage, int startContextIdx, int endContextIdx) {
		this(context, errorMessage, startContextIdx, endContextIdx, null);
	}
	
	public QueryError(Context context, String errorMessage, int startContextIdx, int endContextIdx, Throwable t) {
		if (context == Context.UNKNOWN) {
			throw new IllegalArgumentException("Context cannot be set to UNKNOWN when providing context start and end infofrmation.");
		}
		if (startContextIdx < 0 || endContextIdx < 0 || endContextIdx < startContextIdx) {
			throw new IllegalArgumentException("Start and End context information is invalid: start="+startContextIdx+", end="+endContextIdx);
		}
		this.context         = context;
		this.startContextIdx = startContextIdx;
		this.endContextIdx   = endContextIdx;
		this.errorMessage    = errorMessage;
	}
	
	public Context getContext() {
		return context;
	}
	
	public int getStartContextIndex() {
		return startContextIdx;
	}
	
	public int getEndContextIndex() {
		return endContextIdx;
	}
	
	public String getErrorMessage() {
		return errorMessage;
	}
	
	public Throwable getThrowable() {
		return throwable;
	}
	
	@Override
	public String toString() {
		StringJoiner sj = new StringJoiner("\n");
		
		sj.add("Context: "+context+(context == Context.UNKNOWN ? "" : "@"+startContextIdx+":"+endContextIdx));
		sj.add("Message: "+errorMessage);
		
		return sj.toString();
	}
}
