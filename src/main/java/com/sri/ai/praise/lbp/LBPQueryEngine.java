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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.slf4j.Marker;

import com.google.common.annotations.Beta;

/**
 * Utility interface that encapsulates the logic required to call
 * 'R_belief(belief(V))'. The interface is designed around using the string
 * representation of expressions and model and evidence declarations so that its
 * easier to integrate with non-expression aware code (e.g. a front end).
 * 
 * @author oreilly
 * 
 */
@Beta
public interface LBPQueryEngine {
	
	/**
	 * 
	 */
	public class QueryOptions implements Serializable {
		//
		private static final long serialVersionUID = 1L;
		//
		private boolean knownTypeSizes = true;
		private boolean traceOn          = true;
		private boolean justificationsOn = true;
		//
		private LBPConfiguration lbpConfiguration = LBPFactory.newLBPConfiguration();
		
		public QueryOptions() {
			
		}

		public QueryOptions(boolean knownTypeSizes, boolean traceOn, boolean justificationsOn) {
			setKnownTypeSizes(knownTypeSizes);
			setTraceOn(traceOn);
			setJustificationsOn(justificationsOn);
		}
		
		public boolean isKnownTypeSizes() {
			return knownTypeSizes;
		}

		public void setKnownTypeSizes(boolean knownTypeSizes) {
			this.knownTypeSizes = knownTypeSizes;
		}

		public boolean isTraceOn() {
			return traceOn;
		}

		public void setTraceOn(boolean traceOn) {
			this.traceOn = traceOn;
		}

		public boolean isJustificationsOn() {
			return justificationsOn;
		}

		public void setJustificationsOn(boolean justificationsOn) {
			this.justificationsOn = justificationsOn;
		}
		
		public LBPConfiguration getLBPConfiguration() {
			return lbpConfiguration;
		}
		
		public void setLBPConfiguration(LBPConfiguration lbpConfiguration) {
			this.lbpConfiguration = lbpConfiguration;
		}
	}

	/**
	 * Utility class for representing the set of errors that can be generated
	 * when calling the LBPQueryEngine.
	 * 
	 * @author oreilly
	 * 
	 */
	public class QueryError implements Serializable {
		public static enum TYPE {
			INVALID_QUERY, INVALID_MODEL_DECLARATION, INVALID_EVIDENCE_DECLARATION, QUERY_INTENTIONALLY_STOPPED, UNEXPECTED_PROCESSING_ERROR
		};

		//
		private static final long serialVersionUID = 1L;
		//
		private TYPE errorType = null;
		private String description = null;

		public QueryError(TYPE errorType, String description) {
			this.errorType = errorType;
			this.description = description;
		}

		public TYPE getErrorType() {
			return errorType;
		}

		public String getDescription() {
			return description;
		}
		
		@Override
		public String toString() {
			return errorType.name() + ": " + description;
		}
	}

	/**
	 * Utility class for encapsulating a list of Query Errors that may be
	 * generated when calling the LBPQueryEngine.
	 * 
	 * @author oreilly
	 * 
	 */
	public class QueryException extends RuntimeException {
		private static final long serialVersionUID = 1L;
		//
		private List<QueryError> errors = new ArrayList<QueryError>();

		public QueryException(String message, List<QueryError> errors) {
			super(message);
			this.errors.addAll(errors);
		}

		public QueryException(String message, Throwable cause,
				List<QueryError> errors) {
			super(message, cause);
			this.errors.addAll(errors);
		}

		public List<QueryError> getErrors() {
			return Collections.unmodifiableList(errors);
		}
	}
	
	/**
	 * Utility class detailing a step and the associated cost in nanoseconds
	 * associated with processing a query request.
	 * 
	 * @author oreilly
	 *
	 */
	public class QueryStep implements Serializable {
		private static final long serialVersionUID = 1L;
		//
		private String          description       = null;
		private Long            timeInNanoseconds = null;
		private List<QueryStep> subSteps          = new ArrayList<QueryStep>();
		
		public QueryStep(String description, Long timeInNanoseconds) {
			this.description       = description;
			this.timeInNanoseconds = timeInNanoseconds;
		}
		
		public QueryStep(String description, Long timeInNanoseconds, List<QueryStep> subSteps) {
			this.description       = description;
			this.timeInNanoseconds = timeInNanoseconds;
			this.subSteps.addAll(subSteps);
		}
		
		public String getDescription() {
			return description;
		}
		
		public Long getTimeInNanoseconds() {
			return timeInNanoseconds;
		}
		
		public boolean hasSubSteps() {
			return subSteps.size() > 0;
		}
		
		public List<QueryStep> getSubSteps() {
			return Collections.unmodifiableList(subSteps);
		}
	}
	
	/**
	 * An interface to be implemented when interested in receiving notifications
	 * that query steps have begun and ended.
	 * 
	 * @author oreilly
	 *
	 */
	public interface QueryStepListener {
		/**
		 * Called when a query step is started.<br>
		 * <br>
		 * Note: there can be multiple calls to this method before a
		 * corresponding queryStepComplete is called as query steps may be
		 * nested (i.e. QueryStep.getSubSteps()).
		 * 
		 * @param queryUUID
		 *            the queryUUID identifying the query this step is
		 *            associated with.
		 * @param description
		 *            a description of the query step that has been started.
		 * 
		 */
		void queryStepStarting(String queryUUID, String description);
		
		/**
		 * Called when a query step has completed.
		 * 
		 * @param queryUUID
		 *            the queryUUID identifying the query this step is
		 *            associated with.
		 * @param completedStep
		 *            the query step that has just completed.
		 */
		void queryStepComplete(String queryUUID, QueryStep completedStep);
	}
	
	
	/**
	 * Add a QueryStepListener to be notified when query steps occur.
	 * 
	 * @param listener
	 *            the QueryStepListener to be added.
	 */
	void addQueryStepListener(QueryStepListener listener);

	/**
	 * Remove a QueryStepListener.
	 * 
	 * @param listener
	 *            the QueryStepListener to be removed.
	 */
	void removeQueryStepListener(QueryStepListener listener);
	
	/**
	 * Interface to be implemented by objects interested in receiving trace
	 * event information associated with a query.
	 * 
	 * @author oreilly
	 * 
	 */
	public interface TraceListener {
		/**
		 * Called when a new Trace event that the listener is interested in is
		 * generated.
		 * 
		 * @param queryUUID
		 *            the queryUUID identifying the query the trace is
		 *            associated with.
		 * @param traceLevel
		 *            the current trace level.
		 * @param rootProfileInfo
		 *            an optional root profiler in nanoseconds associated with
		 *            the current trace message.
		 * @param profileInfo
		 *            an optional profiler in nanoseconds associated with the
		 *            current trace message.
		 * @param marker
		 *            an optional marker associated with the trace event.
		 * @param formattedMsg
		 *            a formatted version of the trace message with arguments
		 *            embedded (if applicable).
		 * @param args
		 *            the arguments passed to the trace call to be included in
		 *            the message to be formatted.
		 */
		void traceEvent(String queryUUID, int traceLevel, Long rootProfileInfo, Long profileInfo, Marker marker,
				String formattedMsg, Object... args);
	}
	
	/**
	 * Add a trace listener to receive query related trace information.
	 * 
	 * @param listener
	 *            the trace listener to be added.
	 */
	void addTraceListener(TraceListener listener);

	/**
	 * Remove a trace listener from receiving query related trace
	 * information.
	 * 
	 * @param listener
	 *            the trace listener to be removed.
	 */
	void removeTraceListener(TraceListener listener);
	
	/**
	 * Interface to be implemented by objects interested in receiving justification
	 * event information associated with a query.
	 * 
	 * @author oreilly
	 * 
	 */
	public interface JustificationListener {
		/**
		 * Called when a new Justification event that the listener is interested in is
		 * generated.
		 * 
		 * @param queryUUID
		 *            the queryUUID identifying the query the justification is
		 *            associated with.
		 * @param justificationLevel
		 *            the current trace level.
		 * @param marker
		 *            an optional marker associated with the justification event.
		 * @param formattedMsg
		 *            a formatted version of the justification message with arguments
		 *            embedded (if applicable).
		 * @param args
		 *            the arguments passed to the justification call to be included in
		 *            the message to be formatted.
		 */
		void justificationEvent(String queryUUID, int justificationLevel, Marker marker,
				String formattedMsg, Object... args);
	}
	
	/**
	 * Add a justification listener to receive query related justification information.
	 * 
	 * @param listener
	 *            the justification listener to be added.
	 */
	void addJustificationListener(JustificationListener listener);

	/**
	 * Remove a justification listener from receiving query related justification
	 * information.
	 * 
	 * @param listener
	 *            the justification listener to be removed.
	 */
	void removeJustificationListener(JustificationListener listener);
	
	/**
	 * 
	 * @return a new Universal Unique Identifier that can be used to perform a
	 *         single query.
	 */
	String newQueryUUID();
	
	/**
	 * 
	 * @param queryOptions
	 *        options to be associated with how the query is run.
	 * @return a new Universal Unique Identifier that can be used to perform a
	 *         single query.
	 */
	String newQueryUUID(QueryOptions queryOptions);

	/**
	 * Run Lifted Belief Propagation to calculate the belief of a random
	 * variable within the context of the specified model.
	 * 
	 * @param queryUUID
	 *            a queryUUID that must have been obtained by calling
	 *            newQueryUUID.
	 * @param beliefQuery
	 *            a String representation of a 'belief(V)' expression where V is
	 *            a random variable in the model provided.
	 * @param modelDeclaration
	 *            a String representation of the model declaration that the
	 *            query is to be run against.
	 * @return a String representation of an expression describing the
	 *         calculated belief (may be a numeric integer or a conditional
	 *         expression).
	 * @throws QueryException
	 *             if errors occur when processing the query.
	 */
	String queryBeliefOfRandomVariable(String queryUUID, String beliefQuery,
			String modelDeclaration);
	
	/**
	 * Run Lifted Belief Propagation to calculate the belief of a random
	 * variable within the context of the specified model.
	 * 
	 * @param queryUUID
	 *            a queryUUID that must have been obtained by calling
	 *            newQueryUUID.
	 * @param beliefQuery
	 *            a String representation of a 'belief(V)' expression where V is
	 *            a random variable in the model provided.
	 * @param modelDeclaration
	 *            a String representation of the model declaration that the
	 *            query is to be run against.
	 * @param evidenceDeclaration
	 *            a String representation of the evidence declaration that
	 *            the query is to be run against. This should be a parfactors
	 *            declaration containing only extensionally defined factors
	 *            (i.e. fully grounded, no logical variables should be 
	 *             present).
	 * @return a String representation of an expression describing the
	 *         calculated belief (may be a numeric integer or a conditional
	 *         expression).
	 * @throws QueryException
	 *             if errors occur when processing the query.
	 */
	String queryBeliefOfRandomVariable(String queryUUID, String beliefQuery,
			String modelDeclaration, String evidenceDeclaration);
	
	/**
	 * Stop a specified query.
	 * 
	 * @param queryUUID
	 *            a queryUUID that must be currently in use by one of the
	 *            query...() methods.
	 * @return true if the query was stopped successfully, false otherwise.
	 */
	boolean stopQuery(String queryUUID);
}
