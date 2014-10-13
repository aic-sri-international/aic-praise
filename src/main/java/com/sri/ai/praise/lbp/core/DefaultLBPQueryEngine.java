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
package com.sri.ai.praise.lbp.core;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

import org.slf4j.ILoggerFactory;
import org.slf4j.LoggerFactory;
import org.slf4j.Marker;
import org.slf4j.profiler.StopWatch;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;
import ch.qos.logback.core.filter.Filter;
import ch.qos.logback.core.spi.FilterReply;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Parser;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.helper.RewriterLogging;
import com.sri.ai.grinder.helper.RewriterLoggingNamedRewriterFilter;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderParserWrapper;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.PRAiSEConfiguration;
import com.sri.ai.praise.lbp.LBPConfiguration;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.lbp.LBPQueryEngine;
import com.sri.ai.praise.lbp.LBPQueryEngine.QueryError.TYPE;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.Model.ModelException;
import com.sri.ai.praise.model.ParfactorsDeclaration;
import com.sri.ai.util.Configuration;
import com.sri.ai.util.log.LogX;

/**
 * Default implementation of {@link LBPQueryEngine} interface.
 * 
 * @author oreilly
 *
 */
@Beta
public class DefaultLBPQueryEngine implements LBPQueryEngine {
	private static final String STEP_1 = "Step 1 get a query as expression";
	private static final String STEP_2 = "Step 2 create RewritingProcess";
	private static final String STEP_3 = "Step 3 get the model as Expression";
	private static final String STEP_4 = "Step 4 set RewritingProcessModel";
	private static final String STEP_5 = "Step 5 extend model with evidence";
	private static final String STEP_6 = "Step 6 create new Belief expression from 1 and 2 using step 4";
	private static final String STEP_7 = "Step 7 set precision on result of step 6";
	//
	private static LoggerContext _loggerContext             = null;
	private static Level         _traceInitialLevel         = null;
	private static Level         _justificationInitialLevel = null;
	{
		while (_loggerContext == null) {
			ILoggerFactory lf = LoggerFactory.getILoggerFactory();
			if (lf instanceof LoggerContext) {
				_loggerContext = (LoggerContext) lf;
			} 
			else {
				try {
					Thread.sleep(100);
				} catch (Exception ex) {
					
				}
			}
		}
		_traceInitialLevel         = _loggerContext.getLogger(Trace.getDefaultLoggerName()).getLevel();
		_justificationInitialLevel = _loggerContext.getLogger(Justification.getDefaultLoggerName()).getLevel();
	}
	private static AtomicInteger _numberTraceListeners         = new AtomicInteger(0);
	private static AtomicInteger _numberJustificationListeners = new AtomicInteger(0);
	//
	
	private List<QueryStepListener>     queryStepListeners     = new ArrayList<QueryStepListener>();
	private List<TraceListener>         traceListeners         = new ArrayList<TraceListener>();
	private List<JustificationListener> justificationListeners = new ArrayList<JustificationListener>();
	private Map<String, QueryOptions>   openQueryUUIDs         = new LinkedHashMap<String, QueryOptions>(); 
	private Map<String, RunLBPQuery>    activeQueries          = new LinkedHashMap<String, RunLBPQuery>();

	public DefaultLBPQueryEngine() {
	}
	
	//
	// START-LBPQueryEngine
	@Override
	public void addQueryStepListener(QueryStepListener listener) {
		synchronized (queryStepListeners) {
			if (!this.queryStepListeners.contains(listener)) {
				queryStepListeners.add(listener);
			}
		}
	}
	
	@Override
	public void removeQueryStepListener(QueryStepListener listener) {
		synchronized (queryStepListeners) {
			queryStepListeners.remove(listener);
		}
	}
	
	@Override
	public void addTraceListener(TraceListener listener) {
		synchronized (traceListeners) {
			if (!this.traceListeners.contains(listener)) {
				traceListeners.add(listener);
				if (_numberTraceListeners.incrementAndGet() == 1) {
					_loggerContext.getLogger(Trace.getDefaultLoggerName()).setLevel(Level.TRACE);
				}
			}
		}
	}
	
	@Override
	public void removeTraceListener(TraceListener listener) {
		synchronized (traceListeners) {
			if (traceListeners.remove(listener)) {
				if (_numberTraceListeners.decrementAndGet() == 0) {
					_loggerContext.getLogger(Trace.getDefaultLoggerName()).setLevel(_traceInitialLevel);
				}
			}
		}
	}
	
	@Override
	public void addJustificationListener(JustificationListener listener) {
		synchronized (justificationListeners) {
			if (!justificationListeners.contains(listener)) {
				justificationListeners.add(listener);
				if (_numberJustificationListeners.incrementAndGet() == 1) {
					_loggerContext.getLogger(Justification.getDefaultLoggerName()).setLevel(Level.TRACE);
				}
			}
		}
	}
	
	@Override
	public void removeJustificationListener(JustificationListener listener) {
		synchronized (justificationListeners) {
			if (justificationListeners.remove(listener)) {
				if (_numberJustificationListeners.decrementAndGet() == 0) {
					_loggerContext.getLogger(Justification.getDefaultLoggerName()).setLevel(_justificationInitialLevel);
				}
			}
		}
	}
	
	@Override
	public String newQueryUUID() {
		return newQueryUUID(new QueryOptions());
	}
	
	@Override
	public String newQueryUUID(QueryOptions options) {
		String queryUUID = UUID.randomUUID().toString();
		synchronized (openQueryUUIDs) {
			openQueryUUIDs.put(queryUUID, options);
		}
		return queryUUID;
	}
	
	@Override
	public String queryBeliefOfRandomVariable(String queryUUID, 
			String beliefQuery, String modelDeclaration) {
		return queryBeliefOfRandomVariable(queryUUID, beliefQuery, modelDeclaration, null);
	}
	
	@Override
	public String queryBeliefOfRandomVariable(String queryUUID, String beliefQuery,
			String modelDeclaration, String evidenceDeclaration) {
		RunLBPQuery queryRun = runQueryBeliefOfRandomVariable(queryUUID, beliefQuery, modelDeclaration, evidenceDeclaration);
		return queryRun.getResultAsString();
	}
	
	@Override
	public boolean stopQuery(String queryUUID) {
		boolean result = false;
		
		RunLBPQuery activeQuery = activeQueries.remove(queryUUID);
		if (activeQuery != null) {
			activeQuery.interrupt();
			result = true;
		}
		
		return result;
	}
	
	// END-LBPQueryEngine
	//
	
	//
	// PROTECTED METHODS
	//
	protected RunLBPQuery runQueryBeliefOfRandomVariable(String queryUUID, String beliefQuery,
			String modelDeclaration, String evidenceDeclaration) {
		QueryOptions options = null;
		// Validate the query UUID
		synchronized (openQueryUUIDs) {
			options = openQueryUUIDs.remove(queryUUID);
			if (options == null) {
				// Not a valid open query UUID
				throw new IllegalArgumentException("queryUUID was not generated via newQueryUUID() or has been used more than once: "+queryUUID);
			}
		}
		
		RunLBPQuery query  = new RunLBPQuery(queryUUID, options, beliefQuery, modelDeclaration, evidenceDeclaration);
		activeQueries.put(queryUUID, query);
		try {
			Thread queryThread = new Thread(query);
			Configuration.inheritConfiguration(Thread.currentThread(), queryThread);
			// Start and then wait for the query to complete
			// this ensure all trace and justification output
			// can be associated with a specific query.
			queryThread.start();
			queryThread.join();
		} catch (InterruptedException ie) {
			query.addError(new QueryError(TYPE.UNEXPECTED_PROCESSING_ERROR, "Query Thread Interruped"), ie);
		} 
		finally {
			activeQueries.remove(queryUUID);
		}
		
		if (query.getQueryErrors().size() > 0) {
			if (query.getOptionalErrorCause() != null) {
				throw new QueryException("Errors Processing Query", query.getOptionalErrorCause(), query.getQueryErrors());
			} 
			else {
				throw new QueryException("Errors Processing Query", query.getQueryErrors());
			}
		}
		return query;
	}
	
	protected class RunLBPQuery implements Runnable {
		private RewritingProcess  process				= null;
		private Parser            parser                = new AntlrGrinderParserWrapper();
		private String            queryUUID             = null;
		private QueryOptions      options               = null;
		private String            beliefQuery           = null;
		private String            modelDeclaration      = null;
		private String            evidenceDeclaration   = null;
		private Expression        resultAsExpression    = null;
		private String            resultAsString        = null;
		private StopWatch         stopWatch             = new StopWatch("QuerySteps");
		private Map<String, Long> rewriterProfiledTimes = new LinkedHashMap<String, Long>();
		private List<QueryError>  queryErrors           = new ArrayList<QueryError>();
		private Throwable         cause                 = null;
		private boolean           intentionallyStoped   = false;
		//
		private AppenderBase<ILoggingEvent> traceAppender         = null;
		private AppenderBase<ILoggingEvent> justificationAppender = null;
		private Filter<ILoggingEvent>       queryUUIDFilter       = null;
		
		public RunLBPQuery(String queryUUID, QueryOptions options, String beliefQuery, String modelDeclaration, String evidenceDeclaration) {
			this.queryUUID           = queryUUID;
			this.options             = options;
			this.beliefQuery         = beliefQuery;
			this.modelDeclaration    = modelDeclaration;
			this.evidenceDeclaration = evidenceDeclaration;
		}
		
		public String getResultAsString() {
			return resultAsString;
		}
		
		public Expression getResultAsExpression() {
			return resultAsExpression;
		}
		
		public void addError(QueryError error, Throwable cause) {
			queryErrors.add(error);
			if (null != cause) {
				this.cause = cause;
			}
		}
		
		public List<QueryError> getQueryErrors() {
			return queryErrors;
		}
		
		public Throwable getOptionalErrorCause() {
			return cause;
		}
		
		public void interrupt() {
			while (process == null) {
				if (queryErrors.size() > 0) {
					break;
				}
				try {
					Thread.sleep(100);
				} 
				catch (InterruptedException ie) {
					// ignore
				}
			}
			if (process != null) {
				intentionallyStoped = true;
				process.interrupt();
			}
		}
		
		@Override 
		public void run() {			
			// Setup the options for this query.
			Configuration.setProperty(PRAiSEConfiguration.KEY_MODEL_ALL_TYPE_SIZES_KNOWN, ""+options.isKnownTypeSizes());
			setupLogging();
						
			try {
				
				//
				// PERFORM THE QUERY STEPS
				
				// Step 1 get a query as expression
				notifyListenersQueryStepStarting(queryUUID, STEP_1, stopWatch);
				Expression queryExpression = null;
				queryExpression = parser.parse(beliefQuery);
				notifyListenersQueryStepComplete(queryUUID, STEP_1, stopWatch);
				
				// Step 2 create RewritingProcess
				notifyListenersQueryStepStarting(queryUUID, STEP_2, stopWatch);
				LBPConfiguration configuration = options.getLBPConfiguration();
				process = LBPFactory.newLBPProcess(queryExpression, configuration);
				notifyListenersQueryStepComplete(queryUUID, STEP_2, stopWatch);
				
				// Step 3 get the model as Expression
				notifyListenersQueryStepStarting(queryUUID, STEP_3, stopWatch);
				Expression modelDefinition = null;
				Model      model           = null;
				try {
					modelDefinition = parser.parse(modelDeclaration);
					
					model           = new Model(modelDefinition, new LinkedHashSet<String>());
				} catch (ModelException mex) {
					queryErrors.add(new QueryError(TYPE.INVALID_MODEL_DECLARATION, mex.getMessage()));
				}
				notifyListenersQueryStepComplete(queryUUID, STEP_3, stopWatch);
				
				if (model != null) {
					
					// Step 4 set RewritingProcessModel
					notifyListenersQueryStepStarting(queryUUID, STEP_4, stopWatch);
					process = Model.setRewritingProcessesModel(model.getModelDefinition(), model.getKnownRandomVariableNameAndArities(), process);
					try {
						// Can only validate here after setting up the model
						// and associating with the process.
						LPIUtil.assertBeliefOk(queryExpression, process);
					} catch (IllegalArgumentException iae) {
						queryErrors.add(new QueryError(TYPE.INVALID_QUERY, iae.getMessage()));
					}
					notifyListenersQueryStepComplete(queryUUID, STEP_4, stopWatch);
					
					if (queryErrors.size() == 0) {
						// Step 5 extend model with evidence
						notifyListenersQueryStepStarting(queryUUID, STEP_5, stopWatch);
						if (evidenceDeclaration != null) {
							try {
								Expression            evidenceDefinition = parser.parse(evidenceDeclaration);
								ParfactorsDeclaration evidenceParfactors = new ParfactorsDeclaration(evidenceDefinition);
								if (ParfactorsDeclaration.isEvidenceOnly(evidenceParfactors, process)) {
									List<Expression> extendedParfactors = new ArrayList<Expression>();
									extendedParfactors.addAll(model.getParfactorsDeclaration().getParfactors());
									extendedParfactors.addAll(evidenceParfactors.getParfactors());
									
									ParfactorsDeclaration extendedParfactorsDeclaration = 
											ParfactorsDeclaration.makeParfactorsDeclaration(extendedParfactors.toArray(new Expression[extendedParfactors.size()]));
									
									// Extend the model
									model = Model.constructFromParts(model.getName(), 
											model.getDescription(), 
											model.getSortDeclarations(), 
											model.getRandomVariableDeclarations(), 
											extendedParfactorsDeclaration, 
											model.getKnownRandomVariableNameAndArities());
									// Update the model associated to the process to be the extended version.
									process = Model.setRewritingProcessesModel(model.getModelDefinition(), model.getKnownRandomVariableNameAndArities(), process);
								} 
								else {
									queryErrors.add(new QueryError(TYPE.INVALID_EVIDENCE_DECLARATION,"Evidence Declaration does not contain only extensionally defined factors."));
								}
							} catch (IllegalArgumentException iae) {
								queryErrors.add(new QueryError(TYPE.INVALID_EVIDENCE_DECLARATION, iae.getMessage()));
							} catch (ModelException mex) {
								queryErrors.add(new QueryError(TYPE.INVALID_EVIDENCE_DECLARATION, mex.getMessage()));
							}
						}
						notifyListenersQueryStepComplete(queryUUID, STEP_5, stopWatch);
						
						if (queryErrors.size() == 0) {
							// Step 6 create new Belief expression from 1 and 2
							notifyListenersQueryStepStarting(queryUUID, STEP_6, stopWatch);
							// Extend the process by any logical variables in the query.
							process = LPIUtil.extendContextualSymbolsWithFreeVariablesInferringDomainsFromUsageInRandomVariables(queryExpression, process);
							Expression belief = process.rewrite(LBPRewriter.R_belief, queryExpression);
							notifyListenersQueryStepComplete(queryUUID, STEP_6, stopWatch, rewriterProfiledTimes);
							
							// Step 7 set precision on result of step 6
							notifyListenersQueryStepStarting(queryUUID, STEP_7, stopWatch);
							resultAsExpression = Expressions.roundToAGivenPrecision(belief, 
									PRAiSEConfiguration.getLBPQueryEngineRoundResultTo(), process);
							resultAsString = resultAsExpression.toString();
							notifyListenersQueryStepComplete(queryUUID, STEP_7, stopWatch);
						}
					}
				}
			} catch (Throwable t) {
				if (intentionallyStoped) {
					addError(new QueryError(TYPE.QUERY_INTENTIONALLY_STOPPED, "Query Intentionally Stopped."), null);
				} else {
					addError(new QueryError(TYPE.UNEXPECTED_PROCESSING_ERROR, t.getMessage()), t);
				}
			}
			
			// Cleanup the Trace and Justification related to this query
			cleanupLogging();
		}
		
		private void setupLogging() {
			// Set up the relevant trace and query information
			// specific for this query.
			RewriterLogging.setUUID(queryUUID);
			
			traceAppender = new AppenderBase<ILoggingEvent>() {
				@Override
				protected void append(ILoggingEvent eventObject) {
					int      level           = LogX.getTraceLevel(eventObject.getLoggerName());
					Long     rootProfileInfo = LogX.getRootProfileInfo(eventObject.getLoggerName());
					Long     profileInfo     = LogX.getProfileInfo(eventObject.getLoggerName());
					Marker   marker          = eventObject.getMarker();
					String   msg             = eventObject.getFormattedMessage();
					Object[] args            = eventObject.getArgumentArray(); 
					for (TraceListener traceListener : traceListeners) {
						traceListener.traceEvent(queryUUID, level, rootProfileInfo, profileInfo, marker, msg, args);
					}
					
					if (RewriterLogging.REWRITER_PROFILE_INFO.equals(eventObject.getMarker())) {
						if (profileInfo != null) {
							String rewriterName = RewriterLogging.getCurrentRewriterName();
							Long currentProfileInfo = rewriterProfiledTimes.get(rewriterName);
							if (currentProfileInfo != null) {
								currentProfileInfo += profileInfo;
							} 
							else {
								currentProfileInfo = profileInfo;
							}
							rewriterProfiledTimes.put(rewriterName, currentProfileInfo);
						}
					}
				}
			};
			traceAppender.addFilter(new RewriterLoggingNamedRewriterFilter());
			traceAppender.setContext(_loggerContext);
			
			justificationAppender = new AppenderBase<ILoggingEvent>() {
				@Override
				protected void append(ILoggingEvent eventObject) {
					int      level       = LogX.getTraceLevel(eventObject.getLoggerName());
					Marker   marker      = eventObject.getMarker();
					String   msg         = eventObject.getFormattedMessage();
					Object[] args        = eventObject.getArgumentArray(); 
					for (JustificationListener justificationListener : justificationListeners) {
						justificationListener.justificationEvent(queryUUID, level, marker, msg, args);
					}
				}
			};
			justificationAppender.addFilter(new RewriterLoggingNamedRewriterFilter());
			justificationAppender.setContext(_loggerContext);
			
			queryUUIDFilter = new Filter<ILoggingEvent>() {
				@Override
				public FilterReply decide(ILoggingEvent event) {
					String eventUUID = event.getMDCPropertyMap().get(RewriterLogging.getMDCUUIDKey());
					if (queryUUID.equals(eventUUID)) {
						if (!options.isTraceOn() &&
							event.getLoggerName().equals("com.sri.ai.grinder.helper.Trace")) {
							return FilterReply.DENY;
						}
						if (!options.isJustificationsOn() &&
							event.getLoggerName().equals("com.sri.ai.grinder.helper.Justification")) {
							return FilterReply.DENY;
						}
						return FilterReply.ACCEPT;
					}
					return FilterReply.DENY;
				}
			};
			queryUUIDFilter.setContext(_loggerContext);
			
			traceAppender.addFilter(queryUUIDFilter);
			justificationAppender.addFilter(queryUUIDFilter);
			
			queryUUIDFilter.start();
			traceAppender.start();
			justificationAppender.start();
			
			_loggerContext.getLogger(Trace.getDefaultLoggerName()).addAppender(traceAppender);
			_loggerContext.getLogger(Justification.getDefaultLoggerName()).addAppender(justificationAppender);
		}
		
		private void cleanupLogging() {
			queryUUIDFilter.stop();
			traceAppender.stop();
			justificationAppender.stop();
			_loggerContext.getLogger(Trace.getDefaultLoggerName()).detachAppender(traceAppender);
			_loggerContext.getLogger(Justification.getDefaultLoggerName()).detachAppender(justificationAppender);
		}
		
		/**
		 * Get the rewriting process used during this query evaluation
		 * @return
		 */
		public RewritingProcess getProcess() {
			return process;
		}
		
	}
	
	private void notifyListenersQueryStepStarting(String queryUUID, String step, StopWatch stopWatch) {
		synchronized (queryStepListeners) {
			for (QueryStepListener l : queryStepListeners) {
				l.queryStepStarting(queryUUID, step);
			}
		}
		// Start the stopwatch after all the listeners have been notified
		stopWatch.start(step);
	}
	
	private void notifyListenersQueryStepComplete(String queryUUID, String step, StopWatch stopWatch) {
		notifyListenersQueryStepComplete(queryUUID, step, stopWatch, null);
	}
	
	private void notifyListenersQueryStepComplete(String queryUUID, String step, StopWatch stopWatch, Map<String, Long> rewriterProfiledTimes) {
		// Create the query step and stop the stopwatch associated with the step.
		QueryStep completedStep = null;
		if (rewriterProfiledTimes == null) {
			completedStep = new QueryStep(step, stopWatch.stop().elapsedTime());
		} 
		else {
			List<QueryStep> subSteps = new ArrayList<QueryStep>();
			for (Map.Entry<String, Long> rewriterProfileInfo : rewriterProfiledTimes.entrySet()) {
				subSteps.add(new QueryStep(rewriterProfileInfo.getKey(), rewriterProfileInfo.getValue()));
			}
			completedStep = new QueryStep(step, stopWatch.stop().elapsedTime(), subSteps);
		}
		synchronized (queryStepListeners) {
			for (QueryStepListener l : queryStepListeners) {
				l.queryStepComplete(queryUUID, completedStep);
			}
		}
	}
	
}
