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
package com.sri.ai.test.praise;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.junit.Before;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Parser;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderParserWrapper;
import com.sri.ai.grinder.ui.TreeUtil;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.model.Model;
import com.sri.ai.util.Configuration;
import com.sri.ai.util.Util;
import com.sri.ai.util.concurrent.BranchAndMerge;

public abstract class AbstractLPITest {
	
	private Parser parser;
	
	public AbstractLPITest() {
		super();
	}

	@Before
	public void setUp() {
		newRewritingProcess(null);
		// create new process so that all newly constructed expressions are interpreted according to this process's modules,
		// particularly order normalization.

		TreeUtil.flushData();
		Configuration.clear();
		SyntaxTrees.flushGlobalSymbolTable();
		BranchAndMerge.reset();
		
		parser = makeParser();
	}
	
	public void tearDown() {
		parser.close();
		Configuration.clear();
	}
	
	public Parser makeParser() {
		return new AntlrGrinderParserWrapper();
	}
	
	public RewritingProcess newRewritingProcess(Expression rootExpression) {
		return LBPFactory.newLBPProcess(rootExpression);
	}
	
	//
	// PROTECTED METHODS
	//
	protected void perform(TestData[] tests) {
		String assertFailed = null;
		int run = 0;
		for (int i = 0; i < tests.length; i++) {
			String errorMessage = tests[i].perform(i);
			if (errorMessage != null) {
				if (GrinderConfiguration.isWaitUntilUIClosedEnabled()) {
					assertFailed = errorMessage;
					System.err.println(assertFailed);
					break;
				}
				else {
					fail(errorMessage);
				}
			}
			run++;
		}

		if (!GrinderConfiguration.isWaitUntilUIClosedEnabled()) {
			assertEquals("Ensure you run all the tests", tests.length, run);
		}
		
		GrinderUtil.doTreeUtilWaitUntilClosed();
		
		if (assertFailed != null) {
			fail(assertFailed);
		}
	}
	
	/**
	 * @deprecated Use {@link LPIUtil#doTreeUtilWaitUntilClosed()} instead
	 */
	@Deprecated
	public static void doTreeUtilWaitUnilClosed() {
		GrinderUtil.doTreeUtilWaitUntilClosed();
	}
	
	protected Expression parse(String expressionString) {
		return parser.parse(expressionString);
	}
	
	//
	// INNER CLASSES
	//
	protected abstract class TestData  {
		public boolean isIllegalArgumentTest;
		public String expected;		
		public String contextualConstraintString;
		public Model  model;
		
		public TestData(boolean isIllegalArgumentTest, String expected) {
			this.isIllegalArgumentTest = isIllegalArgumentTest;
			this.expected              = expected;
		}
		
		public TestData(String contextualConstraintString, Model model, boolean isIllegalArgumentTest, String expected) {
			this(isIllegalArgumentTest, expected);
			this.contextualConstraintString = contextualConstraintString;
			this.model                      = model;
		}
		
		protected Expression parse(String expressionString) {
			AntlrGrinderParserWrapper parser = model == null? new AntlrGrinderParserWrapper() : new AntlrGrinderParserWrapper(model.getRandomPredicatesSignatures());
			return parser.parse(expressionString);
		}

		/** Performs i-th test of a batch, indicating an error message in case of failure, or null. */
		public String perform(int i) {
			Expression topExpression;
			RewritingProcess process;
			Expression actual = null;

//			topExpression = getTopExpression();  // moved to *after* model is introduced to process, because new non-syntax-based expression implementations era eager
			// with analyzing sub-expressions, and bracketed expressions need the model to do so.
//			process = newRewritingProcess(topExpression);
			
			process = newRewritingProcess(null);
			
			if (model != null) {
				process = model.setRewritingProcessesModel(process);
				process = LPIUtil.extendContextualSymbolsWithFreeVariablesInferringDomainsFromUsageInRandomVariables(Tuple.make(Model.getParfactors(process)), process);
				// parfactors don't typically have free variables, but they might.
			}

			topExpression = getTopExpression();
			process.setRootExpression(topExpression);
			
			Expression contextualConstraint = parse(contextualConstraintString);
			Expression topExpressionAndContextualConstraint = Tuple.make(topExpression, contextualConstraint);
			process =
					GrinderUtil.extendContextualSymbolsWithFreeSymbolsInExpressionwithUnknownTypeForSetUpPurposesOnly(
							topExpressionAndContextualConstraint, process);
			process = GrinderUtil.extendContextualConstraint(contextualConstraint, process);
			
			Expression expectedExpression = parse(expected);
			if (isIllegalArgumentTest) {
				try {
					actual = callRewrite(process);
					fail("tests[i]=" + i + ", " + topExpression + " should have thrown an IllegalArgumentException.");
				} catch (IllegalArgumentException iae) {
					// ok this is expected
					Trace.setTraceLevel(1);
					Trace.log("-R_: expected IllegalArgumentException thrown:"+iae.getMessage());
					Trace.setTraceLevel(0);
					Justification.setJustificationLevel(1);
					Justification.getDefaultLogX().trace("-J_: expected IllegalArgumentException thrown:"+iae.getMessage());
					Justification.setJustificationLevel(0);
				}
			} 
			else {
				long startTime = System.currentTimeMillis();
				Throwable thrown = null;
				try {
					actual = callRewrite(process);
				} catch (Throwable t) {
					thrown = t;
				}
				long rewroteIn = System.currentTimeMillis()-startTime;
				System.out.println("tests["+ i +" rewrote in "+(rewroteIn)+"ms]:" + topExpression + "\n--->\n" + actual);
				// Ensure all notifications and cache output are noted if configured that way.
				process.notifyEndOfRewritingProcess();
				if (thrown != null) {
					String errorMessage = "ERROR tests[" + i + "] = " + topExpression 
							+ "\nexpected: " + expectedExpression // better to show expectedExpression than expression, because parsing errors will be more clear
							+ "\n but an unhandled throwable exception was thrown instead: " + (thrown.getMessage() == null? (thrown.getClass().getName() + "\n" + Util.join("\n", thrown.getStackTrace())) : thrown.getMessage());
					System.out.println(errorMessage);
					throw new Error(thrown);
				}
				else if ( ! Util.equals(actual, expectedExpression)) {
					String errorMessage = "ERROR tests[" + i + "] = " + topExpression 
					+ "\nexpected: " + expectedExpression // better to show expectedExpression than expression, because parsing errors will be more clear
					+ "\nbut was: " + actual;
					System.out.println(errorMessage);
					return errorMessage; // indicates that we need to break test loop
				}
			}
			return null; // indicates that we can proceed with testing
		}

		public abstract Expression getTopExpression();
		
		public abstract Expression callRewrite(RewritingProcess process);
	}
}
