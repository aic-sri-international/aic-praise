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
package com.sri.ai.test.praise.result;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultCompoundSyntaxTree;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.example.TrivialEpidemicSickbob;
import com.sri.ai.praise.result.ResultConverter;
import com.sri.ai.test.praise.AbstractLPITest;

public class ResultConverterTest extends AbstractLPITest {

	@Test
	public void simpleTest() {
		
		RewritingProcess process = newRewritingProcess(Expressions.TRUE);
		Model model =  new TrivialEpidemicSickbob();
		Expression modelExpression = parse(model.getModelDeclaration());
		Model.setRewritingProcessesModel(modelExpression, model.getKnownRandomVariableNames(), process);
		
		DefaultCompoundSyntaxTree input = new DefaultCompoundSyntaxTree("if . then . else .", 
				new DefaultCompoundSyntaxTree("sick", "bob"), 
				DefaultSymbol.createSymbol("0.3"), 
				DefaultSymbol.createSymbol("0.7"));
		
		DefaultCompoundSyntaxTree desiredOutput = new DefaultCompoundSyntaxTree("atomic rule",
				new DefaultCompoundSyntaxTree("sick", "bob"),
				DefaultSymbol.createSymbol("0.3"));
		
		Expression computedOutput = ResultConverter.potentialExpressionToRule(input, process);
				
		assertEquals(desiredOutput, computedOutput);
	}
	
	@Test
	public void nestedTest() {
		
		RewritingProcess process = newRewritingProcess(Expressions.TRUE);
		Model model =  new TrivialEpidemicSickbob();
		Expression modelExpression = parse(model.getModelDeclaration());
		Model.setRewritingProcessesModel(modelExpression, model.getKnownRandomVariableNames(), process);
		
		//if X = bob then if sick(bob) then 0.7 else 0.3 else if sick(X) then 0.2 else 0.8
		//converts to
		//if X = bob then sick(bob) 0.7 else sick(X) 0.2
		
		DefaultCompoundSyntaxTree input = new DefaultCompoundSyntaxTree("if . then . else .", 
				new DefaultCompoundSyntaxTree("=", "X", "bob"), 
				new DefaultCompoundSyntaxTree("if . then . else .",
						new DefaultCompoundSyntaxTree("sick", "bob"), 
						DefaultSymbol.createSymbol("0.7"), 
						DefaultSymbol.createSymbol("0.3")),
				new DefaultCompoundSyntaxTree("if . then . else .",
						new DefaultCompoundSyntaxTree("sick", "X"), 
						DefaultSymbol.createSymbol("0.2"), 
						DefaultSymbol.createSymbol("0.8")));		

		DefaultCompoundSyntaxTree desiredOutput = new DefaultCompoundSyntaxTree("conditional rule",
				new DefaultCompoundSyntaxTree("=", "X", "bob"),
				new DefaultCompoundSyntaxTree("atomic rule",
						new DefaultCompoundSyntaxTree("sick", "bob"),
						DefaultSymbol.createSymbol("0.7")),
				new DefaultCompoundSyntaxTree("atomic rule",
						new DefaultCompoundSyntaxTree("sick", "X"),
						DefaultSymbol.createSymbol("0.2")));
		
		Expression computedOutput = ResultConverter.potentialExpressionToRule(input, process);
		assertEquals(desiredOutput, computedOutput);	
	}
	
}
