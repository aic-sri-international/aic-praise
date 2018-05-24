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
package com.sri.ai.praise.application.empiricalevaluationapplication.election;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;

import java.io.IOException;

import com.sri.ai.praise.application.empiricalevaluationapplication.core.AbstractEvaluationApplication;
import com.sri.ai.praise.application.empiricalevaluationapplication.core.EvaluationConfigurationFromCommandLineOptions;
import com.sri.ai.praise.language.ModelLanguage;
import com.sri.ai.praise.modelscontainer.ModelPage;
import com.sri.ai.praise.modelscontainer.PagedModelContainer;
import com.sri.ai.util.collect.IntegerIterator;


/**
 * Command line interface for evaluating an election model as submitted to IJCAI 2016.
 * 
 * @author braz
 *
 */
public class EvaluationApplicationOnElectionModelWithMultipleDomainSizes extends AbstractEvaluationApplication {
	
	protected PagedModelContainer getModelsContainer(EvaluationConfigurationFromCommandLineOptions evaluationConfiguration) throws IOException {
		return 
				new PagedModelContainer(
						"election", 
						mapIntoList(new IntegerIterator(1, 4), (Integer i) -> 
						new ModelPage(
								ModelLanguage.HOGMv1,
								"election (multiplier " + i + ")",
								modelStringForDomainSize(500, i),
								list("likeIncumbent > likeChallenger"))));
	}

	private String modelStringForDomainSize(int domainSize, int multiplier) {
		domainSize *= multiplier;
		return "random terrorAttacks : 0..20;\r\n" + 
		"random newJobs : 0..100; // 100K\r\n" + 
		"random dow: 110..180;\r\n" + 
		"random economyIsPoor : Boolean;\r\n" + 
		"random economyIsGreat : Boolean;\r\n" + 
		"random attackPerception: Boolean;\r\n" + 
		"random likeIncumbent  : 0.." + domainSize + "; // 100M\r\n" + 
		"random likeChallenger : 0.." + domainSize + "; // 100M\r\n" + 
		"\r\n" + 
		"// P(terrorAttacks) =\r\n" + 
		"if terrorAttacks = 0 then 1/21 else 1/21; // uniform\r\n" + 
		"\r\n" + 
		"terrorAttacks = 1;\r\n" + 
		"dow = 170;\r\n" + 
		"// newJobs = 1;\r\n" + 
		"\r\n" + 
		"// P(newJobs) =\r\n" + 
		"if newJobs = 0 then 1/101 else 1/101; // uniform\r\n" + 
		"\r\n" + 
		"// P(dow) =\r\n" + 
		"if dow = 0 then 1/(180 - 110 + 1) else 1/(180 - 110 + 1); // uniform\r\n" + 
		"\r\n" + 
		"economyIsPoor <=> dow < 130 or newJobs < 30;\r\n" + 
		"\r\n" + 
		"economyIsGreat <=> dow > 160 or newJobs > 70;\r\n" + 
		"\r\n" + 
		"attackPerception <=> terrorAttacks > 4;\r\n" + 
		"\r\n" + 
		"// P(likeIncumbent) = \r\n" + 
		"if dow > 160 or newJobs > 70\r\n" + 
		"   then if likeIncumbent > " + new Double(0.7*domainSize).intValue() + " then 0.9/(" + domainSize + " + 1) else 0.1/(" + domainSize + " + 1)\r\n" + 
		"else\r\n" + 
		"if dow < 130 or newJobs < 30\r\n" + 
		"   then if likeIncumbent < " + new Double(0.5*domainSize).intValue() + " then 0.8/(" + domainSize + " + 1) else 0.2/(" + domainSize + " + 1)\r\n" + 
		"   else if terrorAttacks > 4\r\n" + 
		"        then if likeIncumbent < " + new Double(0.6*domainSize).intValue() + " then 0.9/(" + domainSize + " + 1) else 0.1/(" + domainSize + " + 1)\r\n" + 
		"        else if likeIncumbent = 0  then 1/(" + domainSize + " + 1) else 1/(" + domainSize + " + 1); // uniform\r\n" + 
		"\r\n" + 
		"// P(likeChallenger) =\r\n" + 
		"if likeChallenger = 0 then 1/(" + domainSize + " + 1) else 1/(" + domainSize + " + 1);";
	}

	public static void main(String[] args) throws Exception {
		EvaluationApplicationOnElectionModelWithMultipleDomainSizes evaluator = new EvaluationApplicationOnElectionModelWithMultipleDomainSizes();
		evaluator.run(args);
	}
}