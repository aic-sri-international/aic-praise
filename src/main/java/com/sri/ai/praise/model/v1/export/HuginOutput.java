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
package com.sri.ai.praise.model.v1.export;

import java.io.IOException;
import java.io.Writer;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.language.grounded.bayes.ConditionalProbabilityTable;
import com.sri.ai.praise.language.grounded.transform.XFormMarkovToBayes;

/**
 * Utility class for generating a Hugin dot net bayesian network output file based on a general purpose 
 * representation of bayesian networks.
 *  
 * @author oreilly
 *
 */
@Beta
public class HuginOutput implements XFormMarkovToBayes.BayesOutputListener {
	private Writer writer;
	private Map<Integer, String> varIdxToName;
	private Map<Integer, List<String>> varIdxToRangeValues;
	
	public HuginOutput(Writer writer, Map<Integer, String> varIdxToName, Map<Integer, List<String>> varIdxToRangeValues) {
		this.writer              = writer;
		this.varIdxToName        = varIdxToName;
		this.varIdxToRangeValues = varIdxToRangeValues;
		
		outputVariables();
	}
	
	//
	// START-XFormMarkovToBayes.BayesOutputListener
	@Override
	public void newCPT(ConditionalProbabilityTable cpt) {
		StringJoiner sj = new StringJoiner("\n");
		sj.add("potential "+getPotentialSignature(cpt));
		sj.add("{");
		sj.add("    data = "+getPotentialData(cpt));
		sj.add("}");
		
		output(sj.toString());
	}
	// END-XFormMarkovToBayes.BayesOutputListener
	//
	
	//
	// PRIVATE
	//
	
	private void outputVariables() {
		for (int i = 0; i < varIdxToName.size(); i++) {
			String rv = varIdxToName.get(i);
			
			StringJoiner sj = new StringJoiner("\n");
			
			sj.add("node "+getLegalHuginId(rv));
			sj.add("{");
			sj.add("  states = "+getRange(i));
			sj.add("  label  = \""+rv+"\";");
			sj.add("}");
			
			output(sj.toString());
		}
	}
	
	private void output(String toOutput) {
		// Write to the console so the user can see the output as it occurs
		// System.out.println(toOutput);
		try {
			writer.write(toOutput);
			writer.write("\n");
		}
		catch (IOException ioe) {
			throw new RuntimeException("Exception writing to output file", ioe);
		}
	}
	
	private String getLegalHuginId(String rv) {
		return rv.toString().replace('(', '_').replace(',', '_').replace(' ', '_').replace(')', '_');
	}
	
	private String getRange(Integer rvIdx) {
		// (\"false\" \"true\");
		StringJoiner sj = new StringJoiner(" ", "(", ");");
		for (String rangeValue : this.varIdxToRangeValues.get(rvIdx)) {
			sj.add("\""+rangeValue.toString()+"\"");
		}
		return sj.toString();
	}
	
	private String getPotentialSignature(ConditionalProbabilityTable cpt) {
		StringJoiner sj = new StringJoiner(" ", "(", ")");
		
		sj.add(getLegalHuginId(varIdxToName.get(cpt.getChildVariableIndex())));
		sj.add("|");
		for (Integer p : cpt.getParentVariableIndexes()) {
			sj.add(getLegalHuginId(varIdxToName.get(p)));
		}
		
		return sj.toString();
	}
	
	private static String getPotentialData(ConditionalProbabilityTable cpt) {
		StringJoiner sj = new StringJoiner(" ", "(", ");");
		
		for (Double d : cpt.getTable().getEntries()) {
			sj.add(""+d);
		}
		
		return sj.toString();
	}
}
