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
package com.sri.ai.praise.core.translation.core.uai;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.math.Rational;

@Beta
public class UAIHOGModelGroundingListener implements HOGModelGrounding.Listener {
	
	private PrintWriter uaiModelOutput    = null;
	private PrintWriter uaiEvidenceOutput = null;
	private File tempPreambleFile         = null;
	private File tempFunctionTablesFile   = null;
	private List<Pair<Integer, Integer>> evidence = new ArrayList<>();
	
	private int    numberVariables;
	private Writer preamble       = null;
	private Writer functionTables = null;
	
	public UAIHOGModelGroundingListener(PrintWriter uaiModelOutput, PrintWriter uaiEvidenceOutput) {
		this.uaiModelOutput    = uaiModelOutput;
		this.uaiEvidenceOutput = uaiEvidenceOutput;
		try {	
			tempPreambleFile       = File.createTempFile("preamble", "uai");
			tempFunctionTablesFile = File.createTempFile("factorTables", "uai");
			
			preamble       = new OutputStreamWriter(new FileOutputStream(tempPreambleFile));
			functionTables = new OutputStreamWriter(new FileOutputStream(tempFunctionTablesFile));
		}
		catch (IOException ioe) {
			throw new RuntimeException(ioe);
		}
	}
	
	//
	// START-HOGModelGrounding.Listener
	@Override
	public void numberGroundVariables(int number) { 
		this.numberVariables = number;
		writePreamble("MARKOV\n");
		writePreamble("" + number + "\n");
	}	
	
	@Override
	public void groundVariableCardinality(int variableIndex, int cardinality) {
		writePreamble("" + cardinality);
		if (variableIndex == (numberVariables - 1)) {
			writePreamble("\n");
		}
		else {
			writePreamble(" ");
		}
	}
	
	@Override
	public void numberFactors(int number) {
		writePreamble("" + number + "\n");
	}
	
	@Override
	public void factorParticipants(int factorIndex, int[] variableIndexes) {
		writePreamble("" + variableIndexes.length);
		for (int i = 0; i < variableIndexes.length; i++) {
			writePreamble(" " + variableIndexes[i]);
		}
		writePreamble("\n");
	}
	
	@Override
	public void factorValue(int numberFactorValues, boolean isFirstValue, boolean isLastValue, Rational value) {
		if (isFirstValue) {
			writeFunctionTables("\n" + numberFactorValues + "\n");
		}
		else {
			writeFunctionTables(" ");
		}
		
		writeFunctionTables("" + value.doubleValue());
	
		if (isLastValue) {
			writeFunctionTables("\n");
		}
	}	
	
	@Override
	public void evidence(int variableIndex, int valueIndex) {
		evidence.add(new Pair<>(variableIndex, valueIndex));
	}
	
	@Override
	public void groundingComplete() {
		try {
			preamble.flush();
			preamble.close();
			functionTables.flush();
			functionTables.close();
			
			try (Reader fisPreamble       = new FileReader(tempPreambleFile);
				 Reader fisFunctionTables = new FileReader(tempFunctionTablesFile);) {
				
				char[] buffer = new char[1024];
				int read;
				while ((read = fisPreamble.read(buffer)) != -1) {
					uaiModelOutput.write(buffer, 0, read);
				}
				while ((read = fisFunctionTables.read(buffer)) != -1) {
					uaiModelOutput.write(buffer, 0, read);
				}
				uaiModelOutput.flush();
				
				// Indicate number of observed variables in the evidence file
				uaiEvidenceOutput.write("" + evidence.size());
				// and their variable and value indexes
				for (Pair<Integer, Integer> evidenceAssignment : evidence) {
					uaiEvidenceOutput.write(" ");
					uaiEvidenceOutput.write("" + evidenceAssignment.first);
					uaiEvidenceOutput.write(" ");
					uaiEvidenceOutput.write("" + evidenceAssignment.second);
				}
				uaiEvidenceOutput.flush();
			}
		
			tempPreambleFile.delete();
			tempFunctionTablesFile.delete();
		}
		catch (IOException ioe) {
			throw new RuntimeException(ioe);
		}
	}
	
	// END-HOGModelGrounding.Listener
	//
	
	//
	// PRIVATE
	//
	private void writePreamble(String string) {
		try {
			preamble.write(string);
		}
		catch (IOException ioe) {
			throw new RuntimeException(ioe);
		}
	}
	
	private void writeFunctionTables(String string) {
		try {
			functionTables.write(string);
		}
		catch (IOException ioe) {
			throw new RuntimeException(ioe);
		}
	}
}
