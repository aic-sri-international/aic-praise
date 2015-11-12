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
package com.sri.ai.praise.evaluate.generate;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.util.List;
import java.util.Random;

import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;

import com.google.common.base.Charsets;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.AbstractRandomDPLLProblemGenerator;
import com.sri.ai.praise.model.v1.HOGMSortDeclaration;

/**
 * Utility class for generating Random HOGMv1 models.
 * 
 * @author oreilly
 *
 */
public class RandomHOGMv1Generator {
	public static final Charset FILE_CHARSET = Charsets.UTF_8;
	//
	public static final String GENERATOR_SORT_NAME = "TestDomain";
	
	static class GeneratorArgs implements AutoCloseable {
		// Optional
		Random       random     = null;       // -r 	
		int          domainSize = 1000;       // -s
		PrintStream  out        = System.out; // -o
		// Required
		int numberPotentials;              // -p
		int numberVariables;               // -v
		int numberUniquelyNamedConstants;  // -u
		int formulaDepth;                  // -d
		int formulaBreadth;                // -b		
		
		@Override
		public void close() throws IOException {
			out.flush();
			// Only close if not System.out
			if (out != System.out) {				
				out.close();
			}
		}
	}
	
	/**
	 * Generate Random HOGMv1 models based on given command line arguments.
	 * 
	 * @param args
	 *        pass '--help' to see description of expected program arguments.
	 */
	public static void main(String[] args) {
		try (GeneratorArgs genArgs = getArgs(args)) {		
			// Output the sort information
			genArgs.out.append(HOGMSortDeclaration.FUNCTOR_SORT_DECLARATION);
			genArgs.out.append(" ");
			genArgs.out.append(GENERATOR_SORT_NAME);
			genArgs.out.append(" : ");
			genArgs.out.append(""+genArgs.domainSize);
			for (int i = 0; i < genArgs.numberUniquelyNamedConstants; i++) {
				genArgs.out.append(", ");
				genArgs.out.append(ConditionalGenerator._constantPrefix+i);
			}
			genArgs.out.println(";");
			genArgs.out.println();
			
			// output the random variable information
			for (int i = 0; i < genArgs.numberVariables; i++) {
				genArgs.out.append("random ");
				genArgs.out.append(ConditionalGenerator._variablePrefix+i);
				genArgs.out.append(" : ");
				genArgs.out.append(GENERATOR_SORT_NAME);
				genArgs.out.println(";");
			}
			genArgs.out.println();
			
			ConditionalGenerator conditionalGenerator = new ConditionalGenerator(genArgs.random, genArgs.numberVariables, genArgs.numberUniquelyNamedConstants, genArgs.formulaDepth, genArgs.formulaBreadth);
		
			for (int i = 0; i < genArgs.numberPotentials; i++) {		
				Expression conditional = conditionalGenerator.next();
				// Ensure we have variables in the conditional
				while (Expressions.freeVariables(conditional, new DefaultRewritingProcess(null)).size() == 0) {							
					conditional = conditionalGenerator.next();
				}
				
				genArgs.out.append(conditional.toString());
				genArgs.out.println(";");
			}
		} catch (Exception ex) {
			System.err.println("Error generating random HOGM");
			ex.printStackTrace();
		}
		
	}
	
	private static GeneratorArgs getArgs(String[] args) throws UnsupportedEncodingException, FileNotFoundException, IOException {
		GeneratorArgs result = new GeneratorArgs();
		
		OptionParser parser = new OptionParser();
		// Optional
		OptionSpec<Integer> randomSeed = parser.accepts("r", "random seed.").withRequiredArg().ofType(Integer.class);
		OptionSpec<Integer> domainSize = parser.accepts("s", "domain size.").withRequiredArg().ofType(Integer.class).defaultsTo(result.domainSize);
		OptionSpec<File>    outputFile = parser.accepts("o", "output file name (defaults to stdout).").withRequiredArg().ofType(File.class);
		// Required
		OptionSpec<Integer> numPotentials = parser.accepts("p", "# potentials to generate.").withRequiredArg().required().ofType(Integer.class);
		OptionSpec<Integer> numVariables  = parser.accepts("v", "# variables to use in the generation process.").withRequiredArg().required().ofType(Integer.class);
		OptionSpec<Integer> numConstants  = parser.accepts("u", "# uniquely named constants to use in the generation process.").withRequiredArg().required().ofType(Integer.class);
		OptionSpec<Integer> depth         = parser.accepts("d", "the depth of the generated formulas (all their sub-expressions will have depth equal to depth - 1.").withRequiredArg().required().ofType(Integer.class);
		OptionSpec<Integer> breadth       = parser.accepts("b", "the number of sub-expressions of conjunctions and disjunctions in the generated formulas.").withRequiredArg().required().ofType(Integer.class);
		//
		parser.accepts("help").forHelp();
		
		OptionSet options = parser.parse(args);
		
		if (options.has("help")) {
			parser.printHelpOn(System.out);
			System.exit(0);
		}
		
		// 
		// Handle optional args
		if (options.has(randomSeed)) {
			result.random = new Random(options.valueOf(randomSeed).longValue());
		}
		else {
			result.random = new Random();
		}
		result.domainSize = options.valueOf(domainSize);
		if (options.has(outputFile)) {
			result.out = new PrintStream(options.valueOf(outputFile), FILE_CHARSET.name());
		}
		
		result.numberPotentials             = options.valueOf(numPotentials);
		result.numberVariables              = options.valueOf(numVariables);
		result.numberUniquelyNamedConstants = options.valueOf(numConstants);
		result.formulaDepth                 = options.valueOf(depth);
		result.formulaBreadth               = options.valueOf(breadth);
				
		// #uniquely named constants must be <= domain size
		if (result.numberUniquelyNamedConstants > result.domainSize) {
			throw new IllegalArgumentException("#uniquely named constants "+result.numberUniquelyNamedConstants+" is greater than the given domain size of "+result.domainSize);
		}
		
		return result;
	}
}

class ConditionalGenerator extends AbstractRandomDPLLProblemGenerator {
	//
	static final String _variablePrefix = "X";
	static final String _constantPrefix = "a";
	
	private Random random;
	public ConditionalGenerator(Random random, int numberOfVariables, int numberOfConstants, int depth, int breadth) {
		super(random, numberOfVariables, numberOfConstants, numberOfVariables, depth, breadth);
		this.random = random;
	}
	
	@Override
	protected Expression makeProblem(Expression formula, List<Expression> indices) {
		
		double thenValue = random.nextDouble();
		double elseValue = 1.0 - thenValue;
		
		Expression problem = IfThenElse.make(formula, Expressions.makeSymbol(thenValue), Expressions.makeSymbol(elseValue));
		return problem;
	}	
}
