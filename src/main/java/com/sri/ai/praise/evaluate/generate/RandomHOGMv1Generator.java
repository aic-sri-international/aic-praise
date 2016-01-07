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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.stream.IntStream;

import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;

import com.google.common.base.Charsets;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.AbstractRandomDPLLProblemGenerator;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.tester.RandomConditionalExpressionGenerator;
import com.sri.ai.grinder.sgdpll2.theory.compound.CompoundConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.equality.EqualityConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.inequality.InequalityConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.propositional.PropositionalConstraintTheory;
import com.sri.ai.praise.model.v1.HOGMSortDeclaration;

/**
 * Utility class for generating Random HOGMv1 models.
 * 
 * @author oreilly
 *
 */
public class RandomHOGMv1Generator {
	enum TheoryType {
		HistoricalEqualityFormula("historical"),
		Propositional("propositional"),
		Equality("equality"),
		Inequality("inequality");
		
		public String getCode() {
			return commandLineCode;
		}
		
		//
		public static TheoryType getTheorTypeForCommandLineCode(String code) {
			TheoryType result = null;
			code = code.toLowerCase().trim();
			for (TheoryType tt : TheoryType.values()) {
				if (code.equals(tt.getCode())) {
					result = tt;
					break;
				}
			}
			
			return result;
		}
		
		public static List<String> getLegalCommandLineCodes() {
			List<String> result = new ArrayList<>();
			Arrays.stream(TheoryType.values()).forEach(tt -> result.add(tt.getCode()));
			return result;
		}
		
		//
		//
		private String commandLineCode;
		private TheoryType(String commandLineCode) {
			this.commandLineCode = commandLineCode;
		}
	}
	
	public static final Charset FILE_CHARSET = Charsets.UTF_8;
	//
	public static final String GENERATOR_SORT_NAME = "TestDomain";
	
	static class GeneratorArgs implements AutoCloseable {
		// Optional
		Random       random     = null;       // -r 	
		int          domainSize = 1000;       // -s
		PrintStream  out        = System.out; // -o
		
		// Required
		TheoryType theoryType;             // -t 
		int numberPotentials;              // -p
		int numberVariables;               // -v
		int numberUniquelyNamedConstants;  // -u
		int depth;                         // -d
		int breadth;                       // -b		
		
		// Derived
		RandomTermGenerator termGenerator;
		
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
				genArgs.out.append(genArgs.termGenerator.getUniquelyNamedConstantName(i));
			}
			genArgs.out.println(";");
			genArgs.out.println();
			
			// output the random variable information
			for (int i = 0; i < genArgs.numberVariables; i++) {
				genArgs.out.append("random ");
				genArgs.out.append(genArgs.termGenerator.getVariableNameFor(i));
				genArgs.out.append(" : ");
				genArgs.out.append(GENERATOR_SORT_NAME);
				genArgs.out.println(";");
			}
			genArgs.out.println();
				
			for (int i = 0; i < genArgs.numberPotentials; i++) {		
				Expression conditional = genArgs.termGenerator.nextTerm();
				// Ensure we have variables in the conditional
				while (Expressions.freeVariables(conditional, new DefaultRewritingProcess(null)).size() == 0) {							
					conditional = genArgs.termGenerator.nextTerm();
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
		OptionSpec<String>  theoryTypeCode = parser.accepts("t", "theory type.").withRequiredArg().required().ofType(String.class);
		OptionSpec<Integer> numPotentials  = parser.accepts("p", "# potentials to generate.").withRequiredArg().required().ofType(Integer.class);
		OptionSpec<Integer> numVariables   = parser.accepts("v", "# variables to use in the generation process.").withRequiredArg().required().ofType(Integer.class);
		OptionSpec<Integer> numConstants   = parser.accepts("u", "# uniquely named constants to use in the generation process.").withRequiredArg().required().ofType(Integer.class);
		OptionSpec<Integer> depth          = parser.accepts("d", "the depth of the generated formulas (all their sub-expressions will have depth equal to depth - 1.").withRequiredArg().required().ofType(Integer.class);
		OptionSpec<Integer> breadth        = parser.accepts("b", "the number of sub-expressions of conjunctions and disjunctions in the generated formulas.").withRequiredArg().required().ofType(Integer.class);
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
		
		result.theoryType                   = TheoryType.getTheorTypeForCommandLineCode(options.valueOf(theoryTypeCode));
		result.numberPotentials             = options.valueOf(numPotentials);
		result.numberVariables              = options.valueOf(numVariables);
		result.numberUniquelyNamedConstants = options.valueOf(numConstants);
		result.depth                        = options.valueOf(depth);
		result.breadth                      = options.valueOf(breadth);
		
		if (result.theoryType == null) {
			throw new IllegalArgumentException("Unrecognized theory type code, legal values are: "+TheoryType.getLegalCommandLineCodes());
		}
				
		// #uniquely named constants must be <= domain size
		if (result.numberUniquelyNamedConstants > result.domainSize) {
			throw new IllegalArgumentException("#uniquely named constants "+result.numberUniquelyNamedConstants+" is greater than the given domain size of "+result.domainSize);
		}
		
		switch(result.theoryType) {
		case HistoricalEqualityFormula:
			result.termGenerator = new HistoricalEqualityFormulaConditionalGenerator(result.random,
					result.numberUniquelyNamedConstants, 
					result.numberVariables, 
					result.depth, 
					result.breadth);
			break;
		case Propositional:
		case Equality:
		case Inequality:		
			result.termGenerator = new RandomConditionalExpressionTermGenerator(result.random, 
					newConstraintTheory(result.theoryType), 
					result.domainSize,
					result.numberUniquelyNamedConstants,
					result.numberVariables,
					result.depth);
			break;
		default:
			throw new UnsupportedOperationException("Currently do not support term generation for theory = "+result.theoryType.getCode());
		}
		
		return result;
	}
	
	private static ConstraintTheory newConstraintTheory(TheoryType theoryType) {
		ConstraintTheory result = null;
		
		switch(theoryType) {
		case Propositional:
			result = new PropositionalConstraintTheory();
			break;
		case Equality:
			result = new CompoundConstraintTheory(
					new EqualityConstraintTheory(true, true),
					new PropositionalConstraintTheory());
			break;
		case Inequality:
			result = new CompoundConstraintTheory(
					new InequalityConstraintTheory(true, false),
					new EqualityConstraintTheory(true, true),
					new PropositionalConstraintTheory());
			break;
		default:
			throw new UnsupportedOperationException("Currently do not support constraint theory for = "+theoryType.getCode());
		}
		
		return result;
	}
}

interface RandomTermGenerator {
	default String getVariableNameFor(int idx) {
		return "X"+idx;
	}
	
	default String getUniquelyNamedConstantName(int idx) {
		return "a"+idx;
	}
	
	Expression nextTerm();
}

class RandomConditionalExpressionTermGenerator implements RandomTermGenerator {
	
	private RandomConditionalExpressionGenerator randomConditionalGenerator;
	
	public RandomConditionalExpressionTermGenerator(Random random, ConstraintTheory constraintTheory, int domainSize, int numberOfUniquelyNamedConstants, int numberOfVariables, int depth) {		
		ArrayList<Expression> knownUniquelyNamedConstants = new ArrayList<>();
		IntStream.range(0, numberOfUniquelyNamedConstants)
			.mapToObj(idx -> Expressions.makeSymbol(getUniquelyNamedConstantName(idx)))
			.forEach(knownUniquelyNamedConstants::add);
		constraintTheory.setTypesForTesting(Collections.singletonList(
				new Categorical(RandomHOGMv1Generator.GENERATOR_SORT_NAME, domainSize, knownUniquelyNamedConstants)));
		
		Map<String, String> varToTypeMap = new LinkedHashMap<>();		
		IntStream.range(0, numberOfVariables)
			.mapToObj(this::getVariableNameFor)
			.forEach(varName -> varToTypeMap.put(varName, RandomHOGMv1Generator.GENERATOR_SORT_NAME));
		constraintTheory.setVariableNamesAndTypeNamesForTesting(varToTypeMap);
		
		RewritingProcess process = constraintTheory.extendWithTestingInformation(new DefaultRewritingProcess(null));
		
		randomConditionalGenerator = new RandomConditionalExpressionGenerator(random, constraintTheory, depth,
				() -> Expressions.makeSymbol(random.nextDouble()),
				process);
	}
	
	@Override
	public Expression nextTerm() {
		return randomConditionalGenerator.apply();
	}
}

class HistoricalEqualityFormulaConditionalGenerator extends AbstractRandomDPLLProblemGenerator implements RandomTermGenerator {	
	private Random random;
	public HistoricalEqualityFormulaConditionalGenerator(Random random, int numberOfUniquelyNamedConstants, int numberOfVariables, int depth, int breadth) {
		super(random, numberOfVariables, numberOfUniquelyNamedConstants, numberOfVariables, depth, breadth);
		this.random = random;
	}
	
	@Override
	public Expression nextTerm() {
		return next();
	}
	
	@Override
	protected Expression makeProblem(Expression formula, List<Expression> indices) {
		double thenValue = random.nextDouble();
		double elseValue = 1.0 - thenValue;
		
		Expression problem = IfThenElse.make(formula, Expressions.makeSymbol(thenValue), Expressions.makeSymbol(elseValue));
		return problem;
	}	
}
