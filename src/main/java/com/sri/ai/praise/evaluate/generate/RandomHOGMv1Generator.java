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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.base.Charsets;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.tester.RandomConditionalExpressionGenerator;
import com.sri.ai.praise.lang.ModelLanguage;
import com.sri.ai.praise.model.v1.HOGMSortDeclaration;

import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;

/**
 * Utility class for generating Random HOGMv1 models.
 * 
 * @author oreilly
 *
 */
public class RandomHOGMv1Generator {
		
	public static abstract class TheoryTypeArgs {
		private int numberVariables;
		public int getNumberVariables() {
			return numberVariables;
		}
		
		//
		// PROTECTED
		protected void setNumberVariables(int numberVariables) {
			if (numberVariables <= 0) {
				throw new IllegalArgumentException("Number of variables must be > 0");
			}
			this.numberVariables = numberVariables;
		}
	}
	
	public static class TheoryTypePropositionalArgs extends TheoryTypeArgs {
		private static Pattern _propositionalArgsPattern = Pattern.compile("v(\\d+?)"); 
		public TheoryTypePropositionalArgs(String args) {
			Matcher m = _propositionalArgsPattern.matcher(args);
			if (m.matches()) {
				setNumberVariables(Integer.valueOf(m.group(1)));
			}
			else {
				throw new IllegalArgumentException("Illegal propositional theory arguments passed: "+args);
			}
		}
	}
	
	public static class TheoryTypeEqualityArgs extends TheoryTypeArgs {
		private static Pattern _equalityArgsPattern = Pattern.compile("v(\\d+?)c(\\d+?)u(\\d+?)");
		//
		private int sizeOfCategory, numberOfUniquelyNamedConstantsInCategory;
		
		public TheoryTypeEqualityArgs(String args) {
			Matcher m = _equalityArgsPattern.matcher(args);
			if (m.matches()) {
				setNumberVariables(Integer.valueOf(m.group(1)));
				this.sizeOfCategory                           = Integer.valueOf(m.group(2));
				this.numberOfUniquelyNamedConstantsInCategory = Integer.valueOf(m.group(3));
				if (this.numberOfUniquelyNamedConstantsInCategory > this.sizeOfCategory) {
					throw new IllegalArgumentException("Cannot have number of uniquely named constants be greater than the size of the category.");
				}
			}
			else {
				throw new IllegalArgumentException("Illegal equality theory arguments passed: "+args);
			}
		}
		
		public int getSizeOfCategory() {
			return sizeOfCategory;
		}
		
		public int getNumberOfUniquelyNamedConstantsInCategory() {
			return numberOfUniquelyNamedConstantsInCategory;
		}
	}
	
	public static class TheoryTypeInequalityArgs extends TheoryTypeArgs {
		private static Pattern _inequalityArgsPattern = Pattern.compile("v(\\d+?)s(\\d+?)e(\\d+?)");
		//
		private int startIntervalInclusive, endIntervalInclusive;
		
		public TheoryTypeInequalityArgs(String args) {
			Matcher m = _inequalityArgsPattern.matcher(args);
			if (m.matches()) {
				setNumberVariables(Integer.valueOf(m.group(1)));
				this.startIntervalInclusive  = Integer.valueOf(m.group(2));
				this.endIntervalInclusive    = Integer.valueOf(m.group(3));
				
				if (this.startIntervalInclusive > this.endIntervalInclusive) {
					throw new IllegalArgumentException("Cannot have start of interval be greater than the end of the interval.");
				}
			}
			else {
				throw new IllegalArgumentException("Illegal inequality theory arguments passed: "+args);
			}
		}
		
		public int getStartIntervalInclusive() {
			return startIntervalInclusive;
		}
		
		public int getEndIntervalInclusive() {
			return endIntervalInclusive;
		}
	}
	
	public static final Charset FILE_CHARSET = Charsets.UTF_8;
	//
	public static final String GENERATOR_CATEGORICAL_SORT_NAME_PREFIX = "Categorical";
	
	static class GeneratorArgs implements AutoCloseable {
		// Optional
		Random       random     = null;       // -r 	
		PrintStream  out        = System.out; // -o
		
		// One instance of the following 3 theory types arguments required at minimum
		TheoryTypePropositionalArgs[] propositionalTheories; // -p propositional - number variables
		TheoryTypeEqualityArgs[]      equalityTheories;      // -e equality      - number variables, size of category, number uniquely names constants
		TheoryTypeInequalityArgs[]    inequalityTheories;    // -i inequality    - number variables, start interval (inclusive), end interval (inclusive)
		// Required
		int numberPotentials; // -n
		int depth;            // -d	
		
		// Derived
		RandomConditionalPotentialExpressionGenerator potentialExpressionGenerator;
		
		@Override
		public void close() throws IOException {
			out.flush();
			// Only close if not System.out
			if (out != System.out) {				
				out.close();
			}
		}
	}

// TODO	
	public static void generate(String outputDirectory, long seed, TheoryTypeArgs[] theoryTypes) {
		File rootOutputDirectory = validateDirectory(outputDirectory);
		File hogmv1ProblemDirectory      = new File(rootOutputDirectory, ModelLanguage.HOGMv1.getCode());
		if (!hogmv1ProblemDirectory.exists()) {
			hogmv1ProblemDirectory.mkdir();
		}

// TODO		
//		for (int p = 0; p < params.length; p++) {
//			int numberOfPotentials = params[p][_potentialIdx];
//			int numberOfVariables  = params[p][_variableIdx];			
//			int depth              = params[p][_depthIdx];
//			int breadth            = params[p][_breadthIdx];
//			
//			for (int i = 0; i < domainSizes.length; i++) {
//				int cardinality = domainSizes[i];
//				
//				int numberOfUniquelyNamedConstants = params[p][_uniquelyNamedConstantIdx];
//				// #constants must be <= domain size
//				if (numberOfUniquelyNamedConstants > cardinality) {
//					numberOfUniquelyNamedConstants = cardinality;
//				}
//				
//				String outputFileSuffix = "_r"+seed+"_s"+cardinality+"_t_"+theoryType.getCode()+"_p"+numberOfPotentials+"_v"+numberOfVariables+"_u"+numberOfUniquelyNamedConstants+"_d"+depth+"_b"+breadth;
//				
//				RandomHOGMv1Generator.main(new String[] {
//						"-r="+seed,
//						"-s="+cardinality,
//						"-o="+new File(hogmv1ProblemDirectory, "sg_random_model"+outputFileSuffix+ModelLanguage.HOGMv1.getDefaultFileExtension()).getAbsolutePath(),
//						"-t="+theoryType.getCode(),
//						"-p="+numberOfPotentials,
//						"-v="+numberOfVariables,
//						"-u="+numberOfUniquelyNamedConstants,
//						"-d="+depth,
//						"-b="+breadth
//				});				
//			}
//		}
	}
	
	/**
	 * Generate Random HOGMv1 models based on given command line arguments.
	 * 
	 * @param args
	 *        pass '--help' to see description of expected program arguments.
	 */
	public static void main(String[] args) {
		try (GeneratorArgs genArgs = getArgs(args)) {
			
			// Output the categorical sorts
			for (int c = 0; c < genArgs.equalityTheories.length; c++) {
				TheoryTypeEqualityArgs equalityTheoryArgs = genArgs.equalityTheories[c];
				
				genArgs.out.append(HOGMSortDeclaration.FUNCTOR_SORT_DECLARATION);
				genArgs.out.append(" ");
				genArgs.out.append(GENERATOR_CATEGORICAL_SORT_NAME_PREFIX+c);
				genArgs.out.append(" : ");
				genArgs.out.append(""+equalityTheoryArgs.getSizeOfCategory());
				for (int u = 0; u < equalityTheoryArgs.getNumberOfUniquelyNamedConstantsInCategory(); u++) {
					genArgs.out.append(", ");
					genArgs.out.append(genArgs.potentialExpressionGenerator.getCategoryUniquelyNamedConstantName(c, u));
				}
				genArgs.out.println(";");				
			}
			if (genArgs.equalityTheories.length > 0) {
				genArgs.out.println(); // Place a blank line after any categorical sort declarations
			}
			
//			if (genArgs.theoryType != TheoryType.Inequality) {
//				// Output the categorical sort information
//				genArgs.out.append(HOGMSortDeclaration.FUNCTOR_SORT_DECLARATION);
//				genArgs.out.append(" ");
//				genArgs.out.append(GENERATOR_SORT_NAME);
//				genArgs.out.append(" : ");
//				genArgs.out.append(""+genArgs.domainSize);
//				for (int i = 0; i < genArgs.numberUniquelyNamedConstants; i++) {
//					genArgs.out.append(", ");
//					genArgs.out.append(genArgs.potentialExpressionGenerator.getUniquelyNamedConstantName(i));
//				}
//				genArgs.out.println(";");
//				genArgs.out.println();
//			}
//			
//			// output the random variable information
//			for (int i = 0; i < genArgs.numberVariables; i++) {
//				genArgs.out.append("random ");
//				genArgs.out.append(genArgs.potentialExpressionGenerator.getVariableNameFor(i));
//				genArgs.out.append(" : ");
//				if (genArgs.theoryType != TheoryType.Inequality) {
//					genArgs.out.append(GENERATOR_SORT_NAME);
//				}
//				else {
//					genArgs.out.append("0.."+(genArgs.domainSize-1));
//				}
//				genArgs.out.println(";");
//			}
//			genArgs.out.println();
//				
//			for (int i = 0; i < genArgs.numberPotentials; i++) {		
//				Expression conditional = genArgs.potentialExpressionGenerator.nextPotentialExpression();
//				// Ensure we have variables in the conditional
//				while (Expressions.freeVariables(conditional, new DefaultRewritingProcess(null)).size() == 0) {							
//					conditional = genArgs.potentialExpressionGenerator.nextPotentialExpression();
//				}
//				
//				genArgs.out.append(conditional.toString());
//				genArgs.out.println(";");
//			}
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
		OptionSpec<File>    outputFile = parser.accepts("o", "output file name (defaults to stdout).").withRequiredArg().ofType(File.class);
		// At least one instance of one of the following required
		OptionSpec<String> propositionalTheoryTypes = parser.accepts("p", "propositional theory type args ('v#' - v)variable #number).").withRequiredArg().ofType(String.class);
		OptionSpec<String> equalityTheoryTypes      = parser.accepts("e", "equality theory type args ('v#c##u' - v)ariable #number, c)ategorical size #number, u)niquely named constants listed in category #number).").withRequiredArg().ofType(String.class);
		OptionSpec<String> inequalityTheoryTypes    = parser.accepts("i", "inequality theory type args ('v#s#e#' - v)ariable #number, s)tart integer interval inclusive #number, e)nd integer interval inclusive #number).").withRequiredArg().ofType(String.class);
		// Required
		OptionSpec<Integer> numPotentials  = parser.accepts("n", "# potentials to generate.").withRequiredArg().required().ofType(Integer.class);
		OptionSpec<Integer> depth          = parser.accepts("d", "the depth of the generated formulas (all their sub-expressions will have depth equal to depth - 1.").withRequiredArg().required().ofType(Integer.class);
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

		if (options.has(outputFile)) {
			result.out = new PrintStream(options.valueOf(outputFile), FILE_CHARSET.name());
		}
		
		List<String> propositionalTheoryTypeArgs = options.valuesOf(propositionalTheoryTypes);
		result.propositionalTheories = new TheoryTypePropositionalArgs[propositionalTheoryTypeArgs.size()];
		for (int i = 0; i < result.propositionalTheories.length; i++) {
			result.propositionalTheories[i] = new TheoryTypePropositionalArgs(propositionalTheoryTypeArgs.get(i));
		}
		List<String> equalityTheoryTypeArgs = options.valuesOf(equalityTheoryTypes);
		result.equalityTheories = new TheoryTypeEqualityArgs[equalityTheoryTypeArgs.size()];
		for (int i = 0; i < result.equalityTheories.length; i++) {
			result.equalityTheories[i] = new TheoryTypeEqualityArgs(equalityTheoryTypeArgs.get(i));
		}
		List<String> inequalityTheoryTypeArgs = options.valuesOf(inequalityTheoryTypes);
		result.inequalityTheories = new TheoryTypeInequalityArgs[inequalityTheoryTypeArgs.size()];
		for (int i = 0; i < result.inequalityTheories.length; i++) {
			result.inequalityTheories[i] = new TheoryTypeInequalityArgs(inequalityTheoryTypeArgs.get(i));
		}
		
		if (result.propositionalTheories.length == 0 && result.equalityTheories.length == 0 && result.inequalityTheories.length == 0) {
			throw new IllegalArgumentException("At least one set of propositional, equality, or inequality theory type args must be provided.");
		}
				
		result.numberPotentials             = options.valueOf(numPotentials);
		result.depth                        = options.valueOf(depth);
						
		result.potentialExpressionGenerator = new RandomConditionalPotentialExpressionGenerator(
				result.random, 
				result.propositionalTheories,
				result.equalityTheories,
				result.inequalityTheories,
				result.depth);

		return result;
	}
	
	private static ConstraintTheory newConstraintTheory() {
		ConstraintTheory result = null;
// TODO		
//		switch(theoryType) {
//		case Propositional:
//			result = new PropositionalConstraintTheory();
//			break;
//		case Equality:
//			result = new CompoundConstraintTheory(
//					new EqualityConstraintTheory(true, true), // first flag is 'true' because all equalities are atoms in this theory; there is no need to check arguments type
//					new PropositionalConstraintTheory());
//			break;
//		case Inequality:
//			result = new CompoundConstraintTheory(
//					new InequalityConstraintTheory(false, true), // 'false' because not all equalities are atoms in this theory; need to check arguments type
//					new EqualityConstraintTheory(false, true), // 'false' because not all equalities are atoms in this theory; need to check arguments type
//					new PropositionalConstraintTheory());
//			break;
//		default:
//			throw new UnsupportedOperationException("Currently do not support constraint theory for = "+theoryType.getCode());
//		}
		
		return result;
	}
	
	private static File validateDirectory(String directoryName) {
		File result = new File(directoryName);
		if (!result.exists()) {
			throw new IllegalArgumentException("Output directory does not exist: "+directoryName);
		}
		if (!result.isDirectory()) {
			throw new IllegalArgumentException("Output directory is not a directory: "+result.getAbsolutePath());
		}
		
		return result;
	}
}

class RandomConditionalPotentialExpressionGenerator {
	
	private RandomConditionalExpressionGenerator randomConditionalGenerator;
	
	public RandomConditionalPotentialExpressionGenerator(Random random, 
			RandomHOGMv1Generator.TheoryTypePropositionalArgs[] propositionTheoryArgs, 
			RandomHOGMv1Generator.TheoryTypeEqualityArgs[] equalityTheoryArgs,
			RandomHOGMv1Generator.TheoryTypeInequalityArgs[] inequalityTheoryArgs,
			int depth) {		

		Type sort = null;
// TODO		
//		if (theoryType == RandomHOGMv1Generator.TheoryType.Inequality) {
//			sortName = "Integer(0," + (domainSize - 1) + ")"; // IntegerInterval bounds are included bounds, so they need to be 0..(domainSize - 1)
//			sort = new IntegerInterval(sortName);
//		}
//		else {
//			sortName = RandomHOGMv1Generator.GENERATOR_SORT_NAME;
//			ArrayList<Expression> knownUniquelyNamedConstants = new ArrayList<>();
//			IntStream.range(0, numberOfUniquelyNamedConstants)
//				.mapToObj(idx -> Expressions.makeSymbol(getUniquelyNamedConstantName(idx)))
//				.forEach(knownUniquelyNamedConstants::add);
//			sort = new Categorical(sortName, domainSize, knownUniquelyNamedConstants);
//		}
		
//		Map<String, Type> varToTypeMap = new LinkedHashMap<>();		
//		IntStream.range(0, numberOfVariables)
//			.mapToObj(this::getVariableNameFor)
//			.forEach(varName -> varToTypeMap.put(varName, sort));
//		constraintTheory.setVariableNamesAndTypesForTesting(varToTypeMap);
//		
//		RewritingProcess process = constraintTheory.extendWithTestingInformation(new DefaultRewritingProcess(null));
//		
//		randomConditionalGenerator = new RandomConditionalExpressionGenerator(random, constraintTheory, depth,
//				() -> makeSymbol(random.nextDouble()),
//				process);
	}
	
	public String getVariableNameFor(int theoryIndex, int varIndex) {
		return "T" + theoryIndex + "X" + varIndex;
	}
	
	public String getCategoryUniquelyNamedConstantName(int categoryIndex, int uniqueConstantIndex) {
		return "ac" + categoryIndex + "u" + uniqueConstantIndex;
	}
	
	public Expression nextPotentialExpression() {
		return randomConditionalGenerator.apply();
	}
}
