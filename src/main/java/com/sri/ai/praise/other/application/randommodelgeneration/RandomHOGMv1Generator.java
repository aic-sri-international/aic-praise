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
package com.sri.ai.praise.other.application.randommodelgeneration;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.IntStream;

import com.google.common.base.Charsets;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.tester.RandomConditionalExpressionGenerator;
import com.sri.ai.grinder.tester.TheoryTestingSupport;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.equality.EqualityTheory;
import com.sri.ai.grinder.theory.propositional.PropositionalTheory;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMSortDeclaration;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.ModelLanguage;
import com.sri.ai.util.Util;
import com.sri.ai.util.math.Rational;

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
		
		public abstract List<String> toCommandLineArgs();
		
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
		
		public TheoryTypePropositionalArgs(int numberVariables) {
			setNumberVariables(numberVariables);
		}
		
		@Override
		public List<String> toCommandLineArgs() {
			return Arrays.asList("-p", "v"+getNumberVariables());
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
				checkForIllegalArguments();
			}
			else {
				throw new IllegalArgumentException("Illegal equality theory arguments passed: "+args);
			}
		}
		
		public TheoryTypeEqualityArgs(int numberVariables, int sizeOfCategory, int numberOfUniquelyNamedConstantsInCategory) {
			setNumberVariables(numberVariables);
			this.sizeOfCategory                           = sizeOfCategory;
			this.numberOfUniquelyNamedConstantsInCategory = numberOfUniquelyNamedConstantsInCategory;
			
			checkForIllegalArguments();
		}
		
		public int getSizeOfCategory() {
			return sizeOfCategory;
		}
		
		public int getNumberOfUniquelyNamedConstantsInCategory() {
			return numberOfUniquelyNamedConstantsInCategory;
		}
		
		@Override
		public List<String> toCommandLineArgs() {
			return Arrays.asList("-e", "v"+getNumberVariables()+"c"+getSizeOfCategory()+"u"+getNumberOfUniquelyNamedConstantsInCategory());
		}
		
		private void checkForIllegalArguments() {
			if (this.numberOfUniquelyNamedConstantsInCategory > this.sizeOfCategory) {
				throw new IllegalArgumentException("Cannot have number of uniquely named constants be greater than the size of the category.");
			}
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
				checkForIllegalArguments();
			}
			else {
				throw new IllegalArgumentException("Illegal difference arithmetic theory arguments passed: "+args);
			}
		}
		
		public TheoryTypeInequalityArgs(int numberVariables, int startIntervalInclusive, int endIntervalInclusive) {
			setNumberVariables(numberVariables);
			this.startIntervalInclusive = startIntervalInclusive;
			this.endIntervalInclusive   = endIntervalInclusive;
			checkForIllegalArguments();
		}
		
		public int getStartIntervalInclusive() {
			return startIntervalInclusive;
		}
		
		public int getEndIntervalInclusive() {
			return endIntervalInclusive;
		}
		
		@Override
		public List<String> toCommandLineArgs() {
			return Arrays.asList("-i", "v"+getNumberVariables()+"s"+getStartIntervalInclusive()+"e"+getEndIntervalInclusive());
		}
		
		private void checkForIllegalArguments() {
			if (this.startIntervalInclusive > this.endIntervalInclusive) {
				throw new IllegalArgumentException("Cannot have start of interval be greater than the end of the interval.");
			}
		}
	}
	
	public static final Charset FILE_CHARSET = Charsets.UTF_8;
	
	static class GeneratorArgs implements AutoCloseable {
		// Optional
		Random       random     = null;       // -r 	
		PrintStream  out        = System.out; // -o
		
		// Required
		int numberPotentials; // -n
		int depth;            // -d	
		
		// One instance of the following 3 theory types arguments required at minimum
		TheoryTypePropositionalArgs[] propositionalTheories; // -p propositional - number variables
		TheoryTypeEqualityArgs[]      equalityTheories;      // -e equality      - number variables, size of category, number uniquely names constants
		TheoryTypeInequalityArgs[]    inequalityTheories;    // -i inequality    - number variables, start interval (inclusive), end interval (inclusive)
		
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
	
	public static class GenerateArgs {
		public long seed;
		public int numberPotentials;
		public int depth;
		public TheoryTypeArgs[] theoryTypeArgs;
		
		GenerateArgs(long seed, int numberPotentials, int depth, TheoryTypeArgs... theoryTypeArgs) {
			this.seed             = seed;
			this.numberPotentials = numberPotentials;
			this.depth            = depth;
			this.theoryTypeArgs   = theoryTypeArgs;
		}
	}

	public static void generate(String outputDirectory, GenerateArgs[] generateArgs) {
		File rootOutputDirectory = validateDirectory(outputDirectory);
		File hogmv1ProblemDirectory = new File(rootOutputDirectory, ModelLanguage.HOGMv1.getCode());
		if (!hogmv1ProblemDirectory.exists()) {
			hogmv1ProblemDirectory.mkdir();
		}
		
		for (int a = 0; a < generateArgs.length; a++) {
			GenerateArgs generateArgSet = generateArgs[a];
				
			List<String> args = new ArrayList<>();

			args.add("-r");
			args.add(""+generateArgSet.seed);
			args.add("-n");
			args.add(""+generateArgSet.numberPotentials);
			args.add("-d");
			args.add(""+generateArgSet.depth);
			for (TheoryTypeArgs theoryTypeArgs : generateArgSet.theoryTypeArgs) {
				theoryTypeArgs.toCommandLineArgs().forEach(arg -> args.add(arg));
			}
			StringBuilder outputFileSuffix = new StringBuilder();
			args.forEach(arg -> outputFileSuffix.append(arg));
			String outputFileName = new File(hogmv1ProblemDirectory, "randomModel"+outputFileSuffix+ModelLanguage.HOGMv1.getDefaultFileExtension()).getAbsolutePath();
			args.add("-o");
			args.add(outputFileName);
			
			RandomHOGMv1Generator.main(args.toArray(new String[args.size()]));				
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
			
			// Output the categorical sorts
			for (int c = 0; c < genArgs.equalityTheories.length; c++) {
				TheoryTypeEqualityArgs equalityTheoryArgs = genArgs.equalityTheories[c];
				
				genArgs.out.append(HOGMSortDeclaration.FUNCTOR_SORT_DECLARATION);
				genArgs.out.append(" ");
				genArgs.out.append(getEqualityCategoricalName(c));
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
					
			// Output the random variable information for propositional theories
			for (int i = 0; i < genArgs.propositionalTheories.length; i++) {
				TheoryTypePropositionalArgs propositionalArgs = genArgs.propositionalTheories[i];
				for (int v = 0; v < propositionalArgs.getNumberVariables(); v++) {
					genArgs.out.append("random ");
					genArgs.out.append(genArgs.potentialExpressionGenerator.getPropositionalVariableNameFor(i, v));
					genArgs.out.append(" : ");
					genArgs.out.append(HOGMSortDeclaration.IN_BUILT_BOOLEAN.getName().toString());
					genArgs.out.println(";");
				}
			}
			// Output the random variable information for equality theories
			for (int i = 0; i < genArgs.equalityTheories.length; i++) {
				TheoryTypeEqualityArgs equalityArgs = genArgs.equalityTheories[i];
				for (int v = 0; v < equalityArgs.getNumberVariables(); v++) {
					genArgs.out.append("random ");
					genArgs.out.append(genArgs.potentialExpressionGenerator.getEqualityVariableNameFor(i, v));
					genArgs.out.append(" : ");
					genArgs.out.append(getEqualityCategoricalName(i));
					genArgs.out.println(";");
				}
			}
			// Output the random variable information for the inequality theories
			for (int i = 0; i < genArgs.inequalityTheories.length; i++) {
				TheoryTypeInequalityArgs inequalityArgs = genArgs.inequalityTheories[i];
				for (int v = 0; v < inequalityArgs.getNumberVariables(); v++) {
					genArgs.out.append("random ");
					genArgs.out.append(genArgs.potentialExpressionGenerator.getInequalityVariableNameFor(i, v));
					genArgs.out.append(" : ");
					genArgs.out.append(""+inequalityArgs.getStartIntervalInclusive());
					genArgs.out.append("..");
					genArgs.out.append(""+inequalityArgs.getEndIntervalInclusive());
					genArgs.out.println(";");
				}
			}
			genArgs.out.println();
				
			for (int i = 0; i < genArgs.numberPotentials; i++) {		
				Expression conditional = genArgs.potentialExpressionGenerator.nextPotentialExpression();
				// Ensure we have variables in the conditional
				while (Expressions.freeVariables(conditional, new TrueContext()).size() == 0) {							
					conditional = genArgs.potentialExpressionGenerator.nextPotentialExpression();
				}
				
				genArgs.out.append(conditional.toString());
				genArgs.out.println(";");
			}
		} catch (Exception ex) {
			System.err.println("Error generating random HOGM");
			ex.printStackTrace();
		}
		
	}
	
	public static String getEqualityCategoricalName(int categoricalIndex) {
		return "Equality"+categoricalIndex+"Categorical";
	}
	
	private static GeneratorArgs getArgs(String[] args) throws UnsupportedEncodingException, FileNotFoundException, IOException {
		GeneratorArgs result = new GeneratorArgs();
		
		OptionParser parser = new OptionParser();
		// Optional
		OptionSpec<Integer> randomSeed = parser.accepts("r", "random seed.").withRequiredArg().ofType(Integer.class);
		OptionSpec<File>    outputFile = parser.accepts("o", "output file name (defaults to stdout).").withRequiredArg().ofType(File.class);
		// Required
		OptionSpec<Integer> numPotentials  = parser.accepts("n", "# potentials to generate.").withRequiredArg().required().ofType(Integer.class);
		OptionSpec<Integer> depth          = parser.accepts("d", "the depth of the generated formulas (all their sub-expressions will have depth equal to depth - 1.").withRequiredArg().required().ofType(Integer.class);
		// At least one instance of one of the following required
		OptionSpec<String> propositionalTheoryTypes = parser.accepts("p", "propositional theory type args ('v#' - v)variable #number).").withRequiredArg().ofType(String.class);
		OptionSpec<String> equalityTheoryTypes      = parser.accepts("e", "equality theory type args ('v#c##u' - v)ariable #number, c)ategorical size #number, u)niquely named constants listed in category #number).").withRequiredArg().ofType(String.class);
		OptionSpec<String> inequalityTheoryTypes    = parser.accepts("i", "difference arithmetic theory type args ('v#s#e#' - v)ariable #number, s)tart integer interval inclusive #number, e)nd integer interval inclusive #number).").withRequiredArg().ofType(String.class);
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
		
		
		result.numberPotentials = options.valueOf(numPotentials);
		result.depth            = options.valueOf(depth);
		
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
			throw new IllegalArgumentException("At least one set of propositional, equality, or difference arithmetic theory type args must be provided.");
		}
						
		result.potentialExpressionGenerator = new RandomConditionalPotentialExpressionGenerator(
				result.random, 
				result.propositionalTheories,
				result.equalityTheories,
				result.inequalityTheories,
				result.depth);

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
		
		TheoryTestingSupport theoryTestingSupport = newTheoryTestingSupport(random, propositionTheoryArgs, equalityTheoryArgs, inequalityTheoryArgs);
		Map<String, Type> varToTypeMap = new LinkedHashMap<>();
	
		if (propositionTheoryArgs.length > 0) {
			Categorical booleanCategorical = new Categorical(HOGMSortDeclaration.IN_BUILT_BOOLEAN.getName().toString(), 2, Util.arrayList(Expressions.FALSE, Expressions.TRUE));
			for (int i = 0; i < propositionTheoryArgs.length; i++) {
				RandomHOGMv1Generator.TheoryTypePropositionalArgs propositionalArgs = propositionTheoryArgs[i];
				for (int v = 0; v < propositionalArgs.getNumberVariables(); v++) {
					varToTypeMap.put(getPropositionalVariableNameFor(i, v), booleanCategorical);
				}
			}
		}
		// Output the random variable information for equality theories
		for (int i = 0; i < equalityTheoryArgs.length; i++) {
			RandomHOGMv1Generator.TheoryTypeEqualityArgs equalityArgs = equalityTheoryArgs[i];
			ArrayList<Expression> uniquelyNamedConstants = new ArrayList<>();
			final int categoryIndex = i;
			IntStream.range(0, equalityArgs.getNumberOfUniquelyNamedConstantsInCategory())
				.mapToObj(variableIndex -> makeSymbol(getCategoryUniquelyNamedConstantName(categoryIndex, variableIndex)))
				.forEach(uniquelyNamedConstants::add);
			Categorical equalityCategorical = new Categorical(RandomHOGMv1Generator.getEqualityCategoricalName(i), equalityArgs.getSizeOfCategory(), uniquelyNamedConstants);
			for (int v = 0; v < equalityArgs.getNumberVariables(); v++) {
				varToTypeMap.put(getEqualityVariableNameFor(i, v), equalityCategorical);
			}
		}
		// Output the random variable information for the inequality theories
		for (int i = 0; i < inequalityTheoryArgs.length; i++) {
			RandomHOGMv1Generator.TheoryTypeInequalityArgs inequalityArgs = inequalityTheoryArgs[i];
			IntegerInterval integerInterval = new IntegerInterval(inequalityArgs.getStartIntervalInclusive(), inequalityArgs.getEndIntervalInclusive());
			for (int v = 0; v < inequalityArgs.getNumberVariables(); v++) {
				varToTypeMap.put(getInequalityVariableNameFor(i, v), integerInterval);
			}
		}
		
		theoryTestingSupport.setVariableNamesAndTypesForTesting(varToTypeMap);
		
		Context context = theoryTestingSupport.makeContextWithTestingInformation();
		
		randomConditionalGenerator = new RandomConditionalExpressionGenerator(theoryTestingSupport, depth,
				() -> {
					Symbol lowPrecisionProbability = makeSymbol(new Rational(random.nextInt(100), 100));
					return lowPrecisionProbability;
				},
				context);
	}
	
	public String getPropositionalVariableNameFor(int theoryIndex, int varIndex) {
		return "P" + theoryIndex + "X" + varIndex;
	}
	
	public String getEqualityVariableNameFor(int theoryIndex, int varIndex) {
		return "E" + theoryIndex + "X" + varIndex;
	}
	
	public String getInequalityVariableNameFor(int theoryIndex, int varIndex) {
		return "I" + theoryIndex + "X" + varIndex;
	}
	
	public String getCategoryUniquelyNamedConstantName(int categoryIndex, int uniqueConstantIndex) {
		return "ac" + categoryIndex + "u" + uniqueConstantIndex;
	}
	
	public Expression nextPotentialExpression() {
		return randomConditionalGenerator.apply();
	}
	
	private TheoryTestingSupport newTheoryTestingSupport(Random random,
			RandomHOGMv1Generator.TheoryTypePropositionalArgs[] propositionTheoryArgs, 
			RandomHOGMv1Generator.TheoryTypeEqualityArgs[] equalityTheoryArgs,
			RandomHOGMv1Generator.TheoryTypeInequalityArgs[] inequalityTheoryArgs) {
		List<Theory> theories = new ArrayList<>();
		if (propositionTheoryArgs.length > 0) {
			theories.add(new PropositionalTheory());
		}
		if (equalityTheoryArgs.length > 0) {
			EqualityTheory equalityTheory;
			if (inequalityTheoryArgs.length == 0) {
				// first flag is 'true' because all equalities are atoms in the final theory; there is no need to check arguments type
				equalityTheory = new EqualityTheory(true, true);
			}
			else {
				// 'false' because not all equalities are atoms in this final theory; need to check arguments type
				equalityTheory = new EqualityTheory(false, true);
			}
			theories.add(equalityTheory);
		}
		if (inequalityTheoryArgs.length > 0) {
			DifferenceArithmeticTheory differenceArithmeticTheory;
			if (equalityTheoryArgs.length == 0) {
				// first flag is 'true' because all equalities are atoms in the final theory; there is no need to check arguments type
				differenceArithmeticTheory = new DifferenceArithmeticTheory(true, true);
			}
			else {
				// 'false' because not all equalities are atoms in this final theory; need to check arguments type
				differenceArithmeticTheory = new DifferenceArithmeticTheory(false, true);
			}
			theories.add(differenceArithmeticTheory);
		}
		
		Theory finalTheory;
		if (theories.size() > 1) {
			finalTheory = new CompoundTheory(theories.toArray(new Theory[theories.size()]));
		}
		else {
			finalTheory = theories.get(0);
		}
		
		TheoryTestingSupport result = TheoryTestingSupport.make(random, finalTheory);
		
		return result;
	}
}
