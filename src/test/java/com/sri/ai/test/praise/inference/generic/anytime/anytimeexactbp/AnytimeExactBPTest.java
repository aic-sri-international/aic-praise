package com.sri.ai.test.praise.inference.generic.anytime.anytimeexactbp;

import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.junit.Test;

import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.Tuple;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.bounds.Bound;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.equality.EqualityTheory;
import com.sri.ai.grinder.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.theory.tuple.TupleTheory;
import com.sri.ai.praise.PRAiSEUtil;
import com.sri.ai.praise.inference.generic.anytime.anytimeexactbp.AnytimeExactBP;
import com.sri.ai.praise.inference.generic.anytime.polytope.core.IntensionalConvexHullOfFactors;
import com.sri.ai.praise.inference.generic.exactbp.api.ExactBP;
import com.sri.ai.praise.inference.generic.exactbp.core.AbstractExactBP;
import com.sri.ai.praise.inference.generic.representation.api.Factor;
import com.sri.ai.praise.inference.generic.representation.api.Variable;
import com.sri.ai.praise.inference.generic.representation.expression.ExpressionExactBP;
import com.sri.ai.praise.inference.generic.representation.expression.ExpressionFactor;
import com.sri.ai.praise.inference.generic.representation.expression.ExpressionFactorNetwork;
import com.sri.ai.util.base.IdentityWrapper;
import com.sri.ai.util.computation.anytime.api.Approximation;

import IncrementalAnytimeExactBeliefPropagation.IncrementalAnytimeBeliefPropagationWithSeparatorConditioning;
import IncrementalAnytimeExactBeliefPropagation.PartitionTree;
import IncrementalAnytimeExactBeliefPropagation.Model.BFS;
import IncrementalAnytimeExactBeliefPropagation.Model.Model;


public class AnytimeExactBPTest {

	@Test
	public void test() {
		
		ExpressoConfiguration.setDisplayNumericsExactlyForSymbols(false);
		ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInApproximateRepresentationOfNumericalSymbols(3);

		AbstractExactBP.debug = false;
		
		String[] variableAndTypes;
		String factorNetworkString;
		String queryVariableString;
		Expression expected;

		variableAndTypes = new String[]{"I", "1..10", "P", "Boolean"};
		factorNetworkString = "tuple("
				+ "if P then if I = 1 then 2 else 3 else if I = 1 then 1 else 2"
				+ ")";
		queryVariableString = "I";
		expected = parse("if I = 1 then 3 else 5");
		runTest(variableAndTypes, factorNetworkString, queryVariableString, expected);

		variableAndTypes = new String[]{"I", "1..10", "P", "Boolean"};
		factorNetworkString = "("
				+ "if (P or not P) and I = 1 then 0.1 else 0.9, "
				+ "if (P or not P) and I = 1 then 0.1 else 0.9"
				+ ")";
		queryVariableString = "I";
		expected = parse("if I = 1 then 0.01 else 0.81");
		runTest(variableAndTypes, factorNetworkString, queryVariableString, expected);
		
		variableAndTypes = new String[]{"I", "1..10", "J", "1..10", "P", "Boolean"};
		factorNetworkString = "("
				+ "if J = I then 1 else 0, "
				+ "if (P or not P) and J = 1 then 0.1 else 0.9, "
				+ "if (P or not P) and I = 1 then 0.1 else 0.9"
				+ ")";
		queryVariableString = "I";
		expected = parse("if I = 1 then 0.01 else 0.81");
		runTest(variableAndTypes, factorNetworkString, queryVariableString, expected);
		
		//              -------J-------
        //             /               \
		//            /      phi_2      \
		//           /      /     \      \
		//   I -- phi_1 -- L       M   phi_4 -- N
		//           \      \     /      /
		//            \      phi_3      /
		//             \               /
		//              -------K-------
		variableAndTypes = new String[]{"I", "1..10", "J", "1..10", "K", "1..10", "L", "1..10", "M", "1..10", "N", "1..10"};
		factorNetworkString = "("
				+ "if I = J and I = L and I = K then 1 else 0, "
				+ "if L = M then 1 else 0, "
				+ "if L = M then 1 else 0, "
				+ "if N = J and N = K then 1 else 0, "
				// Evidence: both M and N are equal to 2. Since all factors enforce equality, I should be 2
				+ "if M = 2 then 1 else 0, "
				+ "if N = 2 then 1 else 0 "
				+ ")";
		queryVariableString = "I";
		expected = parse("if I = 2 then 1 else 0");
		runTest(variableAndTypes, factorNetworkString, queryVariableString, expected);
		
		//              -------J-------
        //             /               \
		//            /      phi_2      \
		//           /      /     \      \
		//   I -- phi_1 -- L       M   phi_4 -- N
		//           \      \     /      /
		//            \      phi_3      /
		//             \               /
		//              -------K-------
		variableAndTypes = new String[]{"I", "1..10", "J", "1..10", "K", "1..10", "L", "1..10", "M", "1..10", "N", "1..10"};
		factorNetworkString = "("
				+ "if I = J and I = L and I = K then 1 else 0, "
				+ "if L = M then 1 else 0, "
				+ "if L = M then 1 else 0, "
				+ "if N = J and N = K then 1 else 0, "
				// Evidence: both M and N are equally likely to be 2 or 3, therefore so should I
				+ "if M = 2 then 0.5 else if M = 3 then 0.5 else 0, "
				+ "if N = 2 then 0.5 else if N = 3 then 0.5 else 0 "
				+ ")";
		queryVariableString = "I";
		expected = parse("if I = 2 then 0.25 else if I = 3 then 0.25 else 0"); // Note: ExactBP returns an arbitrary unnormalized message
		runTest(variableAndTypes, factorNetworkString, queryVariableString, expected);

		//              -------J-------
        //             /               \
		//            /      phi_2      \
		//           /      /     \      \
		//   I -- phi_1 -- L       M   phi_4 -- N
		//           \      \     /      /
		//            \      phi_3      /
		//             \               /
		//              -------K-------
		variableAndTypes = new String[]{"I", "1..10", "J", "1..10", "K", "1..10", "L", "1..10", "M", "1..10", "N", "1..10"};
		factorNetworkString = "("
				+ "if I = J and I = L and I = K then 1 else 0, "
				+ "if L = M then 1 else 0, "
				+ "if L = M then 1 else 0, "
				+ "if N = J and N = K then 1 else 0, "
				// Evidence: M can be 2 or 3, N can be 3 or 4; since I has to be equal to both, it must be 3
				+ "if M = 2 then 0.5 else if M = 3 then 0.5 else 0, "
				+ "if N = 3 then 0.5 else if N = 4 then 0.5 else 0 "
				+ ")";
		queryVariableString = "I";
		expected = parse("if I = 3 then 0.25 else 0"); // Note: ExactBP returns an arbitrary unnormalized message
		runTest(variableAndTypes, factorNetworkString, queryVariableString, expected);

		//              -------J-------
        //             /               \
		//            /      phi_2      \
		//           /      /     \      \
		//   I -- phi_1 -- L       M   phi_4 -- N
		//           \      \     /      /
		//            \      phi_3      /
		//             \               /
		//              -------K-------
		variableAndTypes = new String[]{"I", "1..10", "J", "1..10", "K", "1..10", "L", "1..10", "M", "1..10", "N", "1..10"};
		factorNetworkString = "("
				+ "if I = J and I = L and I = K then 1 else 0, "
				+ "if L = M then 1 else 0, "
				+ "if L = M then 1 else 0, "
				+ "if N = J and N = K then 1 else 0, "
				// Evidence: M can be 2 or 3 with probabilities 1/4 and 3/4, N can be also 2 or 3 but with flipped probabilities
				// this will lead to I being 2 or 3 with uniform probability
				+ "if M = 2 then 0.25 else if M = 3 then 0.75 else 0, "
				+ "if N = 2 then 0.75 else if N = 3 then 0.25 else 0 "
				+ ")";
		queryVariableString = "I";
		expected = parse("if I = 2 then 0.1875 else if I = 3 then 0.1875 else 0"); // Note: ExactBP returns an arbitrary unnormalized message
		runTest(variableAndTypes, factorNetworkString, queryVariableString, expected);

		//
		// A00 -- A01 -- ... -- A04
		//  |      |             |
		// A10 -- A11 -- ... -- A14
		//  .	   .    .        .
		//  .	   .      .      .
		//  .	   .        .    .
		//  |      |          .  |
		// A40 -- A41 -- ... -- A44
		//
		variableAndTypes = new String[]{"A_0_0", "Boolean","A_1_0", "Boolean","A_2_0", "Boolean","A_3_0", "Boolean","A_4_0", "Boolean",
										"A_0_1", "Boolean","A_1_1", "Boolean","A_2_1", "Boolean","A_3_1", "Boolean","A_4_1", "Boolean",
										"A_0_2", "Boolean","A_1_2", "Boolean","A_2_2", "Boolean","A_3_2", "Boolean","A_4_2", "Boolean",
										"A_0_3", "Boolean","A_1_3", "Boolean","A_2_3", "Boolean","A_3_3", "Boolean","A_4_3", "Boolean",
										"A_0_4", "Boolean","A_1_4", "Boolean","A_2_4", "Boolean","A_3_4", "Boolean","A_4_4", "Boolean"};
		factorNetworkString = "(" 
				+ "if A_1_1 = false then if A_1_2 = false then 0 else if A_1_2 = true then 3 else 0 else if A_1_1 = true then if A_1_2 = false then 2 else if A_1_2 = true then 3 else 0 else 0, "
				+ "if A_1_1 = false then if A_2_1 = false then 3 else if A_2_1 = true then 4 else 0 else if A_1_1 = true then if A_2_1 = false then 7 else if A_2_1 = true then 8 else 0 else 0, "
				+ "if A_2_1 = false then if A_2_2 = false then 3 else if A_2_2 = true then 8 else 0 else if A_2_1 = true then if A_2_2 = false then 8 else if A_2_2 = true then 7 else 0 else 0, "
				+ "if A_1_3 = false then if A_2_3 = false then 4 else if A_2_3 = true then 6 else 0 else if A_1_3 = true then if A_2_3 = false then 7 else if A_2_3 = true then 4 else 0 else 0, "
				+ "if A_3_2 = false then if A_3_3 = false then 1 else if A_3_3 = true then 4 else 0 else if A_3_2 = true then if A_3_3 = false then 5 else if A_3_3 = true then 1 else 0 else 0, "
				+ "if A_1_0 = false then if A_2_0 = false then 3 else if A_2_0 = true then 3 else 0 else if A_1_0 = true then if A_2_0 = false then 4 else if A_2_0 = true then 8 else 0 else 0, "
				+ "if A_2_0 = false then if A_3_0 = false then 3 else if A_3_0 = true then 0 else 0 else if A_2_0 = true then if A_3_0 = false then 0 else if A_3_0 = true then 8 else 0 else 0, "
				+ "if A_3_0 = false then if A_3_1 = false then 5 else if A_3_1 = true then 4 else 0 else if A_3_0 = true then if A_3_1 = false then 6 else if A_3_1 = true then 1 else 0 else 0, "
				+ "if A_0_0 = false then if A_0_1 = false then 7 else if A_0_1 = true then 3 else 0 else if A_0_0 = true then if A_0_1 = false then 4 else if A_0_1 = true then 2 else 0 else 0, "
				+ "if A_1_0 = false then if A_1_1 = false then 7 else if A_1_1 = true then 8 else 0 else if A_1_0 = true then if A_1_1 = false then 6 else if A_1_1 = true then 7 else 0 else 0, "
				+ "if A_2_2 = false then if A_3_2 = false then 5 else if A_3_2 = true then 8 else 0 else if A_2_2 = true then if A_3_2 = false then 5 else if A_3_2 = true then 5 else 0 else 0, "
				+ "if A_0_2 = false then if A_0_3 = false then 5 else if A_0_3 = true then 3 else 0 else if A_0_2 = true then if A_0_3 = false then 7 else if A_0_3 = true then 0 else 0 else 0, "
				+ "if A_0_2 = false then if A_1_2 = false then 2 else if A_1_2 = true then 4 else 0 else if A_0_2 = true then if A_1_2 = false then 2 else if A_1_2 = true then 0 else 0 else 0, "
				+ "if A_2_3 = false then if A_3_3 = false then 7 else if A_3_3 = true then 5 else 0 else if A_2_3 = true then if A_3_3 = false then 7 else if A_3_3 = true then 7 else 0 else 0, "
				+ "if A_2_1 = false then if A_3_1 = false then 7 else if A_3_1 = true then 4 else 0 else if A_2_1 = true then if A_3_1 = false then 3 else if A_3_1 = true then 3 else 0 else 0, "
				+ "if A_3_1 = false then if A_3_2 = false then 0 else if A_3_2 = true then 0 else 0 else if A_3_1 = true then if A_3_2 = false then 4 else if A_3_2 = true then 3 else 0 else 0, "
				+ "if A_0_1 = false then if A_0_2 = false then 6 else if A_0_2 = true then 5 else 0 else if A_0_1 = true then if A_0_2 = false then 0 else if A_0_2 = true then 6 else 0 else 0, "
				+ "if A_0_1 = false then if A_1_1 = false then 5 else if A_1_1 = true then 5 else 0 else if A_0_1 = true then if A_1_1 = false then 3 else if A_1_1 = true then 2 else 0 else 0, "
				+ "if A_1_2 = false then if A_1_3 = false then 7 else if A_1_3 = true then 3 else 0 else if A_1_2 = true then if A_1_3 = false then 2 else if A_1_3 = true then 2 else 0 else 0, "
				+ "if A_1_2 = false then if A_2_2 = false then 7 else if A_2_2 = true then 5 else 0 else if A_1_2 = true then if A_2_2 = false then 6 else if A_2_2 = true then 4 else 0 else 0, "
				+ "if A_2_0 = false then if A_2_1 = false then 0 else if A_2_1 = true then 5 else 0 else if A_2_0 = true then if A_2_1 = false then 8 else if A_2_1 = true then 6 else 0 else 0, "
				+ "if A_0_0 = false then if A_1_0 = false then 5 else if A_1_0 = true then 6 else 0 else if A_0_0 = true then if A_1_0 = false then 4 else if A_1_0 = true then 3 else 0 else 0, "
				+ "if A_0_3 = false then if A_1_3 = false then 1 else if A_1_3 = true then 3 else 0 else if A_0_3 = true then if A_1_3 = false then 2 else if A_1_3 = true then 1 else 0 else 0, "
				+ "if A_2_2 = false then if A_2_3 = false then 6 else if A_2_3 = true then 1 else 0 else if A_2_2 = true then if A_2_3 = false then 3 else if A_2_3 = true then 3 else 0 else 0"
				+ ")";
		
		queryVariableString = "A_0_0";
		expected = parse("if not A_0_0 then 2515404149056770048 else 857920100616142848"); // Note: ExactBP returns an arbitrary unnormalized message
		runTest(variableAndTypes, factorNetworkString, queryVariableString, expected);
		
		// Commented out for testing time purposes 
		
//		// Importing the file and reading it
//		FileReader modelFile;
//		try {
//		modelFile = new FileReader(new File("").getAbsolutePath()+"/UAITests/BN_0.uai" );
//		
//		UAIModel model = UAIModelReader.read(modelFile);
//					
//		// Converting the network
//		ExpressionFactorNetwork network = UAIModelToExpressionFactorNetwork.convert(model, null);
//		
//		solveWithAnytimeExactBP(parse("v66"), network, network.getContext());
//		
//		} catch (FileNotFoundException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		} catch (IOException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}			
		
	}
	
	private void runTest(String[] variableAndTypes, String factorNetworkString, String queryVariableString, Expression expected) {
		runRodrigos(variableAndTypes, factorNetworkString, queryVariableString, expected);
		runGabriels(variableAndTypes, factorNetworkString, queryVariableString, expected);
	}

	private void runRodrigos(String[] variableAndTypes, String factorNetworkString, String queryVariableString, Expression expected) {
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(variableAndTypes);
		ExpressionFactorNetwork factorNetwork = new ExpressionFactorNetwork(factorNetworkString, context);
		Expression query = Expressions.parse(queryVariableString);

		printProblem(factorNetworkString, queryVariableString, expected);
		println("Solving P(" + queryVariableString + ") given " + factorNetworkString);
		
		runRodrigos(factorNetwork, query,expected);
	}

	private void runRodrigos(ExpressionFactorNetwork factorNetwork, Expression query,Expression expected) {
		Context context = factorNetwork.getContext();
		long initialTime = System.currentTimeMillis();
		ExpressionFactor resultFactor = solveWithExactBP(query, factorNetwork);
		Expression normalizedResult = PRAiSEUtil.normalize(query, resultFactor.getInnerExpression(), context);
		long finalTime = System.currentTimeMillis();
		println("ExactBP: " + normalizedResult);
		println("Time: " + (finalTime - initialTime) + " ms.");
		
		printResults(expected, resultFactor, normalizedResult);
		
		assertEquals(expected, resultFactor.getInnerExpression());
		
		Expression normalizedAnytimeResult = solveWithAnytimeExactBP(query, factorNetwork, context);
		
//		Expression test = parse("there exists C in Real : ((C*(" + resultFactor.getInnerExpression() + ")) = " + normalizedAnytimeResult + ")");
//		println("Solving " + test);
//		Expression testResult = context.evaluate(test);
//		assertEquals(TRUE, testResult);

		Expression test = parse("(" + normalizedResult + ") = (" + normalizedAnytimeResult + ")");
		Expression testResult = context.evaluate(test);
		assertEquals(TRUE, testResult);

//		assertEquals(normalizedResult, normalizedAnytimeResult);
	}

	private ExpressionFactor solveWithExactBP(Expression query, ExpressionFactorNetwork factorNetwork) {
		ExactBP<Variable,Factor> exactBP = new ExpressionExactBP(query, factorNetwork);
		ExpressionFactor result = (ExpressionFactor) exactBP.apply();
		return result;
	}

	private Expression solveWithAnytimeExactBP(Expression query, ExpressionFactorNetwork factorNetwork, Context context) {
		
		Expression result;
		
		println("\nSolving with Anytime\n");
		
		long initialTime = System.currentTimeMillis();
		ExactBP<Variable,Factor> exactBP = new ExpressionExactBP(query, factorNetwork);
		AnytimeExactBP<Variable,Factor> anytimeExactBP = new AnytimeExactBP<>(exactBP);
		Approximation<Factor> current = null;
		while (anytimeExactBP.hasNext()) {
			current = anytimeExactBP.next();
			println("Current bound on " + exactBP.getMessageVariable() + ": " + current);
		}
		long finalTime = System.currentTimeMillis();

		if (current == null) {
			throw new Error("Anytime BP should have at least one approximation, but had none.");
		}
		else {
			ExpressionFactor resultFactor = (ExpressionFactor) ((IntensionalConvexHullOfFactors) current).getFactor();
			result = PRAiSEUtil.normalize(query, resultFactor.getInnerExpression(), context);
			println("P(" + exactBP.getMessageVariable() + "): " + result);
			println("Time: " + (finalTime - initialTime) + " ms.");
		}
		return result;
	}

	private void printProblem(String factorNetworkString, String queryVariableString, Expression expected) {
		println("\nSolving:\n");
		println(join(((Tuple)parse(factorNetworkString)).getArguments(), "\n"));
		println();
		println("Query: " + queryVariableString);
		println("Expected: " + expected);
		println("...");
	}

	private void printResults(Expression expected, ExpressionFactor resultFactor, Expression result) {
		println("Result factor: " + resultFactor);
		println("Normalized   : " + result);
		println("Expected     : " + expected);
		println(expected.equals(resultFactor.getInnerExpression())? "Correct!" : "Error!");
	}


	private void runGabriels( String[] variableAndTypes, String factorNetworkString, String queryVariableString, Expression expected) {
		Theory theory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new TupleTheory(),
				new PropositionalTheory());
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(variableAndTypes);
		ExpressionFactorNetwork factorNetwork = new ExpressionFactorNetwork(factorNetworkString, context);
		Expression query = Expressions.parse(queryVariableString);
		
		Set<Expression> setOfFactors = new HashSet<>(); // not sure it will work
		for(IdentityWrapper iw:factorNetwork.getAs()) {
			ExpressionFactor f = (ExpressionFactor) iw.getObject();
			Expression expressionFactor = f.getInnerExpression();
			boolean successfullyAdded = setOfFactors.add(expressionFactor);
			if (!successfullyAdded) {
				setOfFactors.remove(expressionFactor);
				Expression squareFactor = apply("*", expressionFactor,expressionFactor);
				squareFactor = theory.evaluate(squareFactor, context);
				setOfFactors.add(squareFactor);
			}
		}
		
		// create model
		Model m = new Model(setOfFactors, theory, context,false, query);
		
		// do all iterations until the end, storing time
		Iterator<PartitionTree> bfsExpander = new BFS(m);
		IncrementalAnytimeBeliefPropagationWithSeparatorConditioning sbp = 
				new IncrementalAnytimeBeliefPropagationWithSeparatorConditioning(m, bfsExpander);
	
		long initialTime = System.currentTimeMillis();
		Bound inferenceResult = null;
		println("----------------solving with Gabriels----------------");
		while (bfsExpander.hasNext()) {
			inferenceResult = sbp.expandAndComputeInference();
			
			println("Current bound on "+ query + ": " +inferenceResult);//.normalize(theory, context));
		}
		long finalTime = System.currentTimeMillis();
		
		Expression normalizedResult = inferenceResult.normalize(theory, context);
		normalizedResult = ((IntensionalSet)normalizedResult).getHead();
		Expression normalizedexpected = PRAiSEUtil.normalize(query,expected,context);
		
		println("Result factor: " + ((IntensionalSet)inferenceResult).getHead());
		println("Normalized   : " + normalizedResult);
		//print the way it is done above
		println("Time: " + (finalTime - initialTime) + " ms.");	

		println(normalizedexpected.equals(normalizedResult)? "Correct!" : "Error!");
		
		
		Expression test = parse("(" + normalizedResult + ") = (" + normalizedexpected + ")");
		Expression testResult = context.evaluate(test);
		assertEquals(TRUE, testResult);
	}	
}
