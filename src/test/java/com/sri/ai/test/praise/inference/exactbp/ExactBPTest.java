package com.sri.ai.test.praise.inference.exactbp;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Tuple;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.praise.inference.exactbp.api.ExactBP;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.praise.inference.representation.expression.ExpressionExactBP;
import com.sri.ai.praise.inference.representation.expression.ExpressionFactor;
import com.sri.ai.praise.inference.representation.expression.ExpressionFactorNetwork;

public class ExactBPTest {

	@Test
	public void test() {
		
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
	}
	
	private void runTest(String[] variableAndTypes, String factorNetworkString, String queryVariableString, Expression expected) {
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(variableAndTypes);
		ExpressionFactorNetwork factorNetwork = new ExpressionFactorNetwork(factorNetworkString, context);
		Expression query = Expressions.parse(queryVariableString);

		printQuestion(factorNetworkString, queryVariableString, expected);
		
		ExpressionFactor result = solve(query, factorNetwork);
		
		printResults(expected, result);
		
		assertEquals(expected, result.getInnerExpression());
	}

	private ExpressionFactor solve(Expression query, ExpressionFactorNetwork factorNetwork) {
		ExactBP<Variable,Factor> exactBP = new ExpressionExactBP(query, factorNetwork);
		ExpressionFactor result = (ExpressionFactor) exactBP.apply();
		return result;
	}

	private void printQuestion(String factorNetworkString, String queryVariableString, Expression expected) {
		println("\nSolving:\n");
		println(join(((Tuple)parse(factorNetworkString)).getArguments(), "\n"));
		println();
		println("Query: " + queryVariableString);
		println("Expected: " + expected);
		println("...");
	}

	private void printResults(Expression expected, ExpressionFactor result) {
		println("Result: " + result);
		println("Expected: " + expected);
		println(expected.equals(result.getInnerExpression())? "Correct!" : "Error!");
	}

}
