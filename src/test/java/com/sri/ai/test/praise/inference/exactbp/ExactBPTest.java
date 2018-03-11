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

		//             -------J-------
        //            /               \
		//           /       phi_2     \
		//          /       /     \     \
		//   I -- phi_1 -- L       M   phi_4 -- N
		//          \       \     /     /
		//           \       phi_3     /
		//            \               /
		//             -------K-------
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

		//             -------J-------
        //            /               \
		//           /       phi_2     \
		//          /       /     \     \
		//   I -- phi_1 -- L       M   phi_4 -- N
		//          \       \     /     /
		//           \       phi_3     /
		//            \               /
		//             -------K-------
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

		//             -------J-------
        //            /               \
		//           /       phi_2     \
		//          /       /     \     \
		//   I -- phi_1 -- L       M   phi_4 -- N
		//          \       \     /     /
		//           \       phi_3     /
		//            \               /
		//             -------K-------
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
