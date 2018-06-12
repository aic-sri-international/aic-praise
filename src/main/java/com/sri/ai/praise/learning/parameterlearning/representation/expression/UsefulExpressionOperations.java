package com.sri.ai.praise.learning.parameterlearning.representation.expression;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.println;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;

public class UsefulExpressionOperations {

	public static void main(String[] args) {
		Theory theory = new CommonTheory();
		
		Context context = new TrueContext(theory);
		
		// X is the child, and Y is the only parent
		
		Expression child = parse("X");
		Expression parent = parse("Y");
		
		context = context.extendWithSymbolsAndTypes("X", "1..5", "Y", "1..5");
		println("My context:");
		println(context.getSymbolsAndTypes());
		
		Expression E = Expressions.parse("if Y != 5 then Alpha1 else Alpha2");
		
		println("\nE = " + E);
		println(Expressions.freeVariables(E, context));
		
		// IndexExpressionsSet indexExpressionsSet = getIndexExpressionsForIndicesInListAndTypesInRegistry((Collection<Expression>) list(child), context); // Gives you <X in 1..5>
		
	}

}
