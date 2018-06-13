package com.sri.ai.praise.learning.parameterlearning.representation.expression;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsForIndicesInListAndTypesInRegistry;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.core.DefaultExistentiallyQuantifiedFormula;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.Equivalence;

public class UsefulExpressionOperations {

	public static void main(String[] args) {
		Theory theory = new CommonTheory();
		
		Context context = new TrueContext(theory);
		
		// Only one child and one parent, 2 parameters (Param1 and Param2)
		
		Expression child = parse("Child");
		Expression parent = parse("Parent");
		Expression param1 = parse("Param1");
		Expression param2 = parse("Param2");
		
		context = context.extendWithSymbolsAndTypes("Child", "1..5", "Parent", "1..5", "Param1", "Real", "Param2", "Real");
		println("My context:");
		println(context.getSymbolsAndTypes());
		
		Expression E = parse("if Child < 5 then Param1 else Param2");
		
		println("\nE = " + E + "\n");
		
		IndexExpressionsSet childIndexExpressionsSet = getIndexExpressionsForIndicesInListAndTypesInRegistry(list(child), context); // Gives you <Child in 1..5>
		
		Expression F1 = new DefaultExistentiallyQuantifiedFormula(childIndexExpressionsSet, Equality.make(E, param1));
		println("F1 = " + F1);
		println(context.evaluate(F1) + "\n");
		
		Expression F2 = new DefaultExistentiallyQuantifiedFormula(childIndexExpressionsSet, Equality.make(E, param2));
		println("F2 = " + F2);
		println(context.evaluate(F2) + "\n");
		
		Expression F1intersectsF2 = Equivalence.make(F1, F2);
		println("F1intersectsF2 = " + F1intersectsF2);
		println(context.evaluate(F1intersectsF2)); // should be true
		
		Expression Enew = E.replaceAllOccurrences(param1, parse("Param1_1"), context);
		Enew = Enew.replaceAllOccurrences(param2, parse("Param2_1"), context);
		
		context = context.extendWithSymbolsAndTypes("Param1_1", "Real", "Param2_1", "Real");
		
		println("\nEnew = " + Enew);
		
		// Normalization for Parame1_1
		Expression multiset = new DefaultIntensionalMultiSet(childIndexExpressionsSet, child, Equality.make(Enew, parse("Param1_1")));
		Expression cardinality = apply(CARDINALITY, multiset);
		println("\nCardinality = " + cardinality);
		//Expression cardinalityResult = context.evaluate(cardinality);
		//println("\nN for normalizing Param1_1: " + cardinalityResult);
		
		// println(context.evaluate(parse(child + " > 3 <=> " + child + " < 5")));
	}

}
