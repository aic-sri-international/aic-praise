package com.sri.ai.praise.learning.parameterlearning.representation.expression;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsForIndicesInListAndTypesInRegistry;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;

import java.util.Iterator;
import java.util.LinkedHashSet;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.core.DefaultExistentiallyQuantifiedFormula;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Equivalence;
import com.sri.ai.util.Util;

public class UsefulExpressionOperations {
	

	public static void main(String[] args) {
		Theory theory = new CommonTheory();
		
		Context context = new TrueContext(theory);
		
		// Only one child and one parent, 2 parameters (Param1 and Param2)
		
		Expression child = parse("Child");
		Expression parent = parse("Parent");
		Expression param1 = parse("Param1");
		Expression param2 = parse("Param2");
		Expression param3 = parse("Param3");
		
		context = context.extendWithSymbolsAndTypes("Child", "1..5", "Parent", "1..5", "Param1", "Integer", "Param2", "Integer", "Param3", "Integer");
		
		// Making parameters become constants
		Predicate<Expression> isUniquelyNamedConstantPredicate = context.getIsUniquelyNamedConstantPredicate();
		Predicate<Expression> newIsUniquelyNamedConstantPredicate = s -> s.equals(param1) || s.equals(param2) || s.equals(param3) || isUniquelyNamedConstantPredicate.apply(s);
		context = context.setIsUniquelyNamedConstantPredicate(newIsUniquelyNamedConstantPredicate);
		
		println("My context:");
		println(context.getSymbolsAndTypes());
		
		// Expression E = parse("if Child < 5 then Param1 else Param2");
		// Expression E = parse("if Parent != 5 then Param1 else Param2");
		// Expression E = parse("if Parent != 5 then if Child < 5 then Param1 else Param2 else Param3");
		Expression E = parse("if Parent != 5 then if Child < Parent then Param1 else Param2 else Param3");
		
		println("\nE = " + E + "\n");
		
		IndexExpressionsSet childIndexExpressionsSet = getIndexExpressionsForIndicesInListAndTypesInRegistry(list(child), context); // Gives you <Child in 1..5>
		
		Expression F1 = new DefaultExistentiallyQuantifiedFormula(childIndexExpressionsSet, Equality.make(E, param1));
		println("F1 = " + F1);
		println(context.evaluate(F1) + "\n");
		
		Expression F2 = new DefaultExistentiallyQuantifiedFormula(childIndexExpressionsSet, Equality.make(E, param2));
		println("F2 = " + F2);
		println(context.evaluate(F2) + "\n");
		
		Expression F1intersectsF2 = Equivalence.make(F1, F2); // Equivalence.make(F1, F2), usar o context para simplificar com o F! sabendo que é true ??? ou criar um existe cara ... bonitinho como deve ser? there exist Parent tal que F1 = true ...
		println("F1intersectsF2 = " + F1intersectsF2);
		println(context.evaluate(F1intersectsF2)); // should be true
		
		// Normalization for Parame1_1
		Expression multiset = new DefaultIntensionalMultiSet(childIndexExpressionsSet, child, Equality.make(E, param1));
		Expression cardinality = apply(CARDINALITY, multiset);
		println("\nCardinality = " + cardinality);
		Expression cardinalityResult = context.evaluate(cardinality);
		println("N for normalizing Param1_1: " + cardinalityResult);
		
		LinkedHashSet<Integer> set = Util.set(1, 2, 3);
		Iterator<Integer> it = set.iterator();
		while(it.hasNext()) {
			int curr = it.next();
			println(curr);
			// if(curr == 2) set.add(4);
		}
		
		// 2 problems:
		// - Not possible to add while iterating through the LinkedHashSet (above)
		// - Comment that <=> is not sufficient for what we want
	}

}
