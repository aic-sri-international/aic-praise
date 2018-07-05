package com.sri.ai.praise.learning.parameterlearning.representation.expression;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsForIndicesInListAndTypesInRegistry;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.DefaultExistentiallyQuantifiedFormula;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Equivalence;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;
import com.sri.ai.util.Util;

public class UsefulExpressionOperations {
	
	/**
	 * Given two conditions as Expressions, verify if they are equivalent or return the condition for their intersection
	 * @param e1
	 * @param e2
	 * @param context
	 * @return true if total equivalence, false if the conditions do not intersect, and in the case of partial intersection the condition for this intersection is returned
	 */
	private static Expression verifyEquivalenceAndGetIntersectionCondition(Expression e1, Expression e2, Context context) {
		Expression equivalence = Equivalence.make(e1, e2);
		equivalence = context.evaluate(equivalence);
		if(equivalence.equals(Expressions.TRUE)) {
			return equivalence;
		}
		else {
			Expression and = And.make(e1, e2);
			and = context.evaluate(and);
			return and;
		}
	}

	public static void main(String[] args) {
		Theory theory = new CommonTheory();
		
		Context context = new TrueContext(theory);
		
		// Only one child and one parent, 2 parameters (Param1 and Param2)
		
		Expression child = parse("Child");
		Expression parent = parse("Parent");
		Expression param1 = parse("Param1");
		Expression param2 = parse("Param2");
		Expression param3 = parse("Param3");
		
		context = context.extendWithSymbolsAndTypes("Child", "1..5", "Parent", "1..5", "Param1", "Real", "Param2", "Real", "Param3", "Real", "A", "Boolean");
		
		// Making parameters become constants
		Predicate<Expression> isUniquelyNamedConstantPredicate = context.getIsUniquelyNamedConstantPredicate();
		Predicate<Expression> newIsUniquelyNamedConstantPredicate = s -> s.equals(param1) || s.equals(param2) || s.equals(param3) || isUniquelyNamedConstantPredicate.apply(s);
		context = context.setIsUniquelyNamedConstantPredicate(newIsUniquelyNamedConstantPredicate);
		
		println("My context:");
		println(context.getSymbolsAndTypes());
		
		// Expression E = parse("if Child < 5 then Param1 else Param2");
		Expression E = parse("if Parent != 5 then Param1 else Param2");
		// Expression E = parse("if Parent != 5 then if Child < 5 then Param1 else Param2 else Param3");
		// Expression E = parse("if Parent != 5 then if Child < Parent then Param1 else Param2 else Param3");
		
		println("\nE = " + E + "\n");
		
		IndexExpressionsSet childIndexExpressionsSet = getIndexExpressionsForIndicesInListAndTypesInRegistry(list(child), context); // Gives you <Child in 1..5>
		
		Expression F1 = new DefaultExistentiallyQuantifiedFormula(childIndexExpressionsSet, Equality.make(E, param1));
		println("F1 = " + F1);
		println(context.evaluate(F1) + "\n");
		
		Expression F2 = new DefaultExistentiallyQuantifiedFormula(childIndexExpressionsSet, Equality.make(E, param2));
		println("F2 = " + F2);
		println(context.evaluate(F2) + "\n");
		
		Expression F1intersectsF2 = verifyEquivalenceAndGetIntersectionCondition(F1, F2, context); // Equivalence.make(F1, F2), usar o context para simplificar com o F1 sabendo que é true ??? ou criar um existe cara ... bonitinho como deve ser? there exist Parent tal que F1 = true ...
		println("F1intersectsF2 = " + F1intersectsF2);
		println(context.evaluate(F1intersectsF2)); 
		
		// Normalization for Parame1_1
		Expression multiset = new DefaultIntensionalMultiSet(childIndexExpressionsSet, child, Equality.make(E, param1));
		Expression cardinality = apply(CARDINALITY, multiset);
		println("\nCardinality = " + cardinality);
		Expression cardinalityResult = context.evaluate(cardinality);
		println("N for normalizing Param1_1: " + cardinalityResult);
		
		// Draft for the Java syntax when shattering the families
		LinkedList<Integer> initialFamilies = Util.list(1, 2, 3, 4, 5);
		List<Integer> finalFamilies = Util.list();
		while(!initialFamilies.isEmpty()) {
			int family1 = initialFamilies.removeFirst();
			
			for(Iterator<Integer> it = initialFamilies.iterator(); it.hasNext();) {
				int family2 = it.next();
				if(family2 == 4) {it.remove();}
			}
			
			if(family1 == 3) initialFamilies.add(7); 
			finalFamilies.add(family1);
		}
		println("\n" + finalFamilies);
	
		
		// Testing syntax for context (used for method convertToAnExpressionBasedModelAfterLearning in ExpressionBayesianModel)
		ExpressionVariable expressionVariable = new DefaultExpressionVariable(parse("A"));
		println("\nexpressionVariable.toString() = " + expressionVariable.toString());
		Type type = context.getTypeOfRegisteredSymbol(expressionVariable);
		println("type.toString() = " + type.toString());
		println("child.toStringe() = " + child.toString());
		type = context.getTypeOfRegisteredSymbol(child);
		println("type.toString() = " + type.toString());
	}

}
