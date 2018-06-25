package com.sri.ai.test.praise.learning.parameterlearning;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;

import java.util.LinkedHashSet;
import java.util.LinkedList;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.ExpressionVariable;
import com.sri.ai.praise.learning.parameterlearning.representation.expression.ExpressionBayesianModel;
import com.sri.ai.praise.learning.parameterlearning.representation.expression.ExpressionBayesianNode;
import com.sri.ai.util.Util;

/**
 * CLass to test the implementation of parameter learning for Bayesian nodes (using Expressions)
 * 
 * @author Roger Leite Lucena
 *
 */

public class ExpressionBayesianModelTest {

	// Simple model with 2 nodes: ChildNode and ParentNode
	private static ExpressionBayesianModel generateChildParentModel(ExpressionVariable childVariable, Expression childNodeExpression, LinkedHashSet<Expression> parametersForChildNode, 
			ExpressionVariable parentVariable, Expression parentNodeExpression, LinkedHashSet<Expression> parametersForParentNode, Context context) {
		// Theory theory = new CommonTheory();
		
		// Context context = new TrueContext(theory);
				
		// Only one child and one parent
		
		// ExpressionVariable childVariable = new ExpressionVariable(parse("Child"));
		// ExpressionVariable parentVariable = new ExpressionVariable(parse("Parent"));
		
		// Expression param1 = parse("Param1");
		// Expression param2 = parse("Param2");
		// Expression param3 = parse("Param3");
		
		// context = context.extendWithSymbolsAndTypes("Child", "1..5", "Parent", "1..5", "Param1", "Real", "Param2", "Real", "Param3", "Real");
		
		// LinkedHashSet<Expression> parametersForChildNode = Util.set(param1, param2);
		//parametersForChildNode.add(param3);
		
		// Expression childNodeExpression = parse("if Child < 5 then Param1 else Param2");
		// Expression childNodeExpression = parse("if Parent != 5 then Param1 else Param2");
		// Expression childNodeExpression = parse("if Parent != 5 then if Child < 5 then Param1 else Param2 else Param3");
		// Expression childNodeExpression = parse("if Parent != 5 then if Child < Parent then Param1 else Param2 else Param3"); // partial intersection
		
		LinkedList<ExpressionVariable> parentsForChildNode = list(parentVariable);
		
		ExpressionBayesianNode childNode = new ExpressionBayesianNode(childNodeExpression, context, childVariable, parentsForChildNode, parametersForChildNode);
		ExpressionBayesianNode parentNode = new ExpressionBayesianNode(parentNodeExpression, context, parentVariable, list(), parametersForParentNode);
		
		ExpressionBayesianModel model = new ExpressionBayesianModel(list(childNode, parentNode), context);
		
		return model;
	}
	
	public static void main(String[] args) {
		
	}

}
