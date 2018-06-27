package com.sri.ai.test.praise.learning.parameterlearning;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;
import com.sri.ai.praise.learning.parameterlearning.representation.dataset.DefaultDatapoint;
import com.sri.ai.praise.learning.parameterlearning.representation.dataset.DefaultDataset;
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
	
	static ExpressionVariable childVariable = new DefaultExpressionVariable(parse("Child"));
	static ExpressionVariable parentVariable = new DefaultExpressionVariable(parse("Parent"));
	static Context context = new TrueContext(new CommonTheory());
	
	/**
	 * Simple model, only one childNode and one parentNode, taking as input the expression determining the conditional probability for the childNode
	 * 
	 * @param expressionForChildNode
	 * @return the created model
	 */
	private static ExpressionBayesianModel generateChildParentModel(Expression expressionForChildNode) {
		Expression param1 = parse("Param1");
		Expression param2 = parse("Param2");
		Expression param3 = parse("Param3");
		Expression param4 = parse("Param4");
		
		// Only one child and one parent
		context = context.extendWithSymbolsAndTypes("Child", "1..5", "Parent", "1..5", "Param1", "Real", "Param2", "Real", "Param3", "Real", "Param4", "Real");
		
		ExpressionBayesianNode parentNode = new ExpressionBayesianNode(param4, context, parentVariable, list(), Util.set(param4)); // for the parent it is a uniform distribution, only one parameter (param4)
		ExpressionBayesianNode childNode = new ExpressionBayesianNode(expressionForChildNode, context, childVariable, list(parentNode), Util.set(param1, param2, param3));
		
		ExpressionBayesianModel model = new ExpressionBayesianModel(list(childNode, parentNode), context);
		
		return model;
	}
	
	@Test
	public void testChildParentModel1() {
		
		ExpressionBayesianModel model = generateChildParentModel(parse("if Child < 5 then Param1 else Param2"));
		
		int numberOfDatapoints1 = 1;
		int numberOfDatapoints2 = 0;
		DefaultDataset dataset = generateDatasetForChildParentModel(numberOfDatapoints1, numberOfDatapoints2, 0, 0);
		
		model.learnModelParametersFromCompleteData(dataset);
		ExpressionBayesianNode learnedChild = model.getNodes().get(0);
		ExpressionBayesianNode learnedParent = model.getNodes().get(1);
		
		Expression expectedParam1 = parse("( (4 + " + numberOfDatapoints1 + ")/(5 + " + (numberOfDatapoints1 + numberOfDatapoints2) + ") ) / 4");
		Expression expectedParam2 = parse("(1 + " + numberOfDatapoints2 + ")/(5 + " + (numberOfDatapoints1 + numberOfDatapoints2) + ")");
		
		Expression expectedChildExpression = parse("if Child < 5 then " + expectedParam1 + " else " + expectedParam2);
		Expression expectedParentExpression = parse("0.2");
		
		Expression childVerification = Equality.make(expectedChildExpression, learnedChild);
		Expression parentVerification = Equality.make(expectedParentExpression, learnedParent);
		
		// println(childVerification); // uncomment this line if you want to see the main equality that is being tested
		
		childVerification = context.evaluate(childVerification);
		parentVerification = context.evaluate(parentVerification);
		
		assertEquals(childVerification, Expressions.TRUE);
		assertEquals(parentVerification, Expressions.TRUE);
	}
	

	public static void printChildParentModelTest() {
		
		// Model
		Expression expressionForChildNode = parse("if Child < 5 then Param1 else Param2");
		// Expression expressionForChildNode = parse("if Parent != 5 then Param1 else Param2");
		// Expression expressionForChildNode = parse("if Parent != 5 then if Child < 5 then Param1 else Param2 else Param3");
		// Expression expressionForChildNode = parse("if Parent != 5 then if Child < Parent then Param1 else Param2 else Param3"); // partial intersection
		
		ExpressionBayesianModel model = generateChildParentModel(expressionForChildNode);
		
		// Dataset
		int numberOfDatapoints1 = 1;
		int numberOfDatapoints2 = 0;
		int numberOfDatapoints3 = 0;
		int numberOfDatapoints4 = 0;
		int numberOfDatapoints = numberOfDatapoints1 + numberOfDatapoints2 + numberOfDatapoints3 + numberOfDatapoints4;
		DefaultDataset dataset = generateDatasetForChildParentModel(numberOfDatapoints1, numberOfDatapoints2, numberOfDatapoints3, numberOfDatapoints4);
	
		println("Initial Expression for childNode = " + expressionForChildNode);
	    println("Initial Expression for parentNode = " + model.getNodes().get(1) + "\n");
		
		// Learning
		long startTime = System.currentTimeMillis();
		model.learnModelParametersFromCompleteData(dataset);
		long stopTime = System.currentTimeMillis();
	    long elapsedTime = stopTime - startTime;
	    System.out.println("Elapsed time for learning with " + numberOfDatapoints + " datapoint(s): " + elapsedTime + " miliseconds \n");
	    
	    // Printing the learned nodes
		List<ExpressionBayesianNode> learnedNodes = model.getNodes();
		System.out.println("- Learned nodes:\n");
		for(ExpressionBayesianNode node : learnedNodes) {
			System.out.println("- " + node.getChildVariable() + "\n" + node + "\n");
		}
		
	}
	
	/**
	 * Auxiliar function to generate a dataset for the childParent model based on standard datapoints (designed to test different cases of the model)
	 * 
	 * @param numberOfDatapoints1 - (1, 1)
	 * @param numberOfDatapoints2 - (5, 1)
	 * @param numberOfDatapoints3 - (1, 3)
	 * @param numberOfDatapoints4 - (1, 5)
	 * @return the dataset with the specified number of datapoints
	 */
	private static DefaultDataset generateDatasetForChildParentModel(int numberOfDatapoints1, int numberOfDatapoints2, int numberOfDatapoints3, int numberOfDatapoints4) {
		List<ExpressionVariable> variables = list(childVariable, parentVariable);
		
		DefaultDatapoint datapoint1 = new DefaultDatapoint(variables, list(parse("1"), parse("1")));
		DefaultDatapoint datapoint2 = new DefaultDatapoint(variables, list(parse("5"), parse("1")));
		DefaultDatapoint datapoint3 = new DefaultDatapoint(variables, list(parse("1"), parse("3")));
		DefaultDatapoint datapoint4 = new DefaultDatapoint(variables, list(parse("1"), parse("5")));
		
		List<DefaultDatapoint> datapoints = list();
		for(int i = 1; i <= numberOfDatapoints1; i++) datapoints.add(datapoint1);
		for(int i = 1; i <= numberOfDatapoints2; i++) datapoints.add(datapoint2);
		for(int i = 1; i <= numberOfDatapoints3; i++) datapoints.add(datapoint3);
		for(int i = 1; i <= numberOfDatapoints4; i++) datapoints.add(datapoint4);

		DefaultDataset dataset = new DefaultDataset(datapoints);
		
		return dataset;
	}
	
	public static void main(String[] args) {
		// printChildParentModelTest();
	}

}
