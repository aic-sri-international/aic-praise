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
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;
import com.sri.ai.praise.learning.parameterlearning.representation.dataset.DefaultDatapoint;
import com.sri.ai.praise.learning.parameterlearning.representation.dataset.DefaultDataset;
import com.sri.ai.praise.learning.parameterlearning.representation.expression.ExpressionBayesianModel;
import com.sri.ai.praise.learning.parameterlearning.representation.expression.ExpressionBayesianNode;
import com.sri.ai.util.Util;

/**
 * CLass to test the implementation of parameter learning for Bayesian nodes (using Expressions)
 * (if you want to see some printed tests, run this class as a Java application instead of a JUnit Test and take a look at the main method here)
 * 
 * @author Roger Leite Lucena
 *
 */

public class ExpressionBayesianModelTest {
	
	static ExpressionVariable childVariable = new DefaultExpressionVariable(parse("Child"));
	static ExpressionVariable parentVariable = new DefaultExpressionVariable(parse("Parent"));
	static Context contextForChildParentModel = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes("Child", "1..5", "Parent", "1..5", "Param1", "Real", "Param2", "Real", "Param3", "Real", "Param4", "Real");
	
	static ExpressionVariable earthquake = new DefaultExpressionVariable(parse("Earthquake"));
	static ExpressionVariable burglary = new DefaultExpressionVariable(parse("Burglary"));
	static ExpressionVariable alarm = new DefaultExpressionVariable(parse("Alarm"));
	static Context contextForEarthquakeBurglaryAlarmModel = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes("Earthquake", "0..1", "Burglary", "0..1", "Alarm", "0..1", "Param1", "Real", "Param2", "Real", "Param3", "Real", "Param4", "Real", "Param5", "Real", "OneMinusParam1", "Real", "OneMinusParam2", "Real", "OneMinusParam3", "Real", "OneMinusParam4", "Real", "OneMinusParam5", "Real");
	
	/**
	 * Simple model, only one childNode and one parentNode, taking as input the expression determining the conditional probability for the childNode, for the parentNode the expression is a fixed constant (Param4)
	 * 
	 * @param expressionForChildNode
	 * @return the created model
	 */
	private static ExpressionBayesianModel generateChildParentModel(Expression expressionForChildNode) {
		Expression param1 = parse("Param1");
		Expression param2 = parse("Param2");
		Expression param3 = parse("Param3");
		Expression param4 = parse("Param4");
		
		ExpressionBayesianNode parentNode = new ExpressionBayesianNode(param4, contextForChildParentModel, parentVariable, list(), Util.set(param4)); // for the parent it is a uniform distribution, only one parameter (param4)
		ExpressionBayesianNode childNode = new ExpressionBayesianNode(expressionForChildNode, contextForChildParentModel, childVariable, list(parentNode), Util.set(param1, param2, param3));
		
		ExpressionBayesianModel model = new ExpressionBayesianModel(list(childNode, parentNode));
		
		return model;
	}
	
	/**
	 * Simple model with three nodes, Alarm as child and [Earthquake, Burglary] as parents
	 * 
	 * @return the Earthquake/Burglary/Alarm model
	 */
	private static ExpressionBayesianModel generateEarthquakeBurglaryAlarmModel() {
		Expression expressionForEarthquakeNode = parse("if Earthquake = 1 then 0.01 else 0.99"); // prior probability
		Expression expressionForBurglaryNode = parse("if Burglary = 1 then Param5 else OneMinusParam5");
		
		Expression expressionForAlarmNode = parse("if Earthquake = 1 " + 
														"then if Burglary = 1 " + 
															"then if Alarm = 1 then Param1 else OneMinusParam1 " +
															"else if Alarm = 1 then Param2 else OneMinusParam2 " + 
														"else if Burglary = 1 " + 
															"then if Alarm = 1 then Param3 else OneMinusParam3 " + 
															"else if Alarm = 1 then Param4 else OneMinusParam4");
		
		ExpressionBayesianNode earthquakeNode = new ExpressionBayesianNode(expressionForEarthquakeNode, contextForEarthquakeBurglaryAlarmModel, earthquake, list(), Util.set()); 
		ExpressionBayesianNode burglaryNode = new ExpressionBayesianNode(expressionForBurglaryNode, contextForEarthquakeBurglaryAlarmModel, burglary, list(), Util.set(parse("Param5"), parse("OneMinusParam5"))); 
		ExpressionBayesianNode alarmNode = new ExpressionBayesianNode(expressionForAlarmNode, contextForEarthquakeBurglaryAlarmModel, alarm, list(earthquakeNode, burglaryNode), Util.set(parse("Param1"), parse("Param2"), parse("Param3"), parse("Param4"), parse("OneMinusParam1"), parse("OneMinusParam2"), parse("OneMinusParam3"), parse("OneMinusParam4"))); 
		
		ExpressionBayesianModel model = new ExpressionBayesianModel(list(alarmNode, earthquakeNode, burglaryNode));
		
		return model;
	}
	
	/**
	 * Only one family having the two parameters
	 * expressionForChildNode: if Child < 5 then Param1 else Param2
	 * 
	 * Final families: 
	 * F1: [Condition = true, Parameters: [Param1, Param2]]
	 */
	@Test
	public void testChildParentModel1() {
		ExpressionBayesianModel model = generateChildParentModel(parse("if Child < 5 then Param1 else Param2"));
		
		int numberOfDatapoints1 = 1; // (1, 2)
		int numberOfDatapoints2 = 2; // (5, 1)
		DefaultDataset dataset = generateDatasetForChildParentModel(numberOfDatapoints1, numberOfDatapoints2, 0, 0);
		
		model.learnModelParametersFromCompleteData(dataset);
		ExpressionBayesianNode learnedChild = model.getNodes().get(0);
		ExpressionBayesianNode learnedParent = model.getNodes().get(1);
		
		Expression expectedParam1inF1 = parse("( (4 + " + numberOfDatapoints1 + ")/(5 + " + (numberOfDatapoints1 + numberOfDatapoints2) + ") ) / 4");
		Expression expectedParam2inF1 = parse("(1 + " + numberOfDatapoints2 + ")/(5 + " + (numberOfDatapoints1 + numberOfDatapoints2) + ")");
		
		Expression expectedChildExpression = parse("if Child < 5 then " + expectedParam1inF1 + " else " + expectedParam2inF1);
		Expression expectedParentExpression = parse("0.2");
		
		Expression childVerification = Equality.make(expectedChildExpression, learnedChild);
		Expression parentVerification = Equality.make(expectedParentExpression, learnedParent);
		
		// println(childVerification); // uncomment this line if you want to see the main equality that is being tested
		
		childVerification = contextForChildParentModel.evaluate(childVerification);
		parentVerification = contextForChildParentModel.evaluate(parentVerification);
		
		assertEquals(Expressions.TRUE, childVerification);
		assertEquals(Expressions.TRUE, parentVerification);
	}
	
	/**
	 * Two families, one for each parameter 
	 * expressionForChildNode: if Parent != 5 then Param1 else Param2
	 * 
	 * Final families:
	 * F1: [Condition: Parent in {1, 2, 3, 4}, Parameters: [Param1]]
	 * F2: [Condition: Parent in {5}, Parameters: [Param2]]
	 */
	@Test
	public void testChildParentModel2() {
		ExpressionBayesianModel model = generateChildParentModel(parse("if Parent != 5 then Param1 else Param2"));
		
		int numberOfDatapoints1 = 1; // (1, 2)
		int numberOfDatapoints4 = 2; // (1, 5)
		DefaultDataset dataset = generateDatasetForChildParentModel(numberOfDatapoints1, 0, 0, numberOfDatapoints4);
		
		model.learnModelParametersFromCompleteData(dataset);
		ExpressionBayesianNode learnedChild = model.getNodes().get(0);
		ExpressionBayesianNode learnedParent = model.getNodes().get(1);
		
		Expression expectedParam1inF1 = parse("0.2");
		Expression expectedParam2inF2 = parse("0.2");
		
		Expression expectedChildExpression = parse("if Parent != 5 then " + expectedParam1inF1 + " else " + expectedParam2inF2);
		Expression expectedParentExpression = parse("0.2");
		
		Expression childVerification = Equality.make(expectedChildExpression, learnedChild);
		Expression parentVerification = Equality.make(expectedParentExpression, learnedParent);
		
		// println(childVerification); // uncomment this line if you want to see the main equality that is being tested
		
		childVerification = contextForChildParentModel.evaluate(childVerification);
		parentVerification = contextForChildParentModel.evaluate(parentVerification);
		
		assertEquals(Expressions.TRUE, childVerification);
		assertEquals(Expressions.TRUE, parentVerification);
	}
	
	/**
	 * Two families, one with two of the parameters and the other with the third one
	 * expressionForChildNode: if Parent != 5 then if Child < 5 then Param1 else Param2 else Param3
	 * 
	 * Final families:
	 * F1: [Condition: Parent in {1, 2, 3, 4}, Parameters: [Param1, Param2]]
	 * F2: [Condition: Parent in {5}, Parameters: [Param3]]
	 */
	@Test
	public void testChildParentModel3() {
		ExpressionBayesianModel model = generateChildParentModel(parse("if Parent != 5 then if Child < 5 then Param1 " + 
																									    "else Param2 " +
																					  "else Param3"));
		
		int numberOfDatapoints1 = 1; // (1, 2)
		int numberOfDatapoints2 = 2; // (5, 1)
		int numberOfDatapoints4 = 3; // (1, 5)
		DefaultDataset dataset = generateDatasetForChildParentModel(numberOfDatapoints1, numberOfDatapoints2, 0, numberOfDatapoints4);
		
		model.learnModelParametersFromCompleteData(dataset);
		ExpressionBayesianNode learnedChild = model.getNodes().get(0);
		ExpressionBayesianNode learnedParent = model.getNodes().get(1);
		
		Expression expectedParam1inF1 = parse("( (4 + " + numberOfDatapoints1 + ")/(5 + " + (numberOfDatapoints1 + numberOfDatapoints2) + ") ) / 4");
		Expression expectedParam2inF1 = parse("(1 + " + numberOfDatapoints2 + ")/(5 + " + (numberOfDatapoints1 + numberOfDatapoints2) + ")");
		Expression expectedParam3inF2 = parse("0.2"); 
	
		Expression expectedChildExpression = parse("if Parent != 5 then if Child < 5 then " + expectedParam1inF1 + 
																				   " else " + expectedParam2inF1 + 
																 " else " + expectedParam3inF2);
		
		Expression expectedParentExpression = parse("0.2");
		
		Expression childVerification = Equality.make(expectedChildExpression, learnedChild);
		Expression parentVerification = Equality.make(expectedParentExpression, learnedParent);
		
		// println(childVerification); // uncomment this line if you want to see the main equality that is being tested
		
		childVerification = contextForChildParentModel.evaluate(childVerification);
		parentVerification = contextForChildParentModel.evaluate(parentVerification);
		
		assertEquals(Expressions.TRUE, childVerification);
		assertEquals(Expressions.TRUE, parentVerification);
	}
	
	/**
	 * Two families, case with partial intersection at the beginning and manipulation to generate new completely disjoint families (in terms of their conditions over the Parent)
	 * expressionForChildNode: if Child > Parent then Param1 else Param2
	 * 
	 * Final families:
	 * F1: [Condition: Parent in {5}, Parameters: [Param2]] 
	 * F2: [Condition: Parent in {1, 2, 3, 4}, Parameters: [Param1, Param2]] 
	 */
	@Test
	public void testChildParentModel4() {
		ExpressionBayesianModel model = generateChildParentModel(parse("if Child > Parent then Param1 " +
																						" else Param2 "));
		
		int numberOfDatapoints1 = 1; // (1, 2)
		int numberOfDatapoints2 = 2; // (5, 1)
		int numberOfDatapoints4 = 3; // (1, 5)
		DefaultDataset dataset = generateDatasetForChildParentModel(numberOfDatapoints1, numberOfDatapoints2, 0, numberOfDatapoints4);
		
		model.learnModelParametersFromCompleteData(dataset);
		ExpressionBayesianNode learnedChild = model.getNodes().get(0);
		ExpressionBayesianNode learnedParent = model.getNodes().get(1);
		
		Expression expectedParam2inF1 = parse("0.2");
		Expression expectedParam1inF2 = parse("( ((5 - Parent) + " + numberOfDatapoints2 + ")/(5 + " + (numberOfDatapoints1 + numberOfDatapoints2) + ") ) / (5 - Parent)");
		Expression expectedParam2inF2 = parse("( (Parent + " + numberOfDatapoints1 + ")/(5 + " + (numberOfDatapoints1 + numberOfDatapoints2) + ") ) / Parent");
	
		Expression expectedChildExpression = parse("if Parent = 5 then " + expectedParam2inF1 + 
																" else if Child > Parent then " + expectedParam1inF2 + 
																				   " else " + expectedParam2inF2);
		
		
		Expression expectedParentExpression = parse("0.2");
		
		
		// Generating the childVerification Expression after iteration through all the possible values of Parent
		Expression childVerification;
		Expression andExpression = Expressions.TRUE;
		for(int i = 1; i <= 5; i++) {
			andExpression = And.make(andExpression, Equality.make(expectedChildExpression, learnedChild).replaceAllOccurrences(parentVariable, parse("" + i), contextForChildParentModel));
		}
		childVerification = andExpression;
		
		
		// Expression childVerification = Equality.make(expectedChildExpression, learnedChild); // right way to generate the childVerification Expression, but giving errors (related to PRAiSE evaluations for the Parent variable I believe, out of the scope of parameter learning)
		Expression parentVerification = Equality.make(expectedParentExpression, learnedParent);
		
		// println(childVerification); // uncomment this line if you want to see the main equality that is being tested
		
		childVerification = contextForChildParentModel.evaluate(childVerification);
		parentVerification = contextForChildParentModel.evaluate(parentVerification);
		
		assertEquals(Expressions.TRUE, childVerification);
		assertEquals(Expressions.TRUE, parentVerification);
	}
	
	/**
	 * Three families, case with partial intersection at the beginning and manipulation to generate new completely disjoint families (in terms of their conditions over the Parent)
	 * expressionForChildNode: if Parent != 5 then if Child < Parent then Param1 else Param2 else Param3
	 * 
	 * Final families:
	 * F1: [Condition: Parent in {1}, Parameters: [Param2]] 
	 * F2: [Condition: Parent in {5}, Parameters: [Param3]] 
	 * F3: [Condition: Parent in {2, 3, 4}, Parameters: [Param1, Param2]]
	 */
	@Test
	public void testChildParentModel5() {
		ExpressionBayesianModel model = generateChildParentModel(parse("if Parent != 5 then if Child < Parent then Param1 " +
																						 				    " else Param2 " + 
																	                 " else Param3"));
		
		int numberOfDatapoints1 = 1; // (1, 2)
		int numberOfDatapoints2 = 2; // (5, 1)
		int numberOfDatapoints3 = 3; // (4, 3)
		int numberOfDatapoints4 = 4; // (1, 5)
		DefaultDataset dataset = generateDatasetForChildParentModel(numberOfDatapoints1, numberOfDatapoints2, numberOfDatapoints3, numberOfDatapoints4);
		
		model.learnModelParametersFromCompleteData(dataset);
		ExpressionBayesianNode learnedChild = model.getNodes().get(0);
		ExpressionBayesianNode learnedParent = model.getNodes().get(1);
		
		Expression expectedParam2inF1 = parse("0.2");
		Expression expectedParam3inF2 = parse("0.2");
		Expression expectedParam1inF3 = parse("( ((Parent - 1) + " + numberOfDatapoints1 + " ) / ( 5 + " + (numberOfDatapoints1 + numberOfDatapoints3) + ") ) / (Parent - 1)");
		Expression expectedParam2inF3 = parse("( ((6 - Parent) + " + numberOfDatapoints3 + ")/(5 + " + (numberOfDatapoints1 + numberOfDatapoints3) + ") ) / (6 - Parent)"); 
	
		Expression expectedChildExpression = parse("if Parent = 1 then " + expectedParam2inF1 + 
																" else if Parent = 5 then " + expectedParam3inF2 + 
																				   " else if Child < Parent then " + expectedParam1inF3 + 
																										  " else " + expectedParam2inF3);
		
		Expression expectedParentExpression = parse("0.2");
		
		
		// Generating the childVerification Expression after iteration through all the possible values of Parent
		Expression childVerification;
		Expression andExpression = Expressions.TRUE;
		for(int i = 1; i <= 5; i++) {
			andExpression = And.make(andExpression, Equality.make(expectedChildExpression, learnedChild).replaceAllOccurrences(parentVariable, parse("" + i), contextForChildParentModel));
		}
		childVerification = andExpression;
		
		
		// Expression childVerification = Equality.make(expectedChildExpression, learnedChild); // right way to generate the childVerification Expression, but giving errors (related to PRAiSE evaluations for the Parent variable I believe, out of the scope of parameter learning)
		Expression parentVerification = Equality.make(expectedParentExpression, learnedParent);
		
		// println(childVerification); // uncomment this line if you want to see the main equality that is being tested
		
		childVerification = contextForChildParentModel.evaluate(childVerification);
		parentVerification = contextForChildParentModel.evaluate(parentVerification);
		
		assertEquals(Expressions.TRUE, childVerification);
		assertEquals(Expressions.TRUE, parentVerification);
	}
	
	/**
	 * For the alarmNode:
	 * We have four families (one for every (Earthquake, Burglary) pair value), and naturally each one with two parameters (one for Alarm = 1 and the other for Alarm = 0)
	 * 
	 * Final families:
	 * F1: [Condition: Earthquake = 1 and Burglary = 1, Parameters: [Param1, OneMinusParam1]]
	 * F2: [Condition: Earthquake = 1 and Burglary = 0, Parameters: [Param2, OneMinusParam2]]
	 * F3: [Condition: Earthquake = 0 and Burglary = 1, Parameters: [Param3, OneMinusParam3]]
	 * F4: [Condition: Earthquake = 0 and Burglary = 0, Parameters: [Param4, OneMinusParam4]]
	 * 
	 * For the burglaryNode:
	 * Similar and much simpler, we have only one family here, with two parameters (one for Burglary = 1 and the other for Burglary = 0)
	 * F1burglary = [Condition: true, Parameters: [Param5, OneMinusParam5]]
	 * 
	 * For the earthquakeNode:
	 * Since we have no parameters to learn we have no families here, this expressionFactor is treated as a constant prior probability defined by the user (here, P(Earthquake) is set to 1% in the generateEarthquakeBurglaryAlarmModel method)
	 */
	@Test
	public void testEarthquakeBurglaryAlarmModel() {
		ExpressionBayesianModel model = generateEarthquakeBurglaryAlarmModel();
		
		// Dataset - Order of variables for the datapoints: (Alarm, Earthquake, Burglary)
		int numberOfDatapoints1 = 1; // (1, 0, 1)
		int numberOfDatapoints2 = 2; // (1, 1, 1)
		DefaultDataset dataset = generateDatasetForEarthquakeBurglaryAlarmModel(numberOfDatapoints1, numberOfDatapoints2);
		
		// Expected values for alarmNode
		Expression expectedParam3inF3 = parse("(1 + " + numberOfDatapoints1 + ") / (2 + " + numberOfDatapoints1 + ")");
		Expression expectedOneMinusParam3inF3 = parse("1 / (2 + " + numberOfDatapoints1 + ")");
		
		Expression expectedParam1inF1 = parse("(1 + " + numberOfDatapoints2 + ") / (2 + " + numberOfDatapoints2 + ")");
		Expression expectedOneMinusParam1inF1 = parse("1 / (2 + " + numberOfDatapoints2 + ")");
		
		Expression expectedAlarmNode = parse("if Earthquake = 0 and Burglary = 1 then if Alarm = 1 then " + expectedParam3inF3 + " else " + expectedOneMinusParam3inF3 + " else if Earthquake = 1 and Burglary = 1 then if Alarm = 1 then " + expectedParam1inF1 + " else " + expectedOneMinusParam1inF1 + " else 0.5");
		
		// Expected values for burglaryNode
		Expression expectedParam5inF1burglary = parse("(1 + " + (numberOfDatapoints1 + numberOfDatapoints2) + ") / (2 + " + (numberOfDatapoints1 + numberOfDatapoints2) + ")");
		Expression expectedOneMinusParam5inF1burglary = parse("1 / (2 + " + (numberOfDatapoints1 + numberOfDatapoints2) + ")");
		
		Expression expectedBurglaryNode = parse("if Burglary = 1 then " + expectedParam5inF1burglary + " else " + expectedOneMinusParam5inF1burglary);
		
		// Expected value for earthquakeNode (fixed prior probability)
		Expression expectedEarthquakeNode = parse("if Earthquake = 1 then 0.01 else 0.99");
		
		// Learning
		model.learnModelParametersFromCompleteData(dataset);
		ExpressionBayesianNode learnedAlarmNode = model.getNodes().get(0);
		ExpressionBayesianNode learnedEarthquakeNode = model.getNodes().get(1);
		ExpressionBayesianNode learnedBurglaryNode = model.getNodes().get(2);
		
		// Verifications
		Expression alarmNodeVerification = Equality.make(expectedAlarmNode, learnedAlarmNode); 
		Expression earthquakeNodeVerification = Equality.make(expectedEarthquakeNode, learnedEarthquakeNode);
		Expression burglaryNodeVerification = Equality.make(expectedBurglaryNode, learnedBurglaryNode);
		
		alarmNodeVerification = contextForEarthquakeBurglaryAlarmModel.evaluate(alarmNodeVerification);
		earthquakeNodeVerification = contextForEarthquakeBurglaryAlarmModel.evaluate(earthquakeNodeVerification);
		burglaryNodeVerification = contextForEarthquakeBurglaryAlarmModel.evaluate(burglaryNodeVerification);
		
		// Assertions
		assertEquals(Expressions.TRUE, alarmNodeVerification);
		assertEquals(Expressions.TRUE, earthquakeNodeVerification);
		assertEquals(Expressions.TRUE, burglaryNodeVerification);
	}
	
	/**
	 * Printing the test for a simple Child/Parent Bayesian model, choose one of the five expressionForChildNode below to be tested. The distribution (expression) for the parentNode is fixed to a uniform with only one parameter (as set in generateChildParentModel)
	 */
	public static void printChildParentModelTest() {
		// Model
		// Expression expressionForChildNode = parse("if Child < 5 then Param1 else Param2");
		// Expression expressionForChildNode = parse("if Parent != 5 then Param1 else Param2");
		// Expression expressionForChildNode = parse("if Parent != 5 then if Child < 5 then Param1 else Param2 else Param3");
		Expression expressionForChildNode = parse("if Child > Parent then Param1 else Param2"); // partial intersection
		// Expression expressionForChildNode = parse("if Parent != 5 then if Child < Parent then Param1 else Param2 else Param3"); // partial intersection
		
		ExpressionBayesianModel model = generateChildParentModel(expressionForChildNode);
		
		// Dataset - Order of variables for the datapoints: (Child, Parent)
		int numberOfDatapoints1 = 0; // (1, 2)
		int numberOfDatapoints2 = 0; // (5, 1)
		int numberOfDatapoints3 = 0; // (4, 3)
		int numberOfDatapoints4 = 0; // (1, 5)
		DefaultDataset dataset = generateDatasetForChildParentModel(numberOfDatapoints1, numberOfDatapoints2, numberOfDatapoints3, numberOfDatapoints4);
	
		println("Initial Expression for childNode = " + expressionForChildNode);
	    println("Initial Expression for parentNode = " + model.getNodes().get(1) + "\n");
		
		// Learning
	    println("(Learning ...)");
		long startTime = System.currentTimeMillis();
		model.learnModelParametersFromCompleteData(dataset);
		long stopTime = System.currentTimeMillis();
	    long elapsedTime = stopTime - startTime;
	    System.out.println("Elapsed time for learning with " + dataset.getDatapoints().size() + " datapoint(s): " + elapsedTime + " miliseconds \n");
	    
	    // Printing the learned nodes
		List<ExpressionBayesianNode> learnedNodes = model.getNodes();
		System.out.println("- Learned nodes:\n");
		for(ExpressionBayesianNode node : learnedNodes) {
			println("- " + node.getChildVariable());
			println("Learned value: " + node);
			println("Families: " + node.getFamilies() + "\n");
		}
		
	}
	
	/**
	 * Printing the test for the Earthquake/Burglary/Alarm model
	 * 
	 * For the alarmNode:
	 * We have four families (one for every (Earthquake, Burglary) pair value), and naturally each one with two parameters (one for Alarm = 1 and the other for Alarm = 0)
	 * 
	 * Final families:
	 * F1: [Condition: Earthquake = 1 and Burglary = 1, Parameters: [Param1, OneMinusParam1]]
	 * F2: [Condition: Earthquake = 1 and Burglary = 0, Parameters: [Param2, OneMinusParam2]]
	 * F3: [Condition: Earthquake = 0 and Burglary = 1, Parameters: [Param3, OneMinusParam3]]
	 * F4: [Condition: Earthquake = 0 and Burglary = 0, Parameters: [Param4, OneMinusParam4]]
	 * 
	 * For the burglaryNode:
	 * Similar and much simpler, we have only one family here, with two parameters (one for Burglary = 1 and the other for Burglary = 0)
	 * F1burglary = [Condition: true, Parameters: [Param5, OneMinusParam5]]
	 * 
	 * For the earthquakeNode:
	 * Since we have no parameters to learn we have no families here, this expressionFactor is treated as a constant prior probability defined by the user (here, P(Earthquake) is set to 1% in the generateEarthquakeBurglaryAlarmModel method)
	 */
	public static void printEarthquakeBurglaryAlarmModelTest() {
		ExpressionBayesianModel model = generateEarthquakeBurglaryAlarmModel();
		
		println("-- Initial Expressions for each node:");
		
		for(ExpressionBayesianNode node : model.getNodes()) {
			println("\n- " + node.getChildVariable() + " (initial expression)\n" + node);
		}
		
		// Dataset - Order of variables for the datapoints: (Alarm, Earthquake, Burglary)
		int numberOfDatapoints1 = 2; // (1, 0, 1)
		int numberOfDatapoints2 = 0; // (1, 1, 1)
		DefaultDataset dataset = generateDatasetForEarthquakeBurglaryAlarmModel(numberOfDatapoints1, numberOfDatapoints2);
		
		// Learning
		println("\n(Learning ...)");
		long startTime = System.currentTimeMillis();
		model.learnModelParametersFromCompleteData(dataset);
		long stopTime = System.currentTimeMillis();
	    long elapsedTime = stopTime - startTime;
	    System.out.println("Elapsed time for learning with " + dataset.getDatapoints().size() + " datapoint(s): " + elapsedTime + " miliseconds \n");
	    
	    // Printing the learned nodes
		List<ExpressionBayesianNode> learnedNodes = model.getNodes();
		System.out.println("-- Learned nodes:\n");
		for(ExpressionBayesianNode node : learnedNodes) {
			println("- " + node.getChildVariable());
			println("Learned value: " + node);
			println("Families: " + node.getFamilies() + "\n");
		}
		
	}
	
	/**
	 * Auxiliar function to generate a dataset for the childParent model based on standard datapoints (designed to test all the different subcases for the model)
	 * 
	 * Order of variables for the datapoints: (Child, Parent)
	 * @param numberOfDatapoints1 - (1, 2)
	 * @param numberOfDatapoints2 - (5, 1)
	 * @param numberOfDatapoints3 - (4, 3)
	 * @param numberOfDatapoints4 - (1, 5)
	 * @return the dataset with the specified number of datapoints
	 */
	private static DefaultDataset generateDatasetForChildParentModel(int numberOfDatapoints1, int numberOfDatapoints2, int numberOfDatapoints3, int numberOfDatapoints4) {
		List<ExpressionVariable> variables = list(childVariable, parentVariable);
		
		DefaultDatapoint datapoint1 = new DefaultDatapoint(variables, list(parse("1"), parse("2"))); 
		DefaultDatapoint datapoint2 = new DefaultDatapoint(variables, list(parse("5"), parse("1"))); 
		DefaultDatapoint datapoint3 = new DefaultDatapoint(variables, list(parse("4"), parse("3"))); 
		DefaultDatapoint datapoint4 = new DefaultDatapoint(variables, list(parse("1"), parse("5")));
		
		List<DefaultDatapoint> datapoints = list();
		for(int i = 1; i <= numberOfDatapoints1; i++) datapoints.add(datapoint1);
		for(int i = 1; i <= numberOfDatapoints2; i++) datapoints.add(datapoint2);
		for(int i = 1; i <= numberOfDatapoints3; i++) datapoints.add(datapoint3);
		for(int i = 1; i <= numberOfDatapoints4; i++) datapoints.add(datapoint4);

		DefaultDataset dataset = new DefaultDataset(datapoints);
		
		return dataset;
	}
	
	/**
	 * Auxiliar function to generate a dataset for the Earthquake/Burglary/Alarm model based on some standard datapoints
	 * 
	 * Order of variables for the datapoints: (Alarm, Earthquake, Burglary)
	 * @param numberOfDatapoints1 - (1, 0, 1)
	 * @param numberOfDatapoints2 - (1, 1, 1)
	 * @return
	 */
	private static DefaultDataset generateDatasetForEarthquakeBurglaryAlarmModel(int numberOfDatapoints1, int numberOfDatapoints2) {
		List<ExpressionVariable> variables = list(alarm, earthquake, burglary);
		DefaultDatapoint datapoint1 = new DefaultDatapoint(variables, list(parse("1"), parse("0"), parse("1"))); 
		DefaultDatapoint datapoint2 = new DefaultDatapoint(variables, list(parse("1"), parse("1"), parse("1"))); 
		
		List<DefaultDatapoint> datapoints = list();
		
		for(int i = 1; i <= numberOfDatapoints1; i++) datapoints.add(datapoint1);
		for(int i = 1; i <= numberOfDatapoints2; i++) datapoints.add(datapoint2);
		
		DefaultDataset dataset = new DefaultDataset(datapoints);
		return dataset;
	}
	
	public static void main(String[] args) {
		// printChildParentModelTest(); // uncomment this line if you want to see the printed test for the Child/Parent model
		printEarthquakeBurglaryAlarmModelTest(); // uncomment this line if you want to see the printed test for the Earthquake/Burglary/Alarm model
	}

}
