package com.sri.ai.test.praise.learning.parameterlearning;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.praise.inference.generic.representation.table.TableVariable;
import com.sri.ai.praise.learning.parameterlearning.representation.dataset.DefaultDatapoint;
import com.sri.ai.praise.learning.parameterlearning.representation.dataset.DefaultDataset;
import com.sri.ai.praise.learning.parameterlearning.representation.table.TableBayesianModel;
import com.sri.ai.praise.learning.parameterlearning.representation.table.TableBayesianNode;

/**
 * CLass to test the implementation of parameter learning for bayesian nodes (using tables)
 * 
 * @author Roger Leite Lucena
 *
 */

public class TableBayesianModelTest {
	
	static final TableVariable sickVariable = new TableVariable("sick", 2);
	static final TableVariable sunVariable = new TableVariable("sun", 2);
	static final TableVariable coldVariable = new TableVariable("cold", 2);
	
	// Simple model with 3 nodes: Sick as child and [Sun, Cold] as parents 
	static TableBayesianModel sickSunColdModel = generateSickSunColdModel();
	
	static TableBayesianModel generateSickSunColdModel() {
		ArrayList<TableVariable> parentsOfSick = arrayList(sunVariable, coldVariable);

		TableBayesianNode sickNode = new TableBayesianNode(sickVariable, parentsOfSick);
		TableBayesianNode sunNode = new TableBayesianNode(sunVariable, arrayList());
		TableBayesianNode coldNode = new TableBayesianNode(coldVariable, arrayList());
		
		List<TableBayesianNode> nodes = list(sickNode, sunNode, coldNode);

		TableBayesianModel sickSunColdModel = new TableBayesianModel(nodes);
		
		return sickSunColdModel;
	}
	
	@Test
	public void testSickSunColdModel() {
		// Dataset
		List<TableVariable> variables = list(sickVariable, sunVariable, coldVariable);
		List<Integer> variableValues = list(1, 0, 1);
		DefaultDatapoint datapoint = new DefaultDatapoint(variables, variableValues);

		List<DefaultDatapoint> datapoints = list();
		int numberOfDatapoints = 2;
		for(int i = 1; i <= numberOfDatapoints; i++) {
			datapoints.add(datapoint);
		}

		DefaultDataset dataset = new DefaultDataset(datapoints);
		
		// Learning
		sickSunColdModel.learnModelParametersFromCompleteData(dataset);
	    
		List<? extends TableBayesianNode> learnedNodes = sickSunColdModel.getNodes();
		
		// Testing
		
		// For the sickNode first:
		// Expected parameters (2 datapoints): {(0, [0, 0])=0.5, (1, [0, 0])=0.5, (1, [1, 0])=0.5, (1, [1, 1])=0.5, (0, [1, 1])=0.5, (0, [1, 0])=0.5, (0, [0, 1])=0.25, (1, [0, 1])=0.75}
		TableBayesianNode learnedSickNode = learnedNodes.get(0); 
		LinkedHashMap<TableVariable, Integer> variablesAndTheirValues = map();
	    variablesAndTheirValues.put(sickVariable, 0);
	    variablesAndTheirValues.put(sunVariable, 0);
	    variablesAndTheirValues.put(coldVariable, 0);
		
	    // Parameter for (0, [0, 0]):
	    Assert.assertEquals(Double.valueOf(0.5), learnedSickNode.getEntryFor(variablesAndTheirValues));
	    
	    // Parameter for (1, [0, 0]):
	    variablesAndTheirValues.put(sickVariable, 1);
	    Assert.assertEquals(Double.valueOf(0.5), learnedSickNode.getEntryFor(variablesAndTheirValues));
	    
	    // Parameter for (1, [1, 0]):
	    variablesAndTheirValues.put(sunVariable, 1);
	    Assert.assertEquals(Double.valueOf(0.5), learnedSickNode.getEntryFor(variablesAndTheirValues));
	    
	    // Parameter for (1, [1, 1]):
	    variablesAndTheirValues.put(coldVariable, 1);
	    Assert.assertEquals(Double.valueOf(0.5), learnedSickNode.getEntryFor(variablesAndTheirValues));
	    
	    // Parameter for (0, [1, 1]):
	    variablesAndTheirValues.put(sickVariable, 0);
	    Assert.assertEquals(Double.valueOf(0.5), learnedSickNode.getEntryFor(variablesAndTheirValues));
	    
	    // Parameter for (0, [1, 0]):
	    variablesAndTheirValues.put(coldVariable, 0);
	    Assert.assertEquals(Double.valueOf(0.5), learnedSickNode.getEntryFor(variablesAndTheirValues));
	    
	    // Parameter for (0, [0, 1]):
	    variablesAndTheirValues.put(sunVariable, 0);
	    variablesAndTheirValues.put(coldVariable, 1);
	    Assert.assertEquals(Double.valueOf(1.0 / (2 + numberOfDatapoints)), learnedSickNode.getEntryFor(variablesAndTheirValues));
	    
	    // Parameter for (1, [0, 1]):
	    variablesAndTheirValues.put(sickVariable, 1);
	    Assert.assertEquals(Double.valueOf((1.0 + numberOfDatapoints) / (2 + numberOfDatapoints)), learnedSickNode.getEntryFor(variablesAndTheirValues));
	    
	    // For the sunNode:
	 	// Expected parameters (2 datapoints): {(0, [])=0.75, (1, [])=0.25}
	    TableBayesianNode learnedSunNode = learnedNodes.get(1); 
		variablesAndTheirValues = map();
	    variablesAndTheirValues.put(sunVariable, 0);
	    
	    // Parameter for (0, []):
	    Assert.assertEquals(Double.valueOf((1.0 + numberOfDatapoints) / (2 + numberOfDatapoints)), learnedSunNode.getEntryFor(variablesAndTheirValues));
	    
	    // Parameter for (1, []):
	    variablesAndTheirValues.put(sunVariable, 1);
	    Assert.assertEquals(Double.valueOf(1.0 / (2 + numberOfDatapoints)), learnedSunNode.getEntryFor(variablesAndTheirValues));
	    
	    // For the coldNode:
 	 	// Expected parameters (2 datapoints): {(0, [])=0.25, (1, [])=0.75}
 	    TableBayesianNode learnedColdNode = learnedNodes.get(2); 
 		variablesAndTheirValues = map();
 	    variablesAndTheirValues.put(coldVariable, 0);
 	    
 	    // Parameter for (0, []):
 	    Assert.assertEquals(Double.valueOf(1.0 / (2 + numberOfDatapoints)), learnedColdNode.getEntryFor(variablesAndTheirValues));
 	    
 	    // Parameter for (1, []):
 	    variablesAndTheirValues.put(coldVariable, 1);
 	    Assert.assertEquals(Double.valueOf((1.0 + numberOfDatapoints) / (2 + numberOfDatapoints)), learnedColdNode.getEntryFor(variablesAndTheirValues));
 	    
	}
	
	@Test
	public void testSickSunColdModelWithDifferentDatapoints() {
		// Dataset
		List<TableVariable> variables = list(sickVariable, sunVariable, coldVariable);
		List<Integer> variableValues1 = list(1, 0, 1);
		DefaultDatapoint datapoint1 = new DefaultDatapoint(variables, variableValues1);

		List<DefaultDatapoint> datapoints = list();
		int numberOfDatapoints1 = 4;
		for(int i = 1; i <= numberOfDatapoints1; i++) {
			datapoints.add(datapoint1);
		}
		
		List<Integer> variableValues2 = list(0, 0, 0);
		DefaultDatapoint datapoint2 = new DefaultDatapoint(variables, variableValues2);
		int numberOfDatapoints2 = 2;
		for(int i = 1; i <= numberOfDatapoints2; i++) {
			datapoints.add(datapoint2);
		}
		
		List<Integer> variableValues3 = list(0, 0, 1);
		DefaultDatapoint datapoint3 = new DefaultDatapoint(variables, variableValues3);
		int numberOfDatapoints3 = 1;
		for(int i = 1; i <= numberOfDatapoints3; i++) {
			datapoints.add(datapoint3);
		}

		DefaultDataset dataset = new DefaultDataset(datapoints);
		
		// Learning
		sickSunColdModel.learnModelParametersFromCompleteData(dataset);
	    
		List<? extends TableBayesianNode> learnedNodes = sickSunColdModel.getNodes();
		
		// Testing
		
		// For the sickNode first:
		// Expected parameters (2 datapoints1): {(0, [0, 0])=0.5, (1, [0, 0])=0.5, (1, [1, 0])=0.5, (1, [1, 1])=0.5, (0, [1, 1])=0.5, (0, [1, 0])=0.5, (0, [0, 1])=0.25, (1, [0, 1])=0.75}
		TableBayesianNode learnedSickNode = learnedNodes.get(0); 
		LinkedHashMap<TableVariable, Integer> variablesAndTheirValues = map();
	    variablesAndTheirValues.put(sickVariable, 0);
	    variablesAndTheirValues.put(sunVariable, 0);
	    variablesAndTheirValues.put(coldVariable, 0);
		
	    // Parameter for (0, [0, 0]):
	    Assert.assertEquals(Double.valueOf((1.0 + numberOfDatapoints2) / (2 + numberOfDatapoints2)), learnedSickNode.getEntryFor(variablesAndTheirValues));
	    
	    // Parameter for (1, [0, 0]):
	    variablesAndTheirValues.put(sickVariable, 1);
	    Assert.assertEquals(Double.valueOf(1.0 / (2 + numberOfDatapoints2)), learnedSickNode.getEntryFor(variablesAndTheirValues));
	    
	    // Parameter for (1, [1, 0]):
	    variablesAndTheirValues.put(sunVariable, 1);
	    Assert.assertEquals(Double.valueOf(0.5), learnedSickNode.getEntryFor(variablesAndTheirValues));
	    
	    // Parameter for (1, [1, 1]):
	    variablesAndTheirValues.put(coldVariable, 1);
	    Assert.assertEquals(Double.valueOf(0.5), learnedSickNode.getEntryFor(variablesAndTheirValues));
	    
	    // Parameter for (0, [1, 1]):
	    variablesAndTheirValues.put(sickVariable, 0);
	    Assert.assertEquals(Double.valueOf(0.5), learnedSickNode.getEntryFor(variablesAndTheirValues));
	    
	    // Parameter for (0, [1, 0]):
	    variablesAndTheirValues.put(coldVariable, 0);
	    Assert.assertEquals(Double.valueOf(0.5), learnedSickNode.getEntryFor(variablesAndTheirValues));
	    
	    // Parameter for (0, [0, 1]):
	    variablesAndTheirValues.put(sunVariable, 0);
	    variablesAndTheirValues.put(coldVariable, 1);
	    Assert.assertEquals(Double.valueOf((1.0 + numberOfDatapoints3) / (2 + numberOfDatapoints1 + numberOfDatapoints3)), learnedSickNode.getEntryFor(variablesAndTheirValues));
	    
	    // Parameter for (1, [0, 1]):
	    variablesAndTheirValues.put(sickVariable, 1);
	    Assert.assertEquals(Double.valueOf((1.0 + numberOfDatapoints1) / (2 + numberOfDatapoints1 + numberOfDatapoints3)), learnedSickNode.getEntryFor(variablesAndTheirValues));
	    
	    // For the sunNode:
	 	// Expected parameters (2 datapoints): {(0, [])=0.75, (1, [])=0.25}
	    TableBayesianNode learnedSunNode = learnedNodes.get(1); 
		variablesAndTheirValues = map();
	    variablesAndTheirValues.put(sunVariable, 0);
	    
	    // Parameter for (0, []):
	    Assert.assertEquals(Double.valueOf((1.0 + numberOfDatapoints1 + numberOfDatapoints2 + numberOfDatapoints3) / (2 + numberOfDatapoints1 + numberOfDatapoints2 + numberOfDatapoints3)), learnedSunNode.getEntryFor(variablesAndTheirValues));
	    
	    // Parameter for (1, []):
	    variablesAndTheirValues.put(sunVariable, 1);
	    Assert.assertEquals(Double.valueOf(1.0 / (2 + numberOfDatapoints1 + numberOfDatapoints2 + numberOfDatapoints3)), learnedSunNode.getEntryFor(variablesAndTheirValues));
	    
	    // For the coldNode:
 	 	// Expected parameters (2 datapoints): {(0, [])=0.25, (1, [])=0.75}
 	    TableBayesianNode learnedColdNode = learnedNodes.get(2); 
 		variablesAndTheirValues = map();
 	    variablesAndTheirValues.put(coldVariable, 0);
 	    
 	    // Parameter for (0, []):
 	    Assert.assertEquals(Double.valueOf((1.0 + numberOfDatapoints2) / (2 + numberOfDatapoints1 + numberOfDatapoints2 + numberOfDatapoints3)), learnedColdNode.getEntryFor(variablesAndTheirValues));
 	    
 	    // Parameter for (1, []):
 	    variablesAndTheirValues.put(coldVariable, 1);
 	    Assert.assertEquals(Double.valueOf((1.0 + numberOfDatapoints1 + numberOfDatapoints3) / (2 + numberOfDatapoints1 + numberOfDatapoints2 + numberOfDatapoints3)), learnedColdNode.getEntryFor(variablesAndTheirValues));
 	    
	}
	
	public static void printSickSunColdModelTest() {
		// Dataset
		List<TableVariable> variables = list(sickVariable, sunVariable, coldVariable);
		List<Integer> variableValues = list(1, 0, 1);
		DefaultDatapoint datapoint = new DefaultDatapoint(variables, variableValues);

		List<DefaultDatapoint> datapoints = list();
		int numberOfDatapoints = 2;
		for(int i = 1; i <= numberOfDatapoints; i++) {
			datapoints.add(datapoint);
		}

		DefaultDataset dataset = new DefaultDataset(datapoints);
		
		// Learning
		long startTime = System.currentTimeMillis();
		sickSunColdModel.learnModelParametersFromCompleteData(dataset);
		long stopTime = System.currentTimeMillis();
	    long elapsedTime = stopTime - startTime;
	    System.out.println("Elapsed time for learning with " + numberOfDatapoints + " datapoints: " + elapsedTime + " miliseconds \n");
	    
		List<? extends TableBayesianNode> learnedNodes = sickSunColdModel.getNodes();
		
		// Testing
		String expectedParametersForSick = "{(0, [0, 0])=0.5, (1, [0, 0])=0.5, (0, [0, 1])=0.25, (1, [0, 1])=0.75, (0, [1, 0])=0.5, (1, [1, 0])=0.5, (0, [1, 1])=0.5, (1, [1, 1])=0.5}";
		System.out.println("Expected parameters for sick (with 2 datapoints):\n" + expectedParametersForSick + "\n");
		
		TableBayesianNode learnedSickNode = learnedNodes.get(0); 
		LinkedHashMap<TableVariable, Integer> variablesAndTheirValues = new LinkedHashMap<TableVariable, Integer>();
	    variablesAndTheirValues.put(sickVariable, 1);
	    variablesAndTheirValues.put(sunVariable, 0);
	    variablesAndTheirValues.put(coldVariable, 1);
		
	    System.out.println("Actual entries for sick:");
	    System.out.println("entryFor(" + variablesAndTheirValues.get(sickVariable) + ", [" + variablesAndTheirValues.get(sunVariable) + ", " + variablesAndTheirValues.get(coldVariable) + "]) = " + learnedSickNode.getEntryFor(variablesAndTheirValues));
		
	    variablesAndTheirValues.put(sickVariable, 0);
	    System.out.println("entryFor(" + variablesAndTheirValues.get(sickVariable) + ", [" + variablesAndTheirValues.get(sunVariable) + ", " + variablesAndTheirValues.get(coldVariable) + "]) = " + learnedSickNode.getEntryFor(variablesAndTheirValues));

	    variablesAndTheirValues.put(coldVariable, 0);
	    System.out.println("entryFor(" + variablesAndTheirValues.get(sickVariable) + ", [" + variablesAndTheirValues.get(sunVariable) + ", " + variablesAndTheirValues.get(coldVariable) + "]) = " + learnedSickNode.getEntryFor(variablesAndTheirValues));
	}

	public static void main(String[] args) {
		// printSickSunColdModelTest();
	}

}
