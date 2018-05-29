package com.sri.ai.test.praise.learning.parameterlearning;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.praise.inference.generic.representation.table.TableVariable;
import com.sri.ai.praise.learning.parameterlearning.representation.dataset.DefaultDatapoint;
import com.sri.ai.praise.learning.parameterlearning.representation.dataset.DefaultDataset;
import com.sri.ai.praise.learning.parameterlearning.representation.table.TableBayesianModel;
import com.sri.ai.praise.learning.parameterlearning.representation.table.TableBayesianNode;

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

		// TableBayesianModel sickSunColdModel = new TableBayesianModel(nodes);
		
		return sickSunColdModel;
	}
	
	@Test
	public static void testSickSunColdModel() {
		// Dataset
		List<TableVariable> variables = list(sickVariable, sunVariable, coldVariable);
		List<Integer> variableValues = list(1, 0, 1);
		DefaultDatapoint datapoint = new DefaultDatapoint(variables, variableValues);

		List<DefaultDatapoint> datapoints = list();
		datapoints.add(datapoint);
		datapoints.add(datapoint);

		DefaultDataset dataset = new DefaultDataset(datapoints);
		
		// Learning
		sickSunColdModel.learnModelParametersFromCompleteData(dataset);
		List<TableBayesianNode> learnedNodes = (List<TableBayesianNode>) sickSunColdModel.getNodes();
		
		// Testing
		String expectedParametersForSick = "{(0, [0, 0])=0.5, (1, [0, 0])=0.5, (0, [0, 1])=0.25, (1, [0, 1])=0.75, (0, [1, 0])=0.5, (1, [1, 0])=0.5, (0, [1, 1])=0.5, (1, [1, 1])=0.5}";
		String actualParametersForSick = learnedNodes.get(0).getParameters().toString(); 
		Assert.assertEquals(expectedParametersForSick, actualParametersForSick);
		
		String expectedParametersForSun = "{(0, [])=0.75, (1, [])=0.25}";
		String actualParametersForSun = learnedNodes.get(1).getParameters().toString(); 
		Assert.assertEquals(expectedParametersForSun, actualParametersForSun);
		
		String expectedParametersForCold = "{(0, [])=0.25, (1, [])=0.75}";
		String actualParametersForCold = learnedNodes.get(2).getParameters().toString(); 
		Assert.assertEquals(expectedParametersForCold, actualParametersForCold);
	}
	
	public static void testSickSunColdModelPrintingParameters() {
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
	    System.out.println("Elapsed time for learning with " + numberOfDatapoints + " datapoints: " + elapsedTime + " miliseconds");
		
	    List<TableBayesianNode> learnedNodes = (List<TableBayesianNode>) sickSunColdModel.getNodes();
		
		for (TableBayesianNode node : learnedNodes) {
			System.out.println("Parameters for node: " + node.getChild().getName());
			System.out.println(node.getParameters() + "\n");
		}
	}

	public static void main(String[] args) {
		testSickSunColdModelPrintingParameters();
	}

}
