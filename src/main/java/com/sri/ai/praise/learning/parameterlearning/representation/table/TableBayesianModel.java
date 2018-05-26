package com.sri.ai.praise.learning.parameterlearning.representation.table;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.inference.generic.representation.table.TableFactorNetwork;
import com.sri.ai.praise.inference.generic.representation.table.TableVariable;
import com.sri.ai.praise.learning.parameterlearning.BayesianModel;
import com.sri.ai.praise.learning.parameterlearning.BayesianNode;
import com.sri.ai.praise.learning.parameterlearning.representation.dataset.DefaultDatapoint;
import com.sri.ai.praise.learning.parameterlearning.representation.dataset.DefaultDataset;

public class TableBayesianModel extends TableFactorNetwork implements BayesianModel {
	
	private List<TableBayesianNode> nodes;
	
	public TableBayesianModel(List<TableBayesianNode> nodes) {
		super(nodes);
		this.nodes = nodes;
	}
	
	public List<? extends BayesianNode> getNodes() {
		return nodes;
	}
	
	public static void main(String[] args) {
		/*
		// Model
		TableVariable sickVariable = new TableVariable("sick", 2);
		TableVariable sunVariable = new TableVariable("sun", 2);
	    
	    ArrayList<TableVariable> parentsOfSick = arrayList();
	    parentsOfSick.add(sunVariable);
	    
	    TableBayesianNode sickNode = new TableBayesianNode(sickVariable, parentsOfSick);
	    List<TableBayesianNode> nodes = list(sickNode);
	    
	    TableBayesianModel model = new TableBayesianModel(nodes);
	    
	    // Dataset
	    List<TableVariable> variables = list(sickVariable);
	    List<Integer> variableValues = list(1);
	    
	    // With sun:
	    variables.add(sunVariable);
	    variableValues.add(0);
	    
	    DefaultDatapoint datapoint = new DefaultDatapoint(variables, variableValues);
	    
	    List<DefaultDatapoint> datapoints = list();
	    datapoints.add(datapoint);
	    datapoints.add(datapoint);
	    
	    DefaultDataset dataset = new DefaultDataset(datapoints);
	    model.learnModelParametersFromCompleteData(dataset);
	    List<TableBayesianNode> learnedNodes = (List<TableBayesianNode>) model.getNodes();
	    for(TableBayesianNode node : learnedNodes) {
	    	System.out.println(node.getParameters());
	    }
	    */
	}
	
}
