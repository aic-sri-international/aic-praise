package com.sri.ai.praise.learning.parameterlearning;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.learning.parameterlearning.representation.Dataset.DefaultDatapoint;
import com.sri.ai.praise.learning.parameterlearning.representation.Dataset.DefaultDataset;
import com.sri.ai.praise.learning.parameterlearning.representation.Table.TableBayesianModel;
import com.sri.ai.praise.learning.parameterlearning.representation.Table.TableBayesianNode;
import com.sri.ai.praise.learning.parameterlearning.representation.Table.TableBayesianVariable;

// Would be the equivalent of FactorNetwork
public interface BayesianModel {
	
	public List<? extends BayesianNode> getNodes();
	
	default public void learnModelParametersFromCompleteData(Dataset dataset) {
		List<? extends BayesianNode> nodes = this.getNodes();
		for(BayesianNode node : nodes) {
			node.setParametersGivenCompleteData(dataset);
		}
	}

	public static void main(String[] args) {
		// Model
		TableBayesianVariable sickVariable = new TableBayesianVariable("sick", 2);
	    TableBayesianVariable sunVariable = new TableBayesianVariable("sun", 2);
	    
	    ArrayList<TableBayesianVariable> parentsOfSick = arrayList();
	    parentsOfSick.add(sunVariable);
	    
	    TableBayesianNode sickNode = new TableBayesianNode(sickVariable, parentsOfSick);
	    List<TableBayesianNode> nodes = list(sickNode);
	    
	    TableBayesianModel model = new TableBayesianModel(nodes);
	    
	    // Dataset
	    List<TableBayesianVariable> variables = list(sickVariable);
	    List<Integer> variableValues = list(1);
	    
	    // With sun:
	    variables.add(sunVariable);
	    variableValues.add(0);
	    
	    DefaultDatapoint datapoint = new DefaultDatapoint(variables, variableValues);
	    DefaultDatapoint datapoint2 = new DefaultDatapoint(variables, variableValues);
	    
	    List<DefaultDatapoint> datapoints = list();
	    datapoints.add(datapoint);
	    datapoints.add(datapoint2);
	    
	    DefaultDataset dataset = new DefaultDataset(datapoints);
	    model.learnModelParametersFromCompleteData(dataset);
	    List<TableBayesianNode> learnedNodes = (List<TableBayesianNode>) model.getNodes();
	    for(TableBayesianNode node : learnedNodes) {
	    	System.out.println(node.getParameters());
	    }
	}

}
    

