package com.sri.ai.praise.learning.parameterlearning;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.learning.parameterlearning.representation.dataset.DefaultDatapoint;
import com.sri.ai.praise.learning.parameterlearning.representation.dataset.DefaultDataset;
import com.sri.ai.praise.learning.parameterlearning.representation.table.TableBayesianModel;
import com.sri.ai.praise.learning.parameterlearning.representation.table.TableBayesianNode;
import com.sri.ai.praise.inference.generic.representation.table.TableVariable;

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
    

