package com.sri.ai.praise.learning.parameterlearning.representation.Table;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import com.sri.ai.praise.learning.parameterlearning.BayesianNode;
import com.sri.ai.praise.learning.parameterlearning.Variable;
import com.sri.ai.util.base.Pair;

public class TableBayesianNode implements BayesianNode {
	
	private TableBayesianVariable variable;
	private ArrayList<TableBayesianVariable> parents;
	private LinkedHashMap<Pair<Integer, ArrayList<Integer>>, Double> parametersForNodeAndParentsAssignment; 
	private LinkedHashMap<ArrayList<Integer>, Double> countsForParentsAssignment; 
	 
	public TableBayesianNode(TableBayesianVariable variable) {
		this.variable = variable;
		this.parents = new ArrayList<TableBayesianVariable>();
		parametersForNodeAndParentsAssignment = new LinkedHashMap<Pair<Integer, ArrayList<Integer>>, Double>();
		countsForParentsAssignment = new LinkedHashMap<ArrayList<Integer>, Double>();
	}
	
	public TableBayesianNode(TableBayesianVariable variable, ArrayList<TableBayesianVariable> parents) {
		this.variable = variable;
		this.parents = parents;
		parametersForNodeAndParentsAssignment = new LinkedHashMap<Pair<Integer, ArrayList<Integer>>, Double>();
		countsForParentsAssignment = new LinkedHashMap<ArrayList<Integer>, Double>();
	}
	
	public LinkedHashMap<Pair<Integer, ArrayList<Integer>>, Double> getParameters() {
		return parametersForNodeAndParentsAssignment;
	}
	
	@Override
	public Variable getVariable() {
		return variable;
	}

	@Override
	public List<? extends Variable> getParents() {
		return parents;
	}

	@Override
	public void setInitialCountsForAllPossibleNodeAndParentsAssignments() {
		ArrayList<Integer> allPossibleNodeAssignments = (ArrayList<Integer>) variable.getValues();
		ArrayList<ArrayList<Integer>> allPossibleParentsAssignments = getAllPossibleParentsAssignments(parents);
		
		for(ArrayList<Integer> parentsAssignment : allPossibleParentsAssignments) {
			for(int nodeAssignment : allPossibleNodeAssignments) {
				Pair<Integer, ArrayList<Integer>> nodeAndParentsAssignement = new Pair<Integer, ArrayList<Integer>>(nodeAssignment,  parentsAssignment);
				parametersForNodeAndParentsAssignment.put(nodeAndParentsAssignement, 1.0);
			}
			countsForParentsAssignment.put(parentsAssignment, (double) allPossibleNodeAssignments.size());
		}
	}
	
	@Override
	public void incrementCountForNodeAndParentsAssignment(Object nodeValue, List<? extends Object> parentsValues) {
		boolean nodeValueIsInteger = nodeValue instanceof Integer;
		boolean parentsValuesAreIntegers = parentsValues.isEmpty() || parentsValues.get(0) instanceof Integer;
		if(!nodeValueIsInteger || !parentsValuesAreIntegers) {
			throw new Error("Values must be Integers.");
		}
		
		double countForThatParentsAssignment = countsForParentsAssignment.get(parentsValues);
		countsForParentsAssignment.put((ArrayList<Integer>) parentsValues, countForThatParentsAssignment + 1);
		
		Pair<Integer, ArrayList<Integer>> nodeAndParentsAssignement = new Pair(nodeValue, parentsValues);
		double newParameterValue = parametersForNodeAndParentsAssignment.get(nodeAndParentsAssignement) + 1;
		parametersForNodeAndParentsAssignment.put(nodeAndParentsAssignement, newParameterValue);
	}
	
	private ArrayList<ArrayList<Integer>> getAllPossibleParentsAssignments(List<TableBayesianVariable> parents) {
		ArrayList<ArrayList<Integer>> allPossibleParentsAssignments = new ArrayList<ArrayList<Integer>>();
		if(parents.isEmpty()) {
			allPossibleParentsAssignments.add(new ArrayList<Integer>());
		}
		else {
			TableBayesianVariable lastVariable = parents.get(parents.size()-1);
			List<ArrayList<Integer>> allPossibleParentsAssignmentsWithoutLastVariable = getAllPossibleParentsAssignments(parents.subList(0, parents.size()-1));
			for(ArrayList<Integer> parentsAssignment : allPossibleParentsAssignmentsWithoutLastVariable) {
				for(int lastVariableAssignment : (ArrayList<Integer>) lastVariable.getValues()) {
					parentsAssignment.add(lastVariableAssignment);
					allPossibleParentsAssignments.add((ArrayList<Integer>) parentsAssignment.clone());
					parentsAssignment.remove(parentsAssignment.size()-1);
				}
			}
		}
			
		return allPossibleParentsAssignments;
	}
	
	@Override
	public void normalizeParameters() {
		for(Pair<Integer, ArrayList<Integer>> key : parametersForNodeAndParentsAssignment.keySet()) {
			double currentParameterValue = parametersForNodeAndParentsAssignment.get(key);
			double countForThatParentsAssignment = countsForParentsAssignment.get(key.second);
			double newParameterValue = currentParameterValue / countForThatParentsAssignment;
			parametersForNodeAndParentsAssignment.put(key, newParameterValue);
		}
	}
	
	public static void main(String[] args) {
		TableBayesianVariable sick = new TableBayesianVariable("sick", 2);
	    TableBayesianVariable sun = new TableBayesianVariable("sun", 2);
	    TableBayesianVariable cold = new TableBayesianVariable("cold", 2);
	    
	    ArrayList<TableBayesianVariable> parents = new ArrayList<TableBayesianVariable>();
	    parents.add(sun);
	    parents.add(cold);
	    
	    TableBayesianNode node = new TableBayesianNode(sick, parents);
	    node.setInitialCountsForAllPossibleNodeAndParentsAssignments();
	    
	    int nodeValue = 1;
	    ArrayList<Integer> parentsValues = new ArrayList<Integer>();
	    parentsValues.add(0);
	    parentsValues.add(1);
	    
	    node.incrementCountForNodeAndParentsAssignment(nodeValue, parentsValues);
	    node.normalizeParameters();
	    System.out.println(node.getParameters());
	}

}
