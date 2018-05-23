// Old TableBayesianNode.java

package com.sri.ai.praise.learning.parameterlearning.representation.table;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import com.sri.ai.praise.learning.parameterlearning.BayesianNode;
import com.sri.ai.praise.inference.generic.representation.table.TableVariable;
import com.sri.ai.util.base.Pair;

public class TableBayesianNode implements BayesianNode {
	
	private TableVariable child;
	private ArrayList<TableVariable> parents;
	private LinkedHashMap<Pair<Integer, ArrayList<Integer>>, Double> parametersForChildAndParentsAssignment; 
	private LinkedHashMap<ArrayList<Integer>, Double> countsForParentsAssignment; 
	 
	public TableBayesianNode(TableVariable child) {
		this.child = child;
		this.parents = new ArrayList<TableVariable>();
		parametersForChildAndParentsAssignment = new LinkedHashMap<Pair<Integer, ArrayList<Integer>>, Double>();
		countsForParentsAssignment = new LinkedHashMap<ArrayList<Integer>, Double>();
	}
	
	public TableBayesianNode(TableVariable child, ArrayList<TableVariable> parents) {
		this.child = child;
		this.parents = parents;
		parametersForChildAndParentsAssignment = new LinkedHashMap<Pair<Integer, ArrayList<Integer>>, Double>();
		countsForParentsAssignment = new LinkedHashMap<ArrayList<Integer>, Double>();
	}
	
	public LinkedHashMap<Pair<Integer, ArrayList<Integer>>, Double> getParameters() {
		return parametersForChildAndParentsAssignment;
	}
	
	@Override
	public TableVariable getChild() {
		return child;
	}

	@Override
	public List<? extends TableVariable> getParents() {
		return parents;
	}

	@Override
	public void setInitialCountsForAllPossibleChildAndParentsAssignments() {
		ArrayList<Integer> allPossibleNodeAssignments = (ArrayList<Integer>) child.getValues();
		ArrayList<ArrayList<Integer>> allPossibleParentsAssignments = getAllPossibleParentsAssignments(parents);
		
		for(ArrayList<Integer> parentsAssignment : allPossibleParentsAssignments) {
			for(int nodeAssignment : allPossibleNodeAssignments) {
				Pair<Integer, ArrayList<Integer>> nodeAndParentsAssignement = new Pair<Integer, ArrayList<Integer>>(nodeAssignment,  parentsAssignment);
				parametersForChildAndParentsAssignment.put(nodeAndParentsAssignement, 1.0);
			}
			countsForParentsAssignment.put(parentsAssignment, (double) allPossibleNodeAssignments.size());
		}
	}
	
	@Override
	public void incrementCountForChildAndParentsAssignment(Object nodeValue, List<? extends Object> parentsValues) {
		boolean nodeValueIsInteger = nodeValue instanceof Integer;
		boolean parentsValuesAreIntegers = parentsValues.isEmpty() || parentsValues.get(0) instanceof Integer;
		if(!nodeValueIsInteger || !parentsValuesAreIntegers) {
			throw new Error("Values must be Integers.");
		}
		
		double countForThatParentsAssignment = countsForParentsAssignment.get(parentsValues);
		countsForParentsAssignment.put((ArrayList<Integer>) parentsValues, countForThatParentsAssignment + 1);
		
		Pair<Integer, ArrayList<Integer>> nodeAndParentsAssignement = new Pair(nodeValue, parentsValues);
		double newParameterValue = parametersForChildAndParentsAssignment.get(nodeAndParentsAssignement) + 1;
		parametersForChildAndParentsAssignment.put(nodeAndParentsAssignement, newParameterValue);
	}
	
	private ArrayList<ArrayList<Integer>> getAllPossibleParentsAssignments(List<TableVariable> parents) {
		ArrayList<ArrayList<Integer>> allPossibleParentsAssignments = new ArrayList<ArrayList<Integer>>();
		if(parents.isEmpty()) {
			allPossibleParentsAssignments.add(new ArrayList<Integer>());
		}
		else {
			TableVariable lastVariable = parents.get(parents.size()-1);
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
		for(Pair<Integer, ArrayList<Integer>> key : parametersForChildAndParentsAssignment.keySet()) {
			double currentParameterValue = parametersForChildAndParentsAssignment.get(key);
			double countForThatParentsAssignment = countsForParentsAssignment.get(key.second);
			double newParameterValue = currentParameterValue / countForThatParentsAssignment;
			parametersForChildAndParentsAssignment.put(key, newParameterValue);
		}
	}
	
	public static void main(String[] args) {
		TableVariable sick = new TableVariable("sick", 2);
		TableVariable sun = new TableVariable("sun", 2);
		TableVariable cold = new TableVariable("cold", 2);
	    
	    ArrayList<TableVariable> parents = new ArrayList<TableVariable>();
	    parents.add(sun);
	    parents.add(cold);
	    
	    TableBayesianNode node = new TableBayesianNode(sick, parents);
	    node.setInitialCountsForAllPossibleChildAndParentsAssignments();
	    
	    int nodeValue = 1;
	    ArrayList<Integer> parentsValues = new ArrayList<Integer>();
	    parentsValues.add(0);
	    parentsValues.add(1);
	    
	    node.incrementCountForChildAndParentsAssignment(nodeValue, parentsValues);
	    node.normalizeParameters();
	    System.out.println(node.getParameters());
	}

}
