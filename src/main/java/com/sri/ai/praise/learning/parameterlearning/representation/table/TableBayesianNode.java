// Old TableBayesianNode.java

package com.sri.ai.praise.learning.parameterlearning.representation.table;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;

import com.sri.ai.praise.learning.parameterlearning.BayesianNode;
import com.sri.ai.praise.inference.generic.representation.table.TableFactor;
import com.sri.ai.praise.inference.generic.representation.table.TableVariable;
import com.sri.ai.util.base.Pair;

public class TableBayesianNode extends TableFactor implements BayesianNode {
	
	private TableVariable child;
	private ArrayList<TableVariable> parents;
	private LinkedHashMap<Pair<Integer, ArrayList<Integer>>, Double> parametersForChildAndParentsAssignment; 
	private LinkedHashMap<ArrayList<Integer>, Double> countsForParentsAssignment; 
	
	public TableBayesianNode(TableVariable child) {
		super(arrayList(child));
		this.child = child;
		this.parents = new ArrayList<TableVariable>();
		parametersForChildAndParentsAssignment = new LinkedHashMap<Pair<Integer, ArrayList<Integer>>, Double>();
		countsForParentsAssignment = new LinkedHashMap<ArrayList<Integer>, Double>();
	}
	
	public TableBayesianNode(TableVariable child, ArrayList<TableVariable> parents) {
		super(mergeVariablesToOneList(child, parents));
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
		ArrayList<Integer> allPossibleChildAssignments = (ArrayList<Integer>) child.getValues();
		List<ArrayList<Integer>> allPossibleParentsAssignments = getAllPossibleVariablesAssignments(parents);
		
		for(ArrayList<Integer> parentsAssignment : allPossibleParentsAssignments) {
			for(int childAssignment : allPossibleChildAssignments) {
				Pair<Integer, ArrayList<Integer>> childAndParentsAssignement = new Pair<Integer, ArrayList<Integer>>(childAssignment,  parentsAssignment);
				parametersForChildAndParentsAssignment.put(childAndParentsAssignement, 1.0);
			}
			countsForParentsAssignment.put(parentsAssignment, (double) allPossibleChildAssignments.size());
		}
	}
	
	@Override
	public void incrementCountForChildAndParentsAssignment(Object childValue, List<? extends Object> parentsValues) {
		boolean childValueIsInteger = childValue instanceof Integer;
		boolean parentsValuesAreIntegers = parentsValues.isEmpty() || parentsValues.get(0) instanceof Integer;
		if(!childValueIsInteger || !parentsValuesAreIntegers) {
			throw new Error("Values must be Integers.");
		}
		
		double countForThatParentsAssignment = countsForParentsAssignment.get(parentsValues);
		countsForParentsAssignment.put((ArrayList<Integer>) parentsValues, countForThatParentsAssignment + 1);
		
		Pair<Integer, ArrayList<Integer>> childAndParentsAssignement = new Pair(childValue, parentsValues);
		double newParameterValue = parametersForChildAndParentsAssignment.get(childAndParentsAssignement) + 1;
		parametersForChildAndParentsAssignment.put(childAndParentsAssignement, newParameterValue);
	}
	
	@Override
	public void normalizeParametersAndFillEntries() {
		for(Pair<Integer, ArrayList<Integer>> key : parametersForChildAndParentsAssignment.keySet()) {
			double currentParameterValue = parametersForChildAndParentsAssignment.get(key);
			double countForThatParentsAssignment = countsForParentsAssignment.get(key.second);
			double newParameterValue = currentParameterValue / countForThatParentsAssignment;
			parametersForChildAndParentsAssignment.put(key, newParameterValue);
		}
		
		fillEntriesWithParameterValues();
	}
	
	private void fillEntriesWithParameterValues() {
		for(Pair<Integer, ArrayList<Integer>> childAndParentsAssignment : parametersForChildAndParentsAssignment.keySet()) {
			LinkedHashMap<TableVariable, Integer> variablesAndTheirValues = new LinkedHashMap<TableVariable, Integer>();
			variablesAndTheirValues.put(child, childAndParentsAssignment.first);
			
			ArrayList<Integer> parentsValues = childAndParentsAssignment.second;
			for(int i = 0; i < parents.size(); i++) {
				variablesAndTheirValues.put(parents.get(i), parentsValues.get(i));
			}
			
			double parameterValue = parametersForChildAndParentsAssignment.get(childAndParentsAssignment);
			this.setEntryFor(variablesAndTheirValues, parameterValue);
		}
	}
	
	private static ArrayList<TableVariable> mergeVariablesToOneList(TableVariable firstVariable, ArrayList<TableVariable> otherVariables) {
		ArrayList<TableVariable> allVariables = new ArrayList<TableVariable>(otherVariables.size() + 1);
		allVariables.add(firstVariable);
		allVariables.addAll(otherVariables);
		return allVariables;
	}
	
	private List<ArrayList<Integer>> getAllPossibleVariablesAssignments(ArrayList<TableVariable> variables) {
		List<ArrayList<Integer>> allPossibleVariablesAssignments = list();
		if(variables.isEmpty()) {
			allPossibleVariablesAssignments.add(new ArrayList<Integer>());
		}
		else {
			Iterator<ArrayList<Integer>> iteratorForParentsAssignments = TableFactor.getCartesianProduct(variables);
			while(iteratorForParentsAssignments.hasNext()) {
				allPossibleVariablesAssignments.add(iteratorForParentsAssignments.next());
			}
		}
			
		return allPossibleVariablesAssignments;
	}
	
	public static void main(String[] args) {
		TableVariable sick = new TableVariable("sick", 2);
		TableVariable sun = new TableVariable("sun", 2);
		TableVariable cold = new TableVariable("cold", 2);
	    
	    ArrayList<TableVariable> parentsOfSick = new ArrayList<TableVariable>();
	    parentsOfSick.add(sun);
	    parentsOfSick.add(cold);
	    
	    TableBayesianNode node = new TableBayesianNode(sick, parentsOfSick);
	    node.setInitialCountsForAllPossibleChildAndParentsAssignments();
	    
	    int childValue = 1;
	    ArrayList<Integer> parentsValues = new ArrayList<Integer>();
	    parentsValues.add(0);
	    parentsValues.add(1);
	    
	    node.incrementCountForChildAndParentsAssignment(childValue, parentsValues);
	    node.normalizeParametersAndFillEntries();
	    System.out.println(node.getParameters());
	    
	    LinkedHashMap<TableVariable, Integer> variablesAndTheirValues = new LinkedHashMap<TableVariable, Integer>();
	    variablesAndTheirValues.put(sick, 1);
	    variablesAndTheirValues.put(sun, 0);
	    variablesAndTheirValues.put(cold, 1);
	    
	    System.out.println("Testing entries:");
	    System.out.println("entryFor(" + variablesAndTheirValues.get(sick) + ", [" + variablesAndTheirValues.get(sun) + ", " + variablesAndTheirValues.get(cold) + "]) = " + node.getEntryFor(variablesAndTheirValues));
	    // System.out.println("all entries: " + node.getEntries());
	}

}
