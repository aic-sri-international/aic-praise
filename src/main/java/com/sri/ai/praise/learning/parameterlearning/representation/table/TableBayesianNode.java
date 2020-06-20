package com.sri.ai.praise.learning.parameterlearning.representation.table;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.praise.learning.parameterlearning.BayesianNode;

/**
 * Implementation of Bayesian nodes based on Tables
 * 
 * @author Roger Leite Lucena
 *
 */

public class TableBayesianNode extends ArrayTableFactor implements BayesianNode {
	
	private TableVariable child;
	private ArrayList<TableVariable> parents;
	private ArrayList<? extends TableVariable> allVariables;
	
	public TableBayesianNode(TableVariable child) {
		super(arrayList(child), -1.0);
		this.child = child;
		this.parents = arrayList();
		this.allVariables = this.getVariables();
	}
	
	public TableBayesianNode(TableVariable child, ArrayList<TableVariable> parents) {
		super(mergeElementsIntoOneList(child, parents), -1.0);
		this.allVariables = this.getVariables();
		this.child = child;
		this.parents = parents; 
	}

	@Override
	public TableVariable getChildVariable() {
		return child;
	}

	@Override
	public List<TableVariable> getParentsVariables() {
		return parents;
	}
	
	@Override
	public List<? extends TableVariable> getAllVariables() {
		return allVariables;
	}

	@SuppressWarnings("deprecation")
	@Override
	public void setInitialCountsForAllPossibleChildAndParentsAssignments() {
		this.reinitializeEntries(1.0); // the initial count is 1 for all possible childAndParentsAssignments
	}

	@SuppressWarnings("unchecked")
	@Override
	public void incrementCountForChildAndParentsAssignment(List<? extends Object> childAndParentsValues) {
		verifyIfInputHasExpectedTypeAndSize(childAndParentsValues);
		incrementCountForThatParameter(new ArrayList<Integer>((List<Integer>) childAndParentsValues));
		// TODO: temporarily building a new ArrayList but ideally we should get an ArrayList as the argument.
		// I am not doing it now to minimize changes while taking care of something else.
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public void normalizeParameters() {
		List<ArrayList<Integer>> allPossibleParentsAssignments = getAllPossibleVariablesAssignments(parents);
		for(ArrayList<Integer> parentsAssignment : allPossibleParentsAssignments) {
			double countForThatParentsAssignment = getCountForThatParentsAssignment(parentsAssignment);
			
			ArrayList<Integer> childAndParentsAssignment = mergeElementsIntoOneList(0, parentsAssignment);
			for(int childAssignment : (List<Integer>) child.getValues()) {
				childAndParentsAssignment.set(0, childAssignment);
				double currentParamenterValue = this.getEntryFor(childAndParentsAssignment);
				double newParameterValue = currentParamenterValue / countForThatParentsAssignment;
				this.setEntryFor(childAndParentsAssignment, newParameterValue);
			}
		}
	}
	
	@Override
	public TableBayesianNode copy() {
		TableBayesianNode copy = new TableBayesianNode(this.child, this.parents);
		return copy;
	}
	
	private List<ArrayList<Integer>> getAllPossibleVariablesAssignments(ArrayList<TableVariable> variables) {
		List<ArrayList<Integer>> allPossibleVariablesAssignments = list();
		Iterator<ArrayList<Integer>> iteratorForParentsAssignments = ArrayTableFactor.makeCartesianProductIterator(variables);
		while(iteratorForParentsAssignments.hasNext()) {
			allPossibleVariablesAssignments.add(iteratorForParentsAssignments.next());
		}
		
		return allPossibleVariablesAssignments;
	}
	
	private void incrementCountForThatParameter(ArrayList<Integer> childAndParentsAssignment) {
		double newParameterValue = this.getEntryFor(childAndParentsAssignment) + 1;
		this.setEntryFor(childAndParentsAssignment, newParameterValue);
	}

	private void verifyIfInputHasExpectedTypeAndSize(List<? extends Object> childAndParentsValues) throws Error {
		boolean valuesAreIntegers = childAndParentsValues.isEmpty() || childAndParentsValues.get(0) instanceof Integer;
		if(!valuesAreIntegers) {
			throw new Error("Values for BayesianVariables must be Integers here.");
		}
		if(childAndParentsValues.size() != allVariables.size()) {
			throw new Error("The list of variableValues must have the same size as the list this.allVariables");
		}
	}
	
	@SuppressWarnings("unchecked")
	private double getCountForThatParentsAssignment(ArrayList<Integer> parentsAssignment) {
		double countForThatParentsAssignment = 0.0;
		ArrayList<Integer> childAndParentsAssignment = mergeElementsIntoOneList(0, parentsAssignment);
		for(int childAssignment : (List<Integer>) child.getValues()) {
			childAndParentsAssignment.set(0, childAssignment);
			double countForThatChildAndParentsAssignment = this.getEntryFor(childAndParentsAssignment);
			countForThatParentsAssignment += countForThatChildAndParentsAssignment;
		}
		return countForThatParentsAssignment;
	}
	
	private static <T> ArrayList<T> mergeElementsIntoOneList(T firstElement, Collection<T> otherElements) {
		ArrayList<T> allElements = new ArrayList<>(otherElements.size() + 1);
		allElements.add(firstElement);
		allElements.addAll(otherElements);
		return allElements;
	}
	
	@SuppressWarnings("unused")
	public static void main(String[] args) {
		TableVariable sick = new TableVariable("sick", 2);
		TableVariable sun = new TableVariable("sun", 2);
		TableVariable cold = new TableVariable("cold", 2);
	    
	    new TableBayesianNode(sun, arrayList());
		TableBayesianNode coldNode = new TableBayesianNode(cold, arrayList());
	    TableBayesianNode sickNode = new TableBayesianNode(sick, arrayList(sun, cold));
	    sickNode.setInitialCountsForAllPossibleChildAndParentsAssignments();
	    
	    ArrayList<Integer> childAndParentsValues = new ArrayList<Integer>();
	    childAndParentsValues.add(1);
	    childAndParentsValues.add(0);
	    childAndParentsValues.add(1);
	    
	    sickNode.incrementCountForChildAndParentsAssignment(childAndParentsValues);
	    sickNode.normalizeParameters();
	    
	    LinkedHashMap<TableVariable, Integer> variablesAndTheirValues = new LinkedHashMap<TableVariable, Integer>();
	    variablesAndTheirValues.put(sick, 1);
	    variablesAndTheirValues.put(sun, 0);
	    variablesAndTheirValues.put(cold, 1);
	    
	    System.out.println("Testing entries:");
	    System.out.println("entryFor(" + variablesAndTheirValues.get(sick) + ", [" + variablesAndTheirValues.get(sun) + ", " + variablesAndTheirValues.get(cold) + "]) = " + sickNode.getEntryFor(variablesAndTheirValues));
	    // System.out.println("all entries: " + node.getEntries());
	}

}
