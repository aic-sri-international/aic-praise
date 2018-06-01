package com.sri.ai.praise.learning.parameterlearning.representation.table;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.count;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapFromListOfKeysAndListOfValues;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.sri.ai.praise.inference.generic.representation.api.Variable;
import com.sri.ai.praise.inference.generic.representation.table.TableFactor;
import com.sri.ai.praise.inference.generic.representation.table.TableVariable;
import com.sri.ai.praise.learning.parameterlearning.BayesianNode;

/**
 * Implementation of Bayesian nodes based on tables
 * 
 * @author Roger Leite Lucena
 *
 */

public class TableBayesianNode extends TableFactor implements BayesianNode {
	
	private TableVariable child;
	private ArrayList<TableVariable> parents;
	private ArrayList<TableVariable> allVariables;
	
	public TableBayesianNode(TableVariable child) {
		super(arrayList(child));
		this.child = child;
		this.parents = arrayList();
		this.allVariables = this.getVariables();
	}
	
	public TableBayesianNode(TableVariable child, ArrayList<TableVariable> parents) {
		super(mergeElementsIntoOneList(child, parents));
		this.child = child;
		this.parents = parents;
		this.allVariables = this.getVariables();
	}

	@Override
	public Variable getChild() {
		return child;
	}

	@Override
	public List<? extends Variable> getParents() {
		return parents;
	}
	
	@Override
	public List<? extends Variable> getAllVariables() {
		return allVariables;
	}

	@Override
	public void setInitialCountsForAllPossibleChildAndParentsAssignments() {
		this.reinitializeEntries(1.0); // the initial count is 1 for all possible childAndParentsAssignments
	}

	@Override
	public void incrementCountForChildAndParentsAssignment(List<? extends Object> childAndParentsValues) {
		verifyIfParametersHaveExpectedType(childAndParentsValues);
		incrementCountForThatParameter((List<Integer>) childAndParentsValues);
	}
	
	@Override
	public void normalizeParameters() {
		List<ArrayList<Integer>> allPossibleParentsAssignments = getAllPossibleVariablesAssignments(parents);
		for(ArrayList<Integer> parentsAssignment : allPossibleParentsAssignments) {
			double countForThatParentsAssignment = getCountForThatParentsAssignment(parentsAssignment);
			
			List<Integer> childAndParentsAssignment = mergeElementsIntoOneList(0, parentsAssignment);
			for(int childAssignment : (List<Integer>) child.getValues()) {
				childAndParentsAssignment.set(0, childAssignment);
				double currentParamenterValue = this.getEntryFor(childAndParentsAssignment);
				double newParameterValue = currentParamenterValue / countForThatParentsAssignment;
				this.setEntryFor(childAndParentsAssignment, newParameterValue);
			}
		}
	}
	
	private static <T> ArrayList<T> mergeElementsIntoOneList(T firstElement, List<T> otherElements) {
		ArrayList<T> allElements = new ArrayList<>(otherElements.size() + 1);
		allElements.add(firstElement);
		allElements.addAll(otherElements);
		return allElements;
	}
	
	private List<ArrayList<Integer>> getAllPossibleVariablesAssignments(ArrayList<TableVariable> variables) {
		List<ArrayList<Integer>> allPossibleVariablesAssignments = list();
		Iterator<ArrayList<Integer>> iteratorForParentsAssignments = TableFactor.getCartesianProduct(variables);
		while(iteratorForParentsAssignments.hasNext()) {
			allPossibleVariablesAssignments.add(iteratorForParentsAssignments.next());
		}
		
		return allPossibleVariablesAssignments;
	}
	
	private void incrementCountForThatParameter(List<Integer> childAndParentsAssignment) {
		double newParameterValue = this.getEntryFor(childAndParentsAssignment) + 1;
		this.setEntryFor(childAndParentsAssignment, newParameterValue);
	}

	private void verifyIfParametersHaveExpectedType(List<? extends Object> childAndParentsValues) throws Error {
		boolean valuesAreIntegers = childAndParentsValues.isEmpty() || childAndParentsValues.get(0) instanceof Integer;
		if(!valuesAreIntegers) {
			throw new Error("Values for BayesianVariables must be Integers.");
		}
	}
	
	private double getCountForThatParentsAssignment(ArrayList<Integer> parentsAssignment) {
		double countForThatParentsAssignment = 0.0;
		List<Integer> childAndParentsAssignment = mergeElementsIntoOneList(0, parentsAssignment);
		for(int childAssignment : (List<Integer>) child.getValues()) {
			childAndParentsAssignment.set(0, childAssignment);
			double countForThatChildAndParentsAssignment = this.getEntryFor(childAndParentsAssignment);
			countForThatParentsAssignment += countForThatChildAndParentsAssignment;
		}
		return countForThatParentsAssignment;
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
	    
	    ArrayList<Integer> childAndParentsValues = new ArrayList<Integer>();
	    childAndParentsValues.add(1);
	    childAndParentsValues.add(0);
	    childAndParentsValues.add(1);
	    
	    node.incrementCountForChildAndParentsAssignment(childAndParentsValues);
	    node.normalizeParameters();
	    
	    LinkedHashMap<TableVariable, Integer> variablesAndTheirValues = new LinkedHashMap<TableVariable, Integer>();
	    variablesAndTheirValues.put(sick, 1);
	    variablesAndTheirValues.put(sun, 0);
	    variablesAndTheirValues.put(cold, 1);
	    
	    System.out.println("Testing entries:");
	    System.out.println("entryFor(" + variablesAndTheirValues.get(sick) + ", [" + variablesAndTheirValues.get(sun) + ", " + variablesAndTheirValues.get(cold) + "]) = " + node.getEntryFor(variablesAndTheirValues));
	    // System.out.println("all entries: " + node.getEntries());
	}

}
