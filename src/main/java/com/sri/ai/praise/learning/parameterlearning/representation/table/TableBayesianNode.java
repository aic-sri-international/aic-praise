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

public class TableBayesianNode extends TableFactor implements BayesianNode {
	
	private TableVariable child;
	private ArrayList<TableVariable> parents;
	private ArrayList<TableVariable> allVariables;
	private LinkedHashMap<ArrayList<Integer>, Double> countsForParentsAssignment; 
	//
	private List<ArrayList<Integer>> allPossibleParentsAssignments;
	
	public TableBayesianNode(TableVariable child) {
		super(arrayList(child));
		this.child = child;
		this.parents = arrayList();
		this.allVariables = this.getVariables();
		countsForParentsAssignment = new LinkedHashMap<ArrayList<Integer>, Double>();
		
		this.allPossibleParentsAssignments = getAllPossibleVariablesAssignments(this.parents);
	}
	
	public TableBayesianNode(TableVariable child, ArrayList<TableVariable> parents) {
		super(mergeElementsIntoOneList(child, parents));
		this.child = child;
		this.parents = parents;
		this.allVariables = this.getVariables();
		countsForParentsAssignment = new LinkedHashMap<ArrayList<Integer>, Double>();
		
		this.allPossibleParentsAssignments = getAllPossibleVariablesAssignments(this.parents);
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
	public void setInitialCountsForAllPossibleChildAndParentsAssignments() {
		this.reinitializeEntries(1.0); // the initial count is 1 for all possible childAndParentsAssignments
		setResultingInitialCountForParentsAssignments();
	}

	@Override
	public void incrementCountForChildAndParentsAssignment(Object childValue, List<? extends Object> parentsValues) {
		verifyIfParametersHaveExpectedType(childValue, parentsValues);
		
		incrementCountForThatParentsAssignment(parentsValues);
		
		incrementCountForThatParameter(mergeElementsIntoOneList((Integer) childValue, (List<Integer>) parentsValues));
	}
	
	@Override
	public void normalizeParametersAndFillEntries() {
		for(ArrayList<Integer> parentsAssignment : allPossibleParentsAssignments) {
			double countForThatParentsAssignment = countsForParentsAssignment.get(parentsAssignment);
			for(int childAssignment : (List<Integer>) child.getValues()) {
				List<Integer> childAndParentsAssignment = mergeElementsIntoOneList(childAssignment, parentsAssignment);
				double currentParamenterValue = this.getEntryFor(childAndParentsAssignment);
				double newParameterValue = currentParamenterValue/countForThatParentsAssignment;
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
	
	private void setResultingInitialCountForParentsAssignments() {
		double numberOfPossibleChildAssignments = child.getValues().size();
		for(ArrayList<Integer> parentsAssignment : allPossibleParentsAssignments) {
			countsForParentsAssignment.put(parentsAssignment, numberOfPossibleChildAssignments);
		}
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
	
	private void incrementCountForThatParameter(List<Integer> childAndParentsAssignment) {
		double newParameterValue = this.getEntryFor(childAndParentsAssignment) + 1;
		this.setEntryFor(childAndParentsAssignment, newParameterValue);
	}

	private void incrementCountForThatParentsAssignment(List<? extends Object> parentsValues) {
		double countForThatParentsAssignment = countsForParentsAssignment.get(parentsValues);
		countsForParentsAssignment.put((ArrayList<Integer>) parentsValues, countForThatParentsAssignment + 1);
	}

	private void verifyIfParametersHaveExpectedType(Object childValue, List<? extends Object> parentsValues) throws Error {
		boolean childValueIsInteger = childValue instanceof Integer;
		boolean parentsValuesAreIntegers = parentsValues.isEmpty() || parentsValues.get(0) instanceof Integer;
		if(!childValueIsInteger || !parentsValuesAreIntegers) {
			throw new Error("Values for BayesianVariables must be Integers.");
		}
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
	    
	    LinkedHashMap<TableVariable, Integer> variablesAndTheirValues = new LinkedHashMap<TableVariable, Integer>();
	    variablesAndTheirValues.put(sick, 1);
	    variablesAndTheirValues.put(sun, 0);
	    variablesAndTheirValues.put(cold, 1);
	    
	    System.out.println("Testing entries:");
	    System.out.println("entryFor(" + variablesAndTheirValues.get(sick) + ", [" + variablesAndTheirValues.get(sun) + ", " + variablesAndTheirValues.get(cold) + "]) = " + node.getEntryFor(variablesAndTheirValues));
	    // System.out.println("all entries: " + node.getEntries());
	}

}
