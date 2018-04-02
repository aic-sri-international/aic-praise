package com.sri.ai.praise.inference.representation.Table;

import static com.sri.ai.util.base.IdentityWrapper.identityWrapper;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import com.sri.ai.praise.inference.representation.core.AbstractFactorNetwork;
import com.sri.ai.praise.lang.grounded.markov.FactorTable;
import com.sri.ai.praise.model.v1.imports.uai.UAIModel;
/**
 * TODO Not Tested
 * 
 * @author gabriel
 *
 */
public class TableFactorNetwork extends AbstractFactorNetwork{
	
	public TableFactorNetwork(List<TableFactor> factors) {
		for(TableFactor f:factors) {
			for(TableVariable v: f.getVariables()) {
				this.add(identityWrapper(f), v);
			}
		}
	}
	
	public TableFactorNetwork(UAIModel model) {
		this(UAIModelToListOfFactors(model));
	}

	private static List<TableFactor> UAIModelToListOfFactors(UAIModel model) {
		LinkedHashMap<Integer, TableVariable> mapFromVariableIndexToVariable = new LinkedHashMap<>();
		addVariablesToMap(model,mapFromVariableIndexToVariable);
		List<TableFactor> factors = new ArrayList<>();
		
		int nFactors = model.numberFactors();
		for(int i = 0; i < nFactors; i++) {
			TableFactor f = convertUAIToTableFactor(model.getFactor(i),mapFromVariableIndexToVariable);
			factors.add(f);
		}
		return factors;
	}

	private static void addVariablesToMap(UAIModel model,	LinkedHashMap<Integer,
			TableVariable> mapFromVariableIndexToVariable) {
		int nFactors = model.numberFactors();
		for(int i = 0; i < nFactors; i++) {
			FactorTable f = model.getFactor(i);
			addVariablesFromFactorToMap(f,mapFromVariableIndexToVariable);
		}
	}

	private static void addVariablesFromFactorToMap(FactorTable f,
			LinkedHashMap<Integer, TableVariable> mapFromVariableIndexToVariable) {
		List<Integer> variablesIndex = f.getVariableIndexes();
		List<Integer> variablesCardinality = f.getTable().getVariableCardinalities();
		
		if(variablesIndex.size() != variablesCardinality.size()) {
			//TODO error message
		}
		
		for(int i = 0; i < variablesIndex.size();i++) {
			Integer variableIndex = variablesIndex.get(i);
			Integer variableCardinality = variablesCardinality.get(i);
			TableVariable variable = new TableVariable(variableIndex,variableCardinality );
			mapFromVariableIndexToVariable.put(variableIndex,variable);
		}
	}

	private static TableFactor convertUAIToTableFactor(FactorTable factor,
			LinkedHashMap<Integer, TableVariable> mapFromVariableIndexToVariable) {
			
		List<TableVariable> listOfVariables = new ArrayList<>();
		
		for(Integer variableIndex : factor.getVariableIndexes()) {
			listOfVariables.add(mapFromVariableIndexToVariable.get(variableIndex));
		}
		
		TableFactor res = new TableFactor(listOfVariables, factor.getTable());
		return res;
	}
	
}
