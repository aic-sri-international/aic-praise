package com.sri.ai.praise.core.representation.translation.rodrigoframework.fromuaitofactors;

import static com.sri.ai.praise.core.representation.classbased.table.core.uai.UAIUtil.genericVariableName;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactor.copyToSubTableFactor;
import static com.sri.ai.util.Util.filter;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.sri.ai.praise.core.representation.classbased.table.core.data.markov.FactorTable;
import com.sri.ai.praise.core.representation.classbased.table.core.uai.UAIModel;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableVariable;
import com.sri.ai.util.Util;

/**
 * A utility for converting a {@link UAIModel} to a list of {@link TableFactor}s.
 * 
 * @author gabriel
 *
 */
public class FromUAIModelToTableFactors {
	
	public static TableFactorNetwork fromUAIModelToTableFactorNetwork(UAIModel model) {
		return new TableFactorNetwork(fromUAIModelToTableFactors(model));
	}
	
	public static List<? extends TableFactor> fromUAIModelToTableFactors(UAIModel model) {
		LinkedHashMap<Integer, TableVariable> mapFromVariableIndexToVariable = computeMapFromVariableIndexToVariable(model);

		List<TableFactor> factors = new ArrayList<>();
		for(int i = 0; i < model.numberFactors(); i++) {
			TableFactor f = convertUAIFactorToTableFactor(model.getFactor(i), mapFromVariableIndexToVariable);
			factors.add(f);
		}
		
		LinkedHashMap<TableVariable, Integer> mapOfEvidence = computeMapOfEvidence(model.getEvidence(), mapFromVariableIndexToVariable);
		List<TableFactor> result = incorporateEvidenceAndSimplifyFactors(factors,mapOfEvidence);
		
		return result;
	}

	private static LinkedHashMap<Integer,TableVariable> computeMapFromVariableIndexToVariable(UAIModel model) {
		LinkedHashMap<Integer,TableVariable> mapFromVariableIndexToVariable = new LinkedHashMap<>();
		int nFactors = model.numberFactors();
		for(int i = 0; i < nFactors; i++) {
			FactorTable f = model.getFactor(i);
			addVariablesFromFactorToMapFromVariableIndexToVariable(f,mapFromVariableIndexToVariable);
		}
		return mapFromVariableIndexToVariable;
	}

	private static TableFactor convertUAIFactorToTableFactor(FactorTable factor, LinkedHashMap<Integer,TableVariable> mapFromVariableIndexToVariable) {
			
		ArrayList<TableVariable> listOfVariables = new ArrayList<>();
		
		for(Integer variableIndex : factor.getVariableIndexes()) {
			listOfVariables.add(mapFromVariableIndexToVariable.get(variableIndex));
		}
		
		TableFactor result = new TableFactor(listOfVariables, new ArrayList<>(factor.getTable().getEntries()));
		return result;
	}

	private static LinkedHashMap<TableVariable, Integer> computeMapOfEvidence(Map<Integer, Integer> evidenceMapIndexVarToValue,
			LinkedHashMap<Integer, TableVariable> mapFromVariableIndexToVariable) {
		LinkedHashMap<TableVariable, Integer> result = new LinkedHashMap<TableVariable,Integer>();
		for(Entry<Integer, Integer> entry : evidenceMapIndexVarToValue.entrySet()) {
			TableVariable var = mapFromVariableIndexToVariable.get(entry.getKey());
			Integer val = entry.getValue();
			result.put(var, val);
		}
		
		return result;
	}

	private static List<TableFactor> incorporateEvidenceAndSimplifyFactors(List<TableFactor> factors, LinkedHashMap<TableVariable, Integer> mapOfEvidences) {
		List<TableFactor> result = Util.mapIntoList(factors, f -> copyToSubTableFactor(f, mapOfEvidences));
		result = filter(result, v -> v != null);
		return result;
	}
	
	private static void addVariablesFromFactorToMapFromVariableIndexToVariable(FactorTable f,	LinkedHashMap<Integer,TableVariable> mapFromVariableIndexToVariable) {
		List<Integer> variablesIndex = f.getVariableIndexes();
		List<Integer> variablesCardinality = f.getTable().getVariableCardinalities();
		
		if(variablesIndex.size() != variablesCardinality.size()) {
			//TODO error message
		}
		
		for(int i = 0; i < variablesIndex.size();i++) {
			Integer variableIndex = variablesIndex.get(i);
			Integer variableCardinality = variablesCardinality.get(i);
			TableVariable variable = new TableVariable(genericVariableName(variableIndex),variableCardinality );
			mapFromVariableIndexToVariable.put(variableIndex,variable);
		}
	}
}
