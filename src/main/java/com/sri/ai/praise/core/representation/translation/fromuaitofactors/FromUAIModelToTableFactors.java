package com.sri.ai.praise.core.representation.translation.fromuaitofactors;

import static com.sri.ai.praise.core.representation.classbased.table.core.uai.UAIUtil.genericVariableName;
import static com.sri.ai.util.Util.filter;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.sri.ai.praise.core.representation.classbased.table.core.data.markov.FactorTable;
import com.sri.ai.praise.core.representation.classbased.table.core.uai.UAIModel;
import com.sri.ai.praise.core.representation.classbased.table.core.uai.parsing.UAIModelReader;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;

/**
 * A utility for converting a {@link UAIModel} to a list of {@link TableFactor}s.
 * 
 * @author gabriel
 *
 */
public class FromUAIModelToTableFactors {
	
	public static TableFactorNetwork fromUAIModelToTableFactorNetwork(UAIModel model, BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, TableFactor> tableFactorMaker) {
		return new TableFactorNetwork(fromUAIModelToTableFactors(model, tableFactorMaker));
	}
	
	public static List<? extends TableFactor> fromUAIModelToTableFactors(UAIModel model, BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, TableFactor> tableFactorMaker) {
		LinkedHashMap<Integer, TableVariable> mapFromVariableIndexToVariable = computeMapFromVariableIndexToVariable(model);

		List<TableFactor> factors = new ArrayList<>();
		for (int i = 0; i < model.numberFactors(); i++) {
			TableFactor f = convertUAIFactorToTableFactor(model.getFactor(i), mapFromVariableIndexToVariable, tableFactorMaker);
			factors.add(f);
		}
		
		LinkedHashMap<TableVariable, Integer> mapOfEvidence = computeMapOfEvidence(model.getEvidence(), mapFromVariableIndexToVariable);
		List<TableFactor> result = incorporateEvidenceAndSimplifyFactors(factors,mapOfEvidence);
		
		return result;
	}

	private static LinkedHashMap<Integer,TableVariable> computeMapFromVariableIndexToVariable(UAIModel model) {
		LinkedHashMap<Integer,TableVariable> mapFromVariableIndexToVariable = new LinkedHashMap<>();
		int nFactors = model.numberFactors();
		for (int i = 0; i < nFactors; i++) {
			FactorTable f = model.getFactor(i);
			addVariablesFromFactorToMapFromVariableIndexToVariable(f,mapFromVariableIndexToVariable);
		}
		return mapFromVariableIndexToVariable;
	}

	private static TableFactor convertUAIFactorToTableFactor(
			FactorTable factor, 
			LinkedHashMap<Integer,TableVariable> mapFromVariableIndexToVariable,
			BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, TableFactor> tableFactorMaker) {
			
		ArrayList<TableVariable> listOfVariables = new ArrayList<>();
		
		for (Integer variableIndex : factor.getVariableIndexes()) {
			listOfVariables.add(mapFromVariableIndexToVariable.get(variableIndex));
		}
		
		TableFactor result = tableFactorMaker.apply(listOfVariables, new ArrayList<>(factor.getTable().getEntries()));
		return result;
	}

	private static LinkedHashMap<TableVariable, Integer> computeMapOfEvidence(Map<Integer, Integer> evidenceMapIndexVarToValue,
			LinkedHashMap<Integer, TableVariable> mapFromVariableIndexToVariable) {
		LinkedHashMap<TableVariable, Integer> result = new LinkedHashMap<TableVariable,Integer>();
		for (Entry<Integer, Integer> entry : evidenceMapIndexVarToValue.entrySet()) {
			TableVariable var = mapFromVariableIndexToVariable.get(entry.getKey());
			Integer val = entry.getValue();
			result.put(var, val);
		}
		
		return result;
	}

	private static List<TableFactor> incorporateEvidenceAndSimplifyFactors(List<TableFactor> factors, LinkedHashMap<TableVariable, Integer> mapOfEvidences) {
		List<TableFactor> result = Util.mapIntoList(factors, f -> f.slice(mapOfEvidences));
		result = filter(result, v -> v != null);
		return result;
	}
	
	private static void addVariablesFromFactorToMapFromVariableIndexToVariable(FactorTable f,	LinkedHashMap<Integer,TableVariable> mapFromVariableIndexToVariable) {
		List<Integer> variablesIndex = f.getVariableIndexes();
		List<Integer> variablesCardinality = f.getTable().getVariableCardinalities();
		
		if (variablesIndex.size() != variablesCardinality.size()) {
			//TODO error message
		}
		
		for (int i = 0; i < variablesIndex.size();i++) {
			Integer variableIndex = variablesIndex.get(i);
			Integer variableCardinality = variablesCardinality.get(i);
			TableVariable variable = new TableVariable(genericVariableName(variableIndex),variableCardinality );
			mapFromVariableIndexToVariable.put(variableIndex,variable);
		}
	}

	public static TableFactorNetwork makeTableFactorNetwork(
			String uaiFileResourcePath,
			BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, TableFactor> tableFactorMaker) {
		
		TableFactorNetwork factorNetwork; 
		try {
			InputStream resourceStream = FromUAIModelToTableFactors.class.getResourceAsStream(uaiFileResourcePath);
			InputStreamReader resourceReader = new InputStreamReader(resourceStream);
			UAIModel uaiModel = UAIModelReader.read(resourceReader);
			factorNetwork = fromUAIModelToTableFactorNetwork(uaiModel, tableFactorMaker);
		} catch (IOException e) {
			throw new Error("Could not read UAI file " + uaiFileResourcePath);
		}
		return factorNetwork;
	}
}
