package com.sri.ai.praise.core.representation.interfacebased.factor.core.table;

import static com.sri.ai.praise.core.representation.classbased.table.core.uai.UAIUtil.genericVariableName;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactor.copyToSubTableFactor;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.sri.ai.praise.core.representation.classbased.table.core.data.markov.FactorTable;
import com.sri.ai.praise.core.representation.classbased.table.core.uai.UAIModel;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.EditableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.DefaultFactorNetwork;
import com.sri.ai.util.Util;
/**
 * 
 * 
 * @author gabriel
 *
 */
public class TableFactorNetwork extends DefaultFactorNetwork {
	
	public TableFactorNetwork(List<? extends TableFactor> factors) {
		super(factors);
	}
	
	public TableFactorNetwork(UAIModel model) {
		this(UAIModelToListOfFactors(model));
	}

	private static List<TableFactor> UAIModelToListOfFactors(UAIModel model) {
		LinkedHashMap<Integer, TableVariable> mapFromVariableIndexToVariable = addVariablesToMap(model);
		List<TableFactor> factors = new ArrayList<>();
		
		int nFactors = model.numberFactors();
		for(int i = 0; i < nFactors; i++) {
			TableFactor f = convertUAIToTableFactor(model.getFactor(i),mapFromVariableIndexToVariable);
			factors.add(f);
		}
		
		LinkedHashMap<TableVariable, Integer> mapOfEvidences = makeMapOfEvidences(model.getEvidence(),mapFromVariableIndexToVariable);
		List<TableFactor> result = incorporateEvidencesAndSimplifyFactors(factors,mapOfEvidences);
		
		
		return result;
	}

	private static List<TableFactor> incorporateEvidencesAndSimplifyFactors(List<TableFactor> factors,
			LinkedHashMap<TableVariable, Integer> mapOfEvidences) {
		List<TableFactor> result = Util.mapIntoList(factors, (f)->copyToSubTableFactor(f, mapOfEvidences));
		result = Util.filter(result, v->v!=null);
		return result;
	}
	
	/*public static TableFactor copyToSubTableFactor(TableFactor factor,
			LinkedHashMap<TableVariable, Integer> mapOfvaluesPredetermined) {
		ArrayList<TableVariable> newVariables = new ArrayList<>(factor.getVariables());
		newVariables.removeAll(mapOfvaluesPredetermined.keySet());
		LinkedHashMap<TableVariable, Integer> predeterminedValuesCopy = new LinkedHashMap<>(mapOfvaluesPredetermined);
		
		if(newVariables.size() == 0) {
			return null;
		}
		
		Iterator<ArrayList<Integer>> cartesianProduct = getCartesianProduct(newVariables);
		
		TableFactor result = new TableFactor(newVariables);
		for(ArrayList<Integer> instantiations: in(cartesianProduct)) {
			for (int i = 0; i < newVariables.size(); i++) {
				predeterminedValuesCopy.put(newVariables.get(i), instantiations.get(i));
			}
			Double newEntryValue =  factor.getEntryFor(predeterminedValuesCopy);
			result.setEntryFor(predeterminedValuesCopy, newEntryValue);
		}
		return result;
	}*/

	private static LinkedHashMap<TableVariable, Integer> makeMapOfEvidences(Map<Integer, Integer> evidenceMapIndexVarToValue,
			LinkedHashMap<Integer, TableVariable> mapFromVariableIndexToVariable) {
		LinkedHashMap<TableVariable, Integer> result = new LinkedHashMap<TableVariable,Integer>();
		for(Entry<Integer, Integer> entry : evidenceMapIndexVarToValue.entrySet()) {
			TableVariable var = mapFromVariableIndexToVariable.get(entry.getKey());
			Integer val = entry.getValue();
			result.put(var, val);
		}
		
		return result;
	}

	private static LinkedHashMap<Integer,TableVariable> addVariablesToMap(UAIModel model) {
		LinkedHashMap<Integer,TableVariable> mapFromVariableIndexToVariable = new LinkedHashMap<>();
		int nFactors = model.numberFactors();
		for(int i = 0; i < nFactors; i++) {
			FactorTable f = model.getFactor(i);
			addVariablesFromFactorToMap(f,mapFromVariableIndexToVariable);
		}
		return mapFromVariableIndexToVariable;
	}

	private static void addVariablesFromFactorToMap(FactorTable f,	LinkedHashMap<Integer,TableVariable> mapFromVariableIndexToVariable) {
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

	private static TableFactor convertUAIToTableFactor(FactorTable factor, LinkedHashMap<Integer,TableVariable> mapFromVariableIndexToVariable) {
			
		ArrayList<TableVariable> listOfVariables = new ArrayList<>();
		
		for(Integer variableIndex : factor.getVariableIndexes()) {
			listOfVariables.add(mapFromVariableIndexToVariable.get(variableIndex));
		}
		
		TableFactor res = new TableFactor(listOfVariables, new ArrayList<>(factor.getTable().getEntries()));
		return res;
	}

	@Override
	public EditableFactorNetwork makeEmptyNetwork() {
		return new TableFactorNetwork(new ArrayList<>());
	}
	
	
//	public static void main(String[] args) {
//		
//		try {
//			// Importing the file and reading it
//			FileReader modelFile = new FileReader(new File("").getAbsolutePath()+"/UAITests/BN_0.uai" );
//			UAIModel model = UAIModelReader.read(modelFile);
//			
//			// Converting the network
//			TableFactorNetwork network = new TableFactorNetwork(model);
//			// Printing the factors
//			//for(IdentityWrapper<Factor> IWf : network.getAs()) {
//			//	Util.println(IWf.getObject());
//			//}
//			
//			//Now we test Product and sum out
//			//product
//			List<IdentityWrapper<Factor>> factors = new ArrayList<>(network.getAs());
//
//			Factor f1 = factors.get(2).getObject();
//			Factor f2 = factors.get(2).getObject();
//			
////			Util.println(f1.multiply(f2)); // OK!
//			int nFactors = network.getAs().size();
//			/*for (int i = 0; i < nFactors; i++) {
//				for (int j = 0; j < nFactors; j++) {
//					f1 = factors.get(i).getObject();
//					f2 = factors.get(j).getObject();
//					
//					Util.println(f1.multiply(f2)); 
//							
//				}
//			}//Seems to be ok!
//			*/
//			//sumOut
//			f1 = factors.get(2).getObject();
//			Util.println(f1.getVariables());
//			List<? extends Variable> a = f1.getVariables();
//			a.remove(0);
//			a.remove(0);
//			Util.println(f1.sumOut(a));
//			
//		} catch (FileNotFoundException e) {
//			e.printStackTrace();
//		} catch (IOException e) {
//			e.printStackTrace();
//		}
//	}
	
}
