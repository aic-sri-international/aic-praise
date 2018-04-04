package com.sri.ai.praise.inference.representation.Table;

import static com.sri.ai.praise.model.v1.imports.uai.UAIUtil.genericVariableName;
import static com.sri.ai.util.base.IdentityWrapper.identityWrapper;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.core.AbstractFactorNetwork;
import com.sri.ai.praise.lang.grounded.markov.FactorTable;
import com.sri.ai.praise.model.v1.imports.uai.UAIModel;
import com.sri.ai.praise.model.v1.imports.uai.UAIModelReader;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.IdentityWrapper;
/**
 * 
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
		LinkedHashMap<Integer, TableVariable> mapFromVariableIndexToVariable = addVariablesToMap(model);
		List<TableFactor> factors = new ArrayList<>();
		
		int nFactors = model.numberFactors();
		for(int i = 0; i < nFactors; i++) {
			TableFactor f = convertUAIToTableFactor(model.getFactor(i),mapFromVariableIndexToVariable);
			factors.add(f);
		}
		return factors;
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
		
		TableFactor res = new TableFactor(listOfVariables, factor.getTable());
		return res;
	}
	
	
	public static void main(String[] args) {
		
		try {
			// Importing the file and reading it
			FileReader modelFile = new FileReader(new File("").getAbsolutePath()+"/UAITests/BN_0.uai" );
			UAIModel model = UAIModelReader.read(modelFile);
			
			// Converting the network
			TableFactorNetwork network = new TableFactorNetwork(model);
			// Printing the factors
			//for(IdentityWrapper<Factor> IWf : network.getAs()) {
			//	Util.println(IWf.getObject());
			//}
			
			//Now we test Product and sum out
			List<IdentityWrapper<Factor>> factors = new ArrayList<>(network.getAs());

			Factor f1 = factors.get(2).getObject();
			Factor f2 = factors.get(2).getObject();
			
//			Util.println(f1.multiply(f2)); // OK!

			f1 = factors.get(1).getObject();
			f2 = factors.get(2).getObject();
			
			Util.println(f1.multiply(f2)); 
			
			
			
			
			
			//producto
			
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
}
