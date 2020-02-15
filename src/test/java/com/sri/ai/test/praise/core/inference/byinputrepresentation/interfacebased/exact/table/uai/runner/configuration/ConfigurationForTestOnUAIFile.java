package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.uai.runner.configuration;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.base.configuration.DefaultConfigurationForTestsOnBatchOfFactorNetworks;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;

public class ConfigurationForTestOnUAIFile<Result> extends DefaultConfigurationForTestsOnBatchOfFactorNetworks<Result> {

	private BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, TableFactor> tableFactorMaker; 
	private String uaiFileResourcePath;
	
	public ConfigurationForTestOnUAIFile(
			List<Pair<String, BinaryFunction<Variable, FactorNetwork, Result>>> algorithms,
			int numberOfRuns,
			String uaiFileResourcePath,
			BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, TableFactor> tableFactorMaker) {
		super(algorithms, numberOfRuns);
		this.tableFactorMaker = tableFactorMaker;
		this.uaiFileResourcePath = uaiFileResourcePath;
	}

	public ConfigurationForTestOnUAIFile(
			List<Pair<String, BinaryFunction<Variable, FactorNetwork, Result>>> algorithms,
			int numberOfRuns, 
			String uaiFileResourcePath) {
		this(algorithms, numberOfRuns, uaiFileResourcePath, (v,e) -> new ArrayTableFactor(v,e));
	}

	public BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, TableFactor> getTableFactorMaker() {
		return tableFactorMaker;
	}

	public String getUAIFileResourcePath() {
		return uaiFileResourcePath;
	}

}
