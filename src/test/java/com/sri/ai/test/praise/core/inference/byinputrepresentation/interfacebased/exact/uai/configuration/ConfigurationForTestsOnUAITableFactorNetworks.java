package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.uai.configuration;

import java.util.ArrayList;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.base.configuration.AbstractConfigurationForTestsOnBatchOfFactorNetworks;
import com.sri.ai.util.base.BinaryFunction;

public class ConfigurationForTestsOnUAITableFactorNetworks extends AbstractConfigurationForTestsOnBatchOfFactorNetworks {

	private BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, TableFactor> tableFactorMaker; 
	private String uaiFileResourcePath;
	
	public ConfigurationForTestsOnUAITableFactorNetworks(
			int numberOfTests,
			String uaiFileResourcePath,
			BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, TableFactor> tableFactorMaker) {
		super(numberOfTests);
		this.tableFactorMaker = tableFactorMaker;
		this.uaiFileResourcePath = uaiFileResourcePath;
	}

	public ConfigurationForTestsOnUAITableFactorNetworks(int numberOfTests, String uaiFileResourcePath) {
		this(numberOfTests, uaiFileResourcePath, (v,e) -> new ArrayTableFactor(v,e));
	}

	public BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, TableFactor> getTableFactorMaker() {
		return tableFactorMaker;
	}

	public String getUAIFileResourcePath() {
		return uaiFileResourcePath;
	}

}
