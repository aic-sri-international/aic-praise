package com.sri.ai.praise.core.representation.interfacebased.factor.core.table;

import static com.sri.ai.praise.core.representation.translation.rodrigoframework.fromuaitofactors.FromUAIModelToTableFactors.fromUAIModelToTableFactors;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.core.representation.classbased.table.core.uai.UAIModel;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.EditableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultEditableFactorNetwork;

/**
 * 
 * 
 * @author gabriel
 *
 */
public class TableFactorNetwork extends DefaultEditableFactorNetwork {
	
	public TableFactorNetwork(List<? extends TableFactor> factors) {
		super(factors);
	}
	
	public TableFactorNetwork(UAIModel model) {
		this(fromUAIModelToTableFactors(model));
	}

	@Override
	public EditableFactorNetwork makeEmptyNetwork() {
		return new TableFactorNetwork(new ArrayList<>());
	}
}
