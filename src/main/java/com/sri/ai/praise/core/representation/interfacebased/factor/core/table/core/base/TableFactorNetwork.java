package com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base;

import static com.sri.ai.praise.core.representation.translation.fromuaitofactors.FromUAIModelToTableFactors.fromUAIModelToTableFactors;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.core.representation.classbased.table.core.uai.UAIModel;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.EditableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultEditableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;

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
		this(fromUAIModelToTableFactors(model, (v,e) -> new ArrayTableFactor(v,e)));
	}

	@Override
	public EditableFactorNetwork makeEmptyNetwork() {
		return new TableFactorNetwork(new ArrayList<>());
	}
	
	@Override
	public TableFactorNetwork clone() {
		return (TableFactorNetwork) super.clone();
	}
}
