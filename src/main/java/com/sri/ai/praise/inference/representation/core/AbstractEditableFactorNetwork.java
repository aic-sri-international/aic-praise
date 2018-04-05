package com.sri.ai.praise.inference.representation.core;

import static com.sri.ai.util.base.IdentityWrapper.identityWrapper;

import com.sri.ai.praise.inference.representation.api.EditableFactorNetwork;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;

/**
 * 
 * @author gabriel
 *
 */
public class AbstractEditableFactorNetwork extends AbstractFactorNetwork implements EditableFactorNetwork{

	@Override
	public void addFactor(Factor factor) {
		for(Variable v: factor.getVariables()) {
			this.add(identityWrapper(factor), v);	
		}
	}
}
