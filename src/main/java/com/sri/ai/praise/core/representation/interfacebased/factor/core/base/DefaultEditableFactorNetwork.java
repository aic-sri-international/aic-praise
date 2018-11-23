package com.sri.ai.praise.core.representation.interfacebased.factor.core.base;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.base.IdentityWrapper.identityWrapper;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.EditableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public class DefaultEditableFactorNetwork extends DefaultFactorNetwork implements EditableFactorNetwork {
	
	public DefaultEditableFactorNetwork(List<? extends Factor> factors) {
		super(factors);
	}
	
	@Override
	public boolean containsFactor(Factor f) {
		return this.containsA(identityWrapper(f));
	}

	@Override
	public void add(Factor factor, Variable variable) {
		this.add(identityWrapper(factor),variable);
	}

	@Override
	public EditableFactorNetwork makeEmptyNetwork() {
		return new DefaultEditableFactorNetwork(list());
	}
}
