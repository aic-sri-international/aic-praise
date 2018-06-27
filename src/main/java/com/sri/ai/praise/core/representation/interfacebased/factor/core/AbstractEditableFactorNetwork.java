package com.sri.ai.praise.core.representation.interfacebased.factor.core;

import static com.sri.ai.util.base.IdentityWrapper.identityWrapper;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.EditableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public abstract class AbstractEditableFactorNetwork extends AbstractFactorNetwork implements EditableFactorNetwork {
	
	public AbstractEditableFactorNetwork(List<? extends Factor> factors) {
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
}
