package com.sri.ai.praise.inference.generic.anytime.gabrielstry.representation.core;

import static com.sri.ai.util.base.IdentityWrapper.identityWrapper;

import com.sri.ai.praise.inference.generic.anytime.gabrielstry.representation.api.EditableFactorNetwork;
import com.sri.ai.praise.inference.generic.representation.api.Factor;
import com.sri.ai.praise.inference.generic.representation.api.Variable;
import com.sri.ai.praise.inference.generic.representation.core.AbstractFactorNetwork;

public abstract class AbstractEditableFactorNetwork extends AbstractFactorNetwork implements EditableFactorNetwork{
	
	public AbstractEditableFactorNetwork() {
		super();
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
