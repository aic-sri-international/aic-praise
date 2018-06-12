package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.representation.core;

import static com.sri.ai.util.base.IdentityWrapper.identityWrapper;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.representation.api.EditableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.AbstractFactorNetwork;

public abstract class AbstractEditableFactorNetwrok extends AbstractFactorNetwork implements EditableFactorNetwork{
	
	public AbstractEditableFactorNetwrok() {
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
