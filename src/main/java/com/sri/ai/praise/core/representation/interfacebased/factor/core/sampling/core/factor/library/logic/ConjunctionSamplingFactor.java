package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.logic;

import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.base.AbstractAssociativeCommutativeSemiRingSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number.AbstractAssocCommutRGWithInverseFromOperationOnOtherArgsNotEqualToAbsorbingElSamplingFactor;

/**
 * A {@link AbstractAssociativeCommutativeSemiRingSamplingFactor} instantiated for conjunction.  
 * @author braz
 *
 */
public class ConjunctionSamplingFactor extends AbstractAssocCommutRGWithInverseFromOperationOnOtherArgsNotEqualToAbsorbingElSamplingFactor<Boolean> {

	public ConjunctionSamplingFactor(Variable result, List<? extends Variable> arguments, Random random) {
		super(result, arguments, random);
	}

	@Override
	protected Boolean computeMissingArgument(Boolean functionResultValue, Boolean definedArgumentsOperatorApplication, int missingArgumentIndex) {
		return functionResultValue;
	}

	@Override
	protected Boolean getIdentityElement() {
		return true;
	}

	@Override
	protected Boolean getAbsorbingElement() {
		return false;
	}

	@Override
	protected boolean isAbsorbingElement(Boolean value) {
		return value.equals(getAbsorbingElement());
	}

	@Override
	protected Boolean apply(Boolean v1, Boolean v2) {
		return v1 && v2;
	}

	@Override
	protected Class<Boolean> getValueClass() {
		return Boolean.class;
	}

	@Override
	protected String getFunctionName() {
		return "and";
	}

}
