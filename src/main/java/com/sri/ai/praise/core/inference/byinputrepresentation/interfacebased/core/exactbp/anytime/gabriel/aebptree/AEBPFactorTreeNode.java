package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.aebptree;

import java.util.ArrayList;

import com.google.common.base.Function;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public class AEBPFactorTreeNode extends AbstractAEBPTreeNode<Factor, Variable> {

	public AEBPFactorTreeNode(Factor root, AbstractAEBPTreeNode<Variable, Factor> parent, Function<Variable, Boolean> isExhausted,
			ArrayList<AbstractAEBPTreeNode<Variable, Factor>> children) {
		super(root, parent,isExhausted, children);
	}
	public AEBPFactorTreeNode(Factor root, AbstractAEBPTreeNode<Variable, Factor> parent,
			Function<Variable, Boolean> isExhausted) {
		super(root, parent,isExhausted);
	}


//TODO
}
