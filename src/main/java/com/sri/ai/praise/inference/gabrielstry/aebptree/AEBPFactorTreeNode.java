package com.sri.ai.praise.inference.gabrielstry.aebptree;

import java.util.List;

import com.google.common.base.Function;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;

public class AEBPFactorTreeNode extends AbstractAEBPTreeNode<Factor, Variable> {

	public AEBPFactorTreeNode(Factor root, AEBPTreeNode<Variable, Factor> parent, Function<Variable, Boolean> isExhausted,
			List<AEBPTreeNode<Variable, Factor>> children) {
		super(root, parent,isExhausted, children);
	}
	public AEBPFactorTreeNode(Factor root, AEBPTreeNode<Variable, Factor> parent,Function<Variable, Boolean> isExhausted) {
		super(root, parent,isExhausted);
	}


//TODO
}
