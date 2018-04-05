package com.sri.ai.praise.inference.gabrielstry.aebptree;

import java.util.List;

import com.google.common.base.Function;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;

public class AEBPVariableTreeNode extends AbstractAEBPTreeNode<Variable, Factor> {

	public AEBPVariableTreeNode(Variable root, AEBPTreeNode<Factor, Variable> parent,Function<Variable, Boolean> isExhausted,
			List<AEBPTreeNode<Factor, Variable>> children) {
		super(root, parent, isExhausted,children);
	}
	public AEBPVariableTreeNode(Variable root, AEBPTreeNode<Factor, Variable> parent,Function<Variable, Boolean> isExhausted) {
		super(root, parent,isExhausted);
	}
	

	

//TODO
}
