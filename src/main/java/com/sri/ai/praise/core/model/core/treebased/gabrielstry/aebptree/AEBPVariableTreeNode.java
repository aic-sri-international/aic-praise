package com.sri.ai.praise.core.model.core.treebased.gabrielstry.aebptree;

import java.util.ArrayList;

import com.google.common.base.Function;
import com.sri.ai.praise.core.model.core.treebased.representation.api.Factor;
import com.sri.ai.praise.core.model.core.treebased.representation.api.Variable;

public class AEBPVariableTreeNode extends AbstractAEBPTreeNode<Variable, Factor> {

	public AEBPVariableTreeNode(Variable root, AbstractAEBPTreeNode<Factor, Variable> parent,Function<Variable, Boolean> isExhausted,
			ArrayList<AbstractAEBPTreeNode<Factor, Variable>> children) {
		super(root, parent, isExhausted,children);
	}
	public AEBPVariableTreeNode(Variable root, AbstractAEBPTreeNode<Factor, Variable> parent,
			Function<Variable, Boolean> isExhausted) {
		super(root, parent,isExhausted);
	}
}
