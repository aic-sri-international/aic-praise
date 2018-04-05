package com.sri.ai.praise.inference.gabrielstry;

import java.util.Iterator;

import com.sri.ai.praise.inference.gabrielstry.aebptree.AEBPTreeNode;
import com.sri.ai.praise.inference.gabrielstry.aebptree.AEBPVariableTreeNode;

public class AEBP {
	AEBPModel model;
	AEBPVariableTreeNode tree;//Tree built from the query
	
	Iterator<AEBPTreeNode<?, ?>> getNextNodeToPutOnTheTree;//TODO trocar por ? extends Node
}
