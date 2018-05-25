package com.sri.ai.praise.inference.treebased.anytimeexactbp.polytope.box;

import com.sri.ai.praise.inference.treebased.representation.Table.TableVariable;

public class TableBoxVariable extends TableVariable implements BoxVariable{

	public final static TableBoxVariable TABLE_BOX_VARIABLE = new TableBoxVariable();
	
	public TableBoxVariable() {
		super("TableBoxVar", 2);
	}

}
