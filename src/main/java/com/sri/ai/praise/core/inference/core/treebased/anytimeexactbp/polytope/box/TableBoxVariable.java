package com.sri.ai.praise.core.inference.core.treebased.anytimeexactbp.polytope.box;

import com.sri.ai.praise.core.model.core.treebased.table.TableVariable;

public class TableBoxVariable extends TableVariable implements BoxVariable{

	public final static TableBoxVariable TABLE_BOX_VARIABLE = new TableBoxVariable();
	
	public TableBoxVariable() {
		super("TableBoxVar", 2);
	}

}
