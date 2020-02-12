package com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.box;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;

public class TableBoxVariable extends TableVariable implements BoxVariable{

	public final static TableBoxVariable TABLE_BOX_VARIABLE = new TableBoxVariable();
	
	public TableBoxVariable() {
		super("TableBoxVar", 2);
	}

}
