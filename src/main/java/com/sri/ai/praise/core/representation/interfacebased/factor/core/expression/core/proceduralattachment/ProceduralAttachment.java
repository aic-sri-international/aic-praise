package com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.proceduralattachment;

import static com.sri.ai.praise.other.integration.proceduralattachment.core.EmptyProcedurePayload.EMPTY_PROCEDURE_PAYLOAD;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.other.integration.proceduralattachment.api.Procedure;

public class ProceduralAttachment {
	
	private Variable variable;
	private Procedure<?> procedure;
	private Object value;

	public ProceduralAttachment(Variable variable, Procedure<?> procedure) {
		this.variable = variable;
		this.procedure = procedure;
	}

	public Variable getVariable() {
		return variable;
	}

	public Procedure<?> getProcedure() {
		return procedure;
	}

	public Object getProcedureValue() {
		if (this.value == null) {
			this.value = procedure.evaluate(EMPTY_PROCEDURE_PAYLOAD);
		}
		return this.value;
	}
}
