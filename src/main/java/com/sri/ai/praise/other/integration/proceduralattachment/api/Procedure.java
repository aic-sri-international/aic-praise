package com.sri.ai.praise.other.integration.proceduralattachment.api;

public interface Procedure<T> {
  T evaluate(ProcedurePayload payload);
}
