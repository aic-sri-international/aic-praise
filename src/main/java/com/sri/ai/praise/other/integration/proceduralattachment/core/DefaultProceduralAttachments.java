package com.sri.ai.praise.other.integration.proceduralattachment.core;

import static com.sri.ai.util.Util.map;

import java.util.Map;

import com.sri.ai.praise.other.integration.proceduralattachment.api.ProceduralAttachments;
import com.sri.ai.praise.other.integration.proceduralattachment.api.Procedure;
import com.sri.ai.util.collect.MapWrapper;

public class DefaultProceduralAttachments extends MapWrapper<String, Procedure> implements ProceduralAttachments {

	public DefaultProceduralAttachments() {
		super(map());
	}

	public DefaultProceduralAttachments(Map<String, Procedure> proceduralAttachments) {
		super(proceduralAttachments);
	}
	
}
