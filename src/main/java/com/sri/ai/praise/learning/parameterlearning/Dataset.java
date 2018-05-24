package com.sri.ai.praise.learning.parameterlearning;

import java.util.List;

public interface Dataset {
	
	public List<? extends Datapoint> getDatapoints();
}
