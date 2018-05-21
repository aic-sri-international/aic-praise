package com.sri.ai.praise.learning.parameterlearning.representation.Dataset;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.learning.parameterlearning.Datapoint;
import com.sri.ai.praise.learning.parameterlearning.Dataset;

public class DefaultDataset implements Dataset {
	
	private List<DefaultDatapoint> datapoints;
	
	public DefaultDataset(List<DefaultDatapoint> datapoints) {
		this.datapoints = datapoints;
	}
	
	@Override
	public List<? extends Datapoint> getDatapoints() {
		return datapoints;
	}
	
}
