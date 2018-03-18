package com.sri.ai.praise.inference.anytimeexactbp.gabrielstry;

import java.util.List;

import com.sri.ai.praise.inference.representation.api.Variable;


//OBS, this is temporary, because , the way i did it, Aproximation calls a variable, 
//and a variable can not be called from util

// solutions, make approximation<T,V>  (but it kinda doesnt make sense for me...) 
public interface Approximation<T> {
	public T totalIgnorance(List<? extends Variable> variablesToSumOut);
}
