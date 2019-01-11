package com.sri.ai.praise.core.representation.interfacebased.factor.core.base;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

/**
 * A {@link Variable} based on a Java object.
 * 
 * @author braz
 *
 */
public class DefaultVariable implements Variable {
	
	private Object object;
	
	public DefaultVariable(Object object) {
		this.object = object;
	}
	
	@Override
	public List<? extends Object> getValues() {
		throw new Error("getValues not supported by " + getClass());
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((object == null) ? 0 : object.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		DefaultVariable other = (DefaultVariable) obj;
		if (object == null) {
			if (other.object != null)
				return false;
		} else if (!object.equals(other.object))
			return false;
		return true;
	}
	
	public Object getObject() {
		return object;
	}

	@Override
	public String toString() {
		return object.toString();
	}

}
