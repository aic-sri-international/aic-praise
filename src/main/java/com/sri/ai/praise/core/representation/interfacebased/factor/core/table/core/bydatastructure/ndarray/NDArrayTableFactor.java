package com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.ndarray;

import static com.sri.ai.util.Util.allEqual;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.castOrThrowError;
import static com.sri.ai.util.Util.flatList;
import static com.sri.ai.util.Util.indexOf;
import static com.sri.ai.util.Util.intersection;
import static com.sri.ai.util.Util.mapIntoArray;
import static com.sri.ai.util.Util.setDifference;
import static com.sri.ai.util.Util.valueOrDefaultIfNull;
import static org.apache.commons.lang3.ArrayUtils.toPrimitive;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;
import org.nd4j.linalg.api.ndarray.INDArray;
import org.nd4j.linalg.factory.Nd4j;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.AbstractTableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.util.Util;

/**
 * Discrete table implementation of {@link Factor} using {@link INDArray}.
 * 
 * @author braz
 *
 */

public class NDArrayTableFactor extends AbstractTableFactor {
	
	////////////////////////////////////////////////////////////////////////////////////////////////////
	// DATA MEMBERS ////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private INDArray parameters;
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// CONSTRUCTORS /////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	public NDArrayTableFactor(String factorName, Collection<? extends TableVariable> variables, INDArray parameters) {
		super(factorName, variables);
		this.parameters = parameters;
	}
	
	public NDArrayTableFactor(String factorName, Collection<? extends TableVariable> variables, double[] parameters) {
		this(factorName, variables, create(variables, parameters));
	}

	private static INDArray create(Collection<? extends TableVariable> variables, double[] parameters) {
		int[] localShape = getShape(variables);
//		println("Going to create new ndarray with shape " + localShape);
		INDArray newNDArray = Nd4j.create(parameters).reshape(localShape);
//		println("Creation successful, number of entries is " + newNDArray.length());
		return newNDArray;
	}
	
	public NDArrayTableFactor(String factorName, Collection<? extends TableVariable> variables, ArrayList<Double> parameters) {
		this(factorName, variables, toPrimitive(parameters.toArray(new Double[parameters.size()])));
	}
	
	public NDArrayTableFactor(Collection<? extends TableVariable> variables, ArrayList<Double> parameters) {
		this("phi", variables, parameters);
	}
	
	public NDArrayTableFactor(Collection<? extends TableVariable> variables, INDArray parameters) {
		this("phi", variables, parameters);
	}
	
	public NDArrayTableFactor(Collection<? extends TableVariable> variables) {
		this(variables, Nd4j.create(getShape(variables)));
	}

	private static int[] getShape(Collection<? extends TableVariable> variables) {
		return toPrimitive(mapIntoArray(Integer.class, variables, TableVariable::getCardinality));
	}
	
	private int[] shape;
	private int[] getShape() {
		return valueOrDefaultIfNull(shape, () -> getShape(getVariables()));
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// BASIC METHODS ////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	protected boolean firstParameterIsZero() {
		int firstIndex = 0;
		return asVector().getDouble(firstIndex) == 0;
	}

	@Override
	protected boolean thereAreZeroParameters() {
		return parameters.length() == 0;
	}
	
	@Override
	protected boolean parametersAreAllEqual() {
		return allEqual(arrayList(asDoubleArray()));
	}

	private double[] asDoubleArray() {
		return asVector().toDoubleVector();
	}

	private INDArray asVector() {
		return parameters.reshape(numberOfEntries());
	}

	@Override
	protected String parametersString() {
		return parameters.toString();
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// SLICING //////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public NDArrayTableFactor slice(List<TableVariable> variables, List<Integer> values) {
		return (NDArrayTableFactor) super.slice(variables, values);
	}

	@Override
	public NDArrayTableFactor slice(Map<TableVariable, Integer> assignment) {
		return (NDArrayTableFactor) super.slice(assignment);
	}

	@Override
	protected NDArrayTableFactor slicePossiblyModifyingAssignment(
			Map<TableVariable, Integer> assignment,
			ArrayList<? extends TableVariable> remainingVariables) {
		
		if (assignment.isEmpty()) {
			return this;
		}
		else {
			throw new Error("Slicing not yet implemented for " + NDArrayTableFactor.class);
		}
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// ADDITION /////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	protected NDArrayTableFactor addTableFactor(TableFactor another) {
		var anotherNDArrayTableFactor = castOrThrowError(getClass(), another, () -> "addTableFactor supported for two " + getClass() + " objects only");
		return new NDArrayTableFactor(getVariables(), parameters.add(anotherNDArrayTableFactor.parameters));
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// MULTIPLICATION ///////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////

	@Override
	public NDArrayTableFactor multiply(Factor another) {
		return (NDArrayTableFactor) super.multiply(another);
	}

	@Override
	protected NDArrayTableFactor multiplyTableFactor(TableFactor another) {
		var anotherNDArrayTableFactor = castOrThrowError(getClass(), another, () -> "multiplyTableFactor supported for two " + getClass() + " objects only");
		Set<TableVariable> common = intersection(getVariables(), anotherNDArrayTableFactor.getVariables());
		List<TableVariable> exclusive1 = setDifference(getVariables(), common);
		List<TableVariable> exclusive2 = setDifference(anotherNDArrayTableFactor.getVariables(), common);
		ArrayList<TableVariable> totalVariables = new ArrayList<>(common.size() + exclusive1.size() + exclusive2.size());
		totalVariables.addAll(exclusive1);
		totalVariables.addAll(exclusive2);
		totalVariables.addAll(common);
		int[] permutation1 = indexOf(getVariables(), flatList(exclusive1, common));
		int[] permutation2 = indexOf(anotherNDArrayTableFactor.getVariables(), flatList(exclusive2, common));
		INDArray aligned1 = parameters.permute(permutation1);
		INDArray aligned2 = anotherNDArrayTableFactor.parameters.permute(permutation2);
		INDArray newParameters = graphicalModelProduct(aligned1, aligned2, common.size());
		NDArrayTableFactor result = new NDArrayTableFactor(totalVariables, newParameters);
		return result;
	}
	
	/**
	 * Takes two ndarrays corresponding to two discrete factors in a factor graph satisfying the following conditions:
	 * <ul>
	 * <li> all of the <code>numberOfLastCommonDimensions</id> last dimensions in each ndarray correspond to the same variables in the same order.
	 * <li> all of the remaining dimensions in each factor correspond to variables not shared by the other factor.
	 * </ul>
	 * The method returns a ndarray corresponding to the product of these two factors, with dimensions corresponding, in this order:
	 * <ul>
	 * <li> to the unique variables of the first factor, in the same order in which they appear in it.
	 * <li> to the unique variables of the second factor, in the same order in which they appear in it.
	 * <li> to the shared variables.
	 * </ul>
	 * @param a1
	 * @param a2
	 * @param numberOfLastCommonDimensions
	 * @return
	 */
	static INDArray graphicalModelProduct(INDArray a1, INDArray a2, int numberOfLastCommonDimensions) {
		// This is a bit tricky to visualize
		// The idea I used was the following:
		// Imagine a reshaping of the second array into a vector.
		// It is composed of vectors for the shared dimensions c, one for each assignment to the unique variables u2 of a2.
		// If c(u2_i) is vector on the shared dimensions for the i-th assignment of u2, we have a2 = c(u2_1), ..., c(u2_n). 
		// Likewise, a1 is c(u1_1), ..., c(u1_m).
		// The result is the array ( c(u1_i) * c(u2_j) )_{i,j} for each i, j.
		// The way we achieve this is by making a 'factor2' vector from broadcasting a2 as many times as there assignments to u1,
		// and making a 'factor1' vector by broadcasting **each** c(u1_i) as many times as there are assignments to u2.
		// The latter means broadcasting at the u1-th dimension, but since ND4j only broadcasts at the initial dimension,
		// this requires shifting the u1 dimensions "out of the way", broadcasting, and shifting them back.
		// factor1 and factor 2 have length equal to the result and the elements are matched, so we just multiply them component-wise,
		// and finally reshape them according to the dimensions for the final result.
		
		int numberOfExternalDimensionsInA1 = a1.rank() - numberOfLastCommonDimensions;
		int numberOfExternalDimensionsInA2 = a2.rank() - numberOfLastCommonDimensions;
		long numberOfAssignmentsToExternalDimensionsInA1 = NDArrayUtil.productOfFirstNDimensions(a1, numberOfExternalDimensionsInA1);
		long numberOfAssignmentsToExternalDimensionsInA2 = NDArrayUtil.productOfFirstNDimensions(a2, numberOfExternalDimensionsInA2);
		long numberOfAssignmentsToCommonDimensions = NDArrayUtil.productOfLastNDimensions(a1 /* could have been a2 */, numberOfLastCommonDimensions);
		long resultLength = numberOfAssignmentsToExternalDimensionsInA1 * numberOfAssignmentsToExternalDimensionsInA2 * numberOfAssignmentsToCommonDimensions;
		INDArray firstFactor = NDArrayUtil.broadcastByNAtKthDimension(a1, numberOfAssignmentsToExternalDimensionsInA2, numberOfExternalDimensionsInA1).reshape(1, resultLength);
		INDArray secondFactor = NDArrayUtil.broadcastByN(a2, numberOfAssignmentsToExternalDimensionsInA1).reshape(1, resultLength);
		long[] resultShape = makeResultShape(a1, a2, numberOfExternalDimensionsInA1);
		//println("Going to multiply to shape " + Util.join(Arrays.stream(resultShape).boxed().collect(Collectors.toList())));
		INDArray product = firstFactor.mul(secondFactor);
		INDArray result = product.reshape(resultShape);
		//println("Product successful, length is " + result.length());
		return result;
	}

	static long[] makeResultShape(INDArray a1, INDArray a2, int numberOfExternalDimensionsInA1) {
		long[] externalDimensionsInA1 = NDArrayUtil.firstNElements(a1.shape(), numberOfExternalDimensionsInA1);
		long[] resultShape = NDArrayUtil.concatenation(externalDimensionsInA1, a2.shape());
		return resultShape;
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// NORMALIZATION ////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	protected double computeNormalizationConstant() {
		return parameters.sumNumber().doubleValue();
	}
	
	@Override
	public NDArrayTableFactor normalize() {
		return (NDArrayTableFactor) super.normalize();
	}

	@Override
	protected NDArrayTableFactor normalizeBy(Double normalizationConstant) {
		return new NDArrayTableFactor(getVariables(), parameters.div(normalizationConstant));
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// SUMMING OUT ///////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public NDArrayTableFactor sumOut(Collection<? extends Variable> variablesToSumOut) {
		return (NDArrayTableFactor) super.sumOut(variablesToSumOut);
	}

	@Override
	protected NDArrayTableFactor sumOut(
			List<? extends TableVariable> variablesToSumOut, 
			ArrayList<? extends TableVariable> variablesNotToSumOut) {
		
		INDArray newParameters = parameters.sum(Util.indexOf(getVariables(), variablesToSumOut));
		return new NDArrayTableFactor(variablesNotToSumOut, newParameters);
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// INVERSION ////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public NDArrayTableFactor invert() {
		return new NDArrayTableFactor(getVariables(), Nd4j.ones(getShape()).div(parameters));
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// GET ENTRIES //////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public ArrayList<Double> getEntries() {
		return makeArrayList(parameters.reshape(numberOfEntries()).toDoubleVector());
	}

	private ArrayList<Double> makeArrayList(double[] doubleVector) {
		Double[] arrayOfDoubles = ArrayUtils.toObject(doubleVector);
		List<Double> asList = Arrays.asList(arrayOfDoubles);
		return new ArrayList<Double>(asList);
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// INDEXED ACCESS - GETTERS /////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	protected Double getEntryFor(int[] values) {
		return parameters.getDouble(values);
	}

	@Override
	public Double getEntryFor(ArrayList<Integer> values) {
		int[] intValues = fromArrayListOfIntegersToIntArray(values);
		return getEntryFor(intValues);
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// INDEXED ACCESS - SETTERS /////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	protected void setEntryFor(int[] values, Double newParameterValue) {
		parameters.putScalar(values, newParameterValue);
	}
	
	@Override
	public void setEntryFor(ArrayList<Integer> values, Double newParameterValue) {
		int[] intValues = fromArrayListOfIntegersToIntArray(values);
		setEntryFor(intValues, newParameterValue);
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// INDEX UTILITIES //////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private int[] fromArrayListOfIntegersToIntArray(ArrayList<Integer> values) {
		Integer[] integerArray = values.toArray(new Integer[values.size()]);
		int[] intValues = toPrimitive(integerArray);
		return intValues;
	}

	@Override
	protected TableFactor max(List<? extends TableVariable> eliminated, ArrayList<? extends TableVariable> remaining) {
		throw new Error("Maximization not yet implemented for NDArrayTableFactor");
	}

	@Override
	protected TableFactor min(List<? extends TableVariable> eliminated, ArrayList<? extends TableVariable> remaining) {
		throw new Error("Minimization not yet implemented for NDArrayTableFactor");
	}

	@Override
	public boolean mathematicallyEquals(Factor another) {
		throw new Error("mathematicallyEquals not supported for " + NDArrayTableFactor.class);
	}
}
