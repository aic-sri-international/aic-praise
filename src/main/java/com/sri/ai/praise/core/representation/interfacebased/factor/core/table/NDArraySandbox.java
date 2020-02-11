package com.sri.ai.praise.core.representation.interfacebased.factor.core.table;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntegersIntoList;
import static com.sri.ai.util.Util.println;

import java.util.ArrayList;
import java.util.List;

import org.nd4j.linalg.api.ndarray.INDArray;
import org.nd4j.linalg.factory.Nd4j;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;

public class NDArraySandbox {

	public static void main(String[] args) {
		
		INDArray array = Nd4j.create(new double[] {1.,2.,3.,4.,1.,2.,3.,4.}, new int[] {2,2,2}, 'c');
		System.out.println("array: " + array);
		// System.out.println("array.broadcast: " + array.broadcast(2, 2,2,2));
		System.out.println("array.transpose(): " + array.transpose());
		System.out.println("array.permute(2, 1, 0): " + array.permute(2, 1, 0));
		
//		INDArray permute = array.permute(2, 1, 0);
//		permute.putScalar(0, 0, 0, 0.0);
//		System.out.println("array  : " + array);
//		System.out.println("permute: " + permute);
		
		List<Object> variables1 = list(3, 1);
		INDArray ndarray1 = create(arrayList( 
				1., 2.,
				3., 4.
				), 2, 2);
		
		List<Object> variables2 = list(2, 0, 1);
		INDArray ndarray2 = create(arrayList( 
				5., 6.,
				7., 8.,

				9., 10.,
				11., 12.), 2, 2, 2);

		TableFactor factor1 = makeTableFactor(variables1, ndarray1);
		TableFactor factor2 = makeTableFactor(variables2, ndarray2);
		Factor productTableFactor = factor1.multiply(factor2);
		println(factor1);
		println(factor2);
		println(productTableFactor);
		
		println("Graphical model product of ndarrays:\n" + graphicalModelProduct(ndarray1, ndarray2, 1).permute(0,3,1,2)); // have 3,2,0,1, want 3,1,2,0
		
//		INDArray product = ndArray2.mul(ndArray1);
//		
//		println(product);
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
	public static INDArray graphicalModelProduct(INDArray a1, INDArray a2, int numberOfLastCommonDimensions) {
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
		long numberOfAssignmentsToExternalDimensionsInA1 = productOfFirstNDimensions(a1, numberOfExternalDimensionsInA1);
		long numberOfAssignmentsToExternalDimensionsInA2 = productOfFirstNDimensions(a2, numberOfExternalDimensionsInA2);
		long numberOfAssignmentsToCommonDimensions = productOfLastNDimensions(a1 /* could have been a2 */, numberOfLastCommonDimensions);
		long resultLength = numberOfAssignmentsToExternalDimensionsInA1 * numberOfAssignmentsToExternalDimensionsInA2 * numberOfAssignmentsToCommonDimensions;
		INDArray firstFactor = broadcastByNAtKthDimension(a1, numberOfAssignmentsToExternalDimensionsInA2, numberOfExternalDimensionsInA1).reshape(1, resultLength);
		INDArray secondFactor = broadcastByN(a2, numberOfAssignmentsToExternalDimensionsInA1).reshape(1, resultLength);
		long[] resultShape = makeResultShape(a1, a2, numberOfExternalDimensionsInA1);
		INDArray product = firstFactor.mul(secondFactor);
		INDArray result = product.reshape(resultShape);

//		println("a1: " + a1);
//		println();
//		println("a2: " + a2);
//		println();
//		println("numberOfExternalDimensionsInA1: " + numberOfExternalDimensionsInA1);
//		println();
//		println("numberOfExternalDimensionsInA2: " + numberOfExternalDimensionsInA2);
//		println();
//		println("numberOfAssignmentsToExternalDimensionsInA1: " + numberOfAssignmentsToExternalDimensionsInA1);
//		println();
//		println("numberOfAssignmentsToExternalDimensionsInA2: " + numberOfAssignmentsToExternalDimensionsInA2);
//		println();
//		println("numberOfAssignmentsToCommonDimensions: " + numberOfAssignmentsToCommonDimensions);
//		println();
//		println("resultLength: " + resultLength);
//		println();
//		println("firstFactor: " + firstFactor);
//		println();
//		println("secondFactor: " + secondFactor);
//		println();
//		println("product: " + product);
//		println();
//		println("resultShape: " + resultShape);
//		println();
//		println("result " + result);
//		println();
		
		return result;
	}

	private static long[] makeResultShape(INDArray a1, INDArray a2, int numberOfExternalDimensionsInA1) {
		long[] externalDimensionsInA1 = firstNElements(a1.shape(), numberOfExternalDimensionsInA1);
		long[] resultShape = concatenation(externalDimensionsInA1, a2.shape());
		return resultShape;
	}

	/**
	 * Returns the array broadcast n times as a new first dimension.
	 * This is a more convenient version of {@link NDArray#broadcast(long [])} which requires a new shape
	 * that is identical to the original shape but for the added dimension.
	 * @param array
	 * @param n
	 * @return
	 */
	public static INDArray broadcastByN(INDArray array, long n) {
		long[] newShape = add(array.shape(), 0, n);
		return array.reshape(1, array.length()).broadcast(n, array.length()).reshape(newShape);
		// For now reason, broadcast does not seem to work on multidimensional arrays, so we need to lineralize like that first!
	}
	
	public static INDArray broadcastByNAtKthDimension(INDArray array, long n, long k) {
		return broadcastByNAtKthDimension(array, intValue(n), intValue(k));
	}
	
	public static INDArray broadcastByNAtKthDimension(INDArray array, int n, int k) {
		return rightShiftPermutation(broadcastByN(leftShiftPermutation(array, k), n), k);
	}
	
	public static INDArray leftShiftPermutation(INDArray array, int k) {
		return array.permute(leftShiftPermutation(k, array.rank()));
	}

	public static INDArray rightShiftPermutation(INDArray array, int k) {
		return array.permute(rightShiftPermutation(k, array.rank()));
	}

	/**
	 * A generalization of {@link INDArray#transpose()} to all ranks, including ranks 0 and 1.
	 * @param array
	 * @return the same array if its rank is less than 2, or <code>array.transpose()</code> otherwise.
	 */
	public static INDArray generalTranspose(INDArray array) {
		if (array.rank() < 2) {
			return array;
		}
		else {
			return array.transpose();
		}
	}

	private static long productOfFirstNDimensions(INDArray ndarray, int n) {
		return productOfRangeOfDimensions(ndarray, 0, n);
	}

	private static long productOfLastNDimensions(INDArray ndarray, int n) {
		return productOfRangeOfDimensions(ndarray, ndarray.rank() - n, ndarray.rank());
	}

	private static long productOfRangeOfDimensions(INDArray ndarray, int start, int end) {
		long[] shape = ndarray.shape();
		long result = 1;
		for (int i = start; i != end; i++) {
			result *= shape[i];
		}
		return result;
	}
	
	public static long[] add(long[] array, int index, long element) {
		long[] result = new long[array.length + 1];
		int i = 0;
		for ( ; i != index; i++) {
			result[i] = array[i];
		}
		result[i++] = element;
		for ( ; i != result.length; i++) {
			result[i] = array[i - 1];
		}
		return result;
	}

	public static long[] firstNElements(long[] array, int n) {
		long[] result = new long[n];
		for (int i = 0; i != n; i++) {
			result[i] = array[i];
		}
		return result;
	}
	
	public static long[] concatenation(long[]... arrays) {
		int totalLength = sumOfLengths(arrays);
		long[] result = new long[totalLength];
		int k = 0;
		for (int i = 0; i != arrays.length; i++) {
			for (int j = 0; j != arrays[i].length; j++) {
				result[k++] = arrays[i][j];
			}
		}
		return result;
	}

	public static int sumOfLengths(long[]... arrays) {
		int result = 0;
		for (int i = 0; i != arrays.length; i++) {
			result += arrays[i].length;
		}
		return result;
	}

	public static long product(long[] array) {
		long result = 1;
		for (int i = 0; i != array.length; i++) {
			result *= array[i];
		}
		return result;
	}
	
	public static int[] leftShiftPermutation(int k, int n) {
		int[] result = new int[Long.valueOf(n).intValue()];
		for (int i = 0; i != n; i++) {
			result[i] = (i + k) % n;
		}
		return result;
	}

	public static int[] rightShiftPermutation(int k, int n) {
		return leftShiftPermutation(n - k, n);
	}

	private static int intValue(long k) {
		return Long.valueOf(k).intValue();
	}
	
	private static ArrayListTableFactor makeTableFactor(List<Object> variableIds, INDArray ndArray) {
		ArrayList<Long> shapeAsList = shapeAsList(ndArray);
		var variables = mapIntegersIntoList(shapeAsList.size(), i -> new TableVariable(variableIds.get(i).toString(), shapeAsList.get(i).intValue()));
//		println("ndArray: " + ndArray);
//		println("ndArray entries as array: " + entries(ndArray));
//		println("ndArray entries as list : " + entriesAsList(ndArray));
		return new ArrayListTableFactor(variables, entriesAsList(ndArray));
	}

	private static ArrayList<Double> entriesAsList(INDArray ndArray) {
		return asList(entries(ndArray));
	}

	private static double[] entries(INDArray ndArray) {
		return ndArray.reshape(ndArray.length()).toDoubleVector();
	}

	private static ArrayList<Long> shapeAsList(INDArray ndArray) {
		return asList(ndArray.shape());
	}

	private static ArrayList<Long> asList(long[] array) {
		ArrayList<Long> shapeAsList = new ArrayList<>(array.length);
		for (int i = 0; i != array.length; i++) {
			shapeAsList.add(array[i]);
		}
		return shapeAsList;
	}

	private static ArrayList<Double> asList(double[] array) {
		ArrayList<Double> shapeAsList = new ArrayList<>(array.length);
		for (int i = 0; i != array.length; i++) {
			shapeAsList.add(array[i]);
		}
		return shapeAsList;
	}

	private static INDArray create(ArrayList<Double> arrayList, int... shape1) {
		double[] array = new double[arrayList.size()];
		for (int i = 0; i != arrayList.size(); i++) array[i] = arrayList.get(i);
		return Nd4j.create(array, shape1, 'c');
	}
}
