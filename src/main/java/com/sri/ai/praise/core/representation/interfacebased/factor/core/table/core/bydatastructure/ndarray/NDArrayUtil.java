package com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.ndarray;

import org.nd4j.linalg.api.ndarray.INDArray;
import org.nd4j.linalg.cpu.nativecpu.NDArray;

public class NDArrayUtil {

	/**
	 * Returns the array broadcast n times as a new first dimension.
	 * This is a more convenient version of {@link NDArray#broadcast(long [])} which requires a new shape
	 * that is identical to the original shape but for the added dimension.
	 * @param array
	 * @param n
	 * @return
	 */
	public static INDArray broadcastByN(INDArray array, long n) {
		long[] newShape = insert(array.shape(), 0, n);
		return array.reshape(1, array.length()).broadcast(n, array.length()).reshape(newShape);
		// For some reason, broadcast does not seem to work on multidimensional arrays, so we need to lineralize like that first!
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

	public static long productOfFirstNDimensions(INDArray ndarray, int n) {
		return productOfRangeOfDimensions(ndarray, 0, n);
	}

	public static long productOfLastNDimensions(INDArray ndarray, int n) {
		return productOfRangeOfDimensions(ndarray, ndarray.rank() - n, ndarray.rank());
	}

	public static long productOfRangeOfDimensions(INDArray ndarray, int start, int end) {
		long[] shape = ndarray.shape();
		long result = 1;
		for (int i = start; i != end; i++) {
			result *= shape[i];
		}
		return result;
	}
	
	public static long[] insert(long[] array, int index, long element) {
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

	public static int intValue(long k) {
		return Long.valueOf(k).intValue();
	}
}
