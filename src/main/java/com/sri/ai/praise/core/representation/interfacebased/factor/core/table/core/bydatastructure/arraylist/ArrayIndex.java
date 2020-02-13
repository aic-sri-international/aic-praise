package com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist;

import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.toIntArray;

import java.util.List;

import com.sri.ai.util.Util;

/**
 * A class that takes a set of dimensions, their cardinalities and their strides, and creates
 * an object that goes over all assignments while keeping a calculation of the current offset with minimum overhead.
 * This is mean to have speed comparable to directly iterating over an array.
 * 
 * @author braz
 *
 */
public class ArrayIndex {

	int[] cardinalities;
	int[] strides;
	int[] adjustedStrides;
	int[] index;
	int offset;
	int canMoveUp;
	
	public ArrayIndex(int[] cardinalities, int[] strides) {
		this.cardinalities = cardinalities;
		this.strides = strides;
		this.adjustedStrides = computeAdjustedStrides(strides);
		reset();
	}
	
	public ArrayIndex(List<Integer> cardinalities, List<Integer> strides) {
		this(toIntArray(cardinalities), toIntArray(strides));
	}
	
	public void reset() {
		this.index = new int[cardinalities.length];
		this.offset = 0;
		this.canMoveUp = findDimensionThatCanBeMovedUp(); 
	}

	private int[] computeAdjustedStrides(int[] strides) {
		// Adjusted strides save computation.
		// When an index place i moves up, strides[i] is added to the offset.
		// Also, all places j > i are zeroed, so before they are zeroed their current contribution to the offset,
		// which is (cardinalities[j] - 1)*stride[j], must be subtracted from it.
		// To avoid performing these subtractions for all j > 1, we subtract them from the stride[i] and use this adjusted stride only. 
		int[] adjustedStrides = new int[strides.length];
		int acc = 0;
		for (int i = strides.length - 1; i != -1; i--) {
			adjustedStrides[i] = strides[i] - acc;
			acc += (cardinalities[i] - 1)*strides[i];
		}
		return adjustedStrides;
	}

	public boolean incrementIfPossible() {
		if (isOver()) {
			return false;
		}
		increment();
		return true;
	}
	public boolean isOver() {
		return canMoveUp == -1;
	}
	
	public int[] index() {
		return index;
	}
	
	public int offset() {
		return offset;
	}
	
	public void increment() {
	
		if (canMoveUp == -1) {
			throw new Error("ArrayIndex is already at last value: " + index);
		}
		
		for (int i = index.length - 1; i != canMoveUp; i--) {
			index[i] = 0;
		}
		index[canMoveUp]++;
		offset += adjustedStrides[canMoveUp];

		canMoveUp = findDimensionThatCanBeMovedUp();
	}

	private int findDimensionThatCanBeMovedUp() {
		for (int i = index.length - 1; i != -1; i--) {
			if (index[i] != cardinalities[i] - 1) {
				return i;
			}
		}
		return -1;
	}
	
	public static void main(String[] args) {
		ArrayIndex ai = new ArrayIndex(new int[] {2, 3, 2}, new int[] {12, 2, 1} );
		println(Util.join(ai.index()) + ", offset: " + ai.offset() + ", over: " + ai.isOver());
		for (int i = 0; !ai.isOver() && i != 8000000; i++) {
			if (i % 10000 == 0) {
				println(i);
			}
			ai.increment();
			println(Util.join(ai.index()) + ", offset: " + ai.offset() + ", over: " + ai.isOver());
		}
	}
	
}
