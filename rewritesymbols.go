package utils

import (
	"container/list"
	"math/big"

	"golang.org/x/exp/constraints"
)

const (
	// maxBaseForPrecompute is the maximum base size for which we consider precomputation, as requested.
	maxBaseForPrecompute = 65536
	// maxPrecomputeMapSize is the maximum number of Rule III variants we will precompute to avoid excessive memory allocation.
	maxPrecomputeMapSize = 1 << 28 //  268_435_456 entries
)

// Digit represents a digit in the mixed-base representation,
// which is an intermediate state during the conversion process.
type Digit struct {
	value int
	base  int // The base of this specific digit in the mixed-base representation.
}


// SymbolsRewriteTo converts a number represented as a slice of digits from an input base to an output base.
// It implements the term-rewriting algorithm for direct base conversion as described in the paper
// "Numeric Base Conversion with Rewriting" by O. Hermant and W. Loboda.
//
// The algorithm works by treating the number as a sequence of digits and repeatedly applying a set of
// local rewrite rules. This process continues until no more rules can be applied, at which point the
// sequence consists entirely of digits in the target base. This method avoids explicit conversion
// to and from a large integer type, operating directly on the digit representation.
func SymbolsRewriteTo[S constraints.Integer, T constraints.Integer](symbols []S, inputBase, outputBase int) []T {
	if inputBase == outputBase {
		result := make([]T, len(symbols))
		for i, v := range symbols {
			result[i] = T(v)
		}
		return result
	}

	if len(symbols) == 0 {
		return []T{}
	}

	// Initialize the mixed-base list from the input symbols.
	workList := list.New()
	for _, s := range symbols {
		workList.PushBack(&Digit{value: int(s), base: inputBase})
	}

	// Optimization for Rule III: For bases up to maxBaseForPrecompute, if the total number of Rule III variants
	// (inputBase * outputBase) is manageable, precompute them in a lookup table for faster access.
	canPrecompute := inputBase <= maxBaseForPrecompute &&
		outputBase <= maxBaseForPrecompute &&
		int64(inputBase)*int64(outputBase) < maxPrecomputeMapSize

	var ruleIIIMap [][]struct{ quot, rem int }
	if canPrecompute {
		ruleIIIMap = make([][]struct{ quot, rem int }, outputBase)
		for d_b2_val := 0; d_b2_val < outputBase; d_b2_val++ {
			ruleIIIMap[d_b2_val] = make([]struct{ quot, rem int }, inputBase)
			for d_b1_val := 0; d_b1_val < inputBase; d_b1_val++ {
				val := d_b1_val + d_b2_val*inputBase
				quot := val / outputBase
				rem := val % outputBase
				ruleIIIMap[d_b2_val][d_b1_val] = struct{ quot, rem int }{quot, rem}
			}
		}
	}

	// Repeatedly apply rewrite rules until the list is in normal form (contains only outputBase digits).
	for { // Main rewrite loop
		// Rule I: `begin · 0_b1 · tl −→ begin · tl`
		// Removes leading zeros from the source base.
		if workList.Len() > 0 {
			front := workList.Front()
			digit := front.Value.(*Digit)
			if digit.base == inputBase && digit.value == 0 {
				workList.Remove(front)
				continue // Restart scan from the beginning.
			}
		}

		// Rule II: `begin · d'_b1 · tl −→ begin · d_b1 · d_b2 · tl`
		// Rewrites the leading digit of the source base.
		if workList.Len() > 0 {
			front := workList.Front()
			digit := front.Value.(*Digit)
			if digit.base == inputBase {
				dPrimeB1Val := digit.value
				quot := dPrimeB1Val / outputBase
				rem := dPrimeB1Val % outputBase

				// Modify d'_b1 in-place and insert d_b2 after it.
				digit.value = quot
				workList.InsertAfter(&Digit{value: rem, base: outputBase}, front)
				continue // Restart scan.
			}
		}

		// Rule III: `d_b2 · d_b1 · tl −→ d'_b1 · d'_b2 · tl`
		// Percolates source base digits to the left by swapping with target base digits.
		ruleIIIApplied := false
		for e := workList.Front(); e != nil && e.Next() != nil; e = e.Next() {
			digit1 := e.Value.(*Digit)
			digit2 := e.Next().Value.(*Digit)
			if digit1.base == outputBase && digit2.base == inputBase {
				d_b2_val := digit1.value
				d_b1_val := digit2.value

				var quot, rem int
				if canPrecompute {
					// Use the precomputed table for a faster lookup.
					result := ruleIIIMap[d_b2_val][d_b1_val]
					quot = result.quot
					rem = result.rem
				} else {
					// Calculate on the fly for large bases.
					val := d_b1_val + d_b2_val*inputBase
					quot = val / outputBase
					rem = val % outputBase
				}

				// Modify the digits in-place.
				digit1.value = quot
				digit1.base = inputBase

				digit2.value = rem
				digit2.base = outputBase

				ruleIIIApplied = true
				break // A rule was applied, so restart the main loop.
			}
		}

		if ruleIIIApplied {
			continue
		}

		// If no rules were applied in a full pass, the conversion is complete.
		break
	}

	// If the original number was 0, the list will be empty. Return [0].
	if workList.Len() == 0 {
		return []T{0}
	}

	// Convert the final list of Digits (now all in outputBase) to the result type.
	result := make([]T, workList.Len())
	i := 0
	for e := workList.Front(); e != nil; e = e.Next() {
		result[i] = T(e.Value.(*Digit).value)
		i++
	}

	return result
}

// SymbolsValue converts a number represented as a slice of Digits (mixed-base representation)
// to its numerical value as a *big.Int.
// The input `symbols` slice is assumed to be ordered from Most Significant Digit (MSD) to
// Least Significant Digit (LSD), i.e., `[an, an-1, ..., a0]`.
// The value is calculated using the formula: ν(an an-1 ...a0) = Σ (from i=0 to n) ν(ai) * Π (from j=0 to i-1) bj
// where a_i is the i-th digit from the right (LSB), and b_j is the base of the j-th digit from the right.
func SymbolsValue(symbols []Digit) *big.Int {
	totalValue := big.NewInt(0)
	currentPowerOfBases := big.NewInt(1)

	for i := len(symbols) - 1; i >= 0; i-- { // Iterate from LSB to MSB
		digit := symbols[i]
		totalValue.Add(totalValue, new(big.Int).Mul(big.NewInt(int64(digit.value)), currentPowerOfBases))
		currentPowerOfBases.Mul(currentPowerOfBases, big.NewInt(int64(digit.base)))
	}
	return totalValue
}
