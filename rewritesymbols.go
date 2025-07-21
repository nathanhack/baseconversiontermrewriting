package utils

import (
	"container/list"
	"math/big"

	"errors"

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
	Value int
	Base  int // The base of this specific digit in the mixed-base representation.
}

// DigitBigInt represents a digit with a value and base, both of which are
// arbitrarily large integers.
type DigitBigInt struct {
	Value *big.Int
	Base  *big.Int
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
		workList.PushBack(&Digit{Value: int(s), Base: inputBase})
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
			if digit.Base == inputBase && digit.Value == 0 {
				workList.Remove(front)
				continue // Restart scan from the beginning.
			}
		}

		// Rule II: `begin · d'_b1 · tl −→ begin · d_b1 · d_b2 · tl`
		// Rewrites the leading digit of the source base.
		if workList.Len() > 0 {
			front := workList.Front()
			digit := front.Value.(*Digit)
			if digit.Base == inputBase {
				dPrimeB1Val := digit.Value
				quot := dPrimeB1Val / outputBase
				rem := dPrimeB1Val % outputBase

				// Modify d'_b1 in-place and insert d_b2 after it.
				digit.Value = quot
				workList.InsertAfter(&Digit{Value: rem, Base: outputBase}, front)
				continue // Restart scan.
			}
		}

		// Rule III: `d_b2 · d_b1 · tl −→ d'_b1 · d'_b2 · tl`
		// Percolates source base digits to the left by swapping with target base digits.
		ruleIIIApplied := false
		for e := workList.Front(); e != nil && e.Next() != nil; e = e.Next() {
			digit1 := e.Value.(*Digit)
			digit2 := e.Next().Value.(*Digit)
			if digit1.Base == outputBase && digit2.Base == inputBase {
				d_b2_val := digit1.Value
				d_b1_val := digit2.Value

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
				digit1.Value = quot
				digit1.Base = inputBase

				digit2.Value = rem
				digit2.Base = outputBase

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
		result[i] = T(e.Value.(*Digit).Value)
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
		totalValue.Add(totalValue, new(big.Int).Mul(big.NewInt(int64(digit.Value)), currentPowerOfBases))
		currentPowerOfBases.Mul(currentPowerOfBases, big.NewInt(int64(digit.Base)))
	}
	return totalValue
}

// SymbolsValueBigInt converts a number represented as a slice of DigitBigInts (mixed-base representation)
// to its numerical value as a *big.Int. This version handles arbitrarily large digit values and bases.
// The input `symbols` slice is assumed to be ordered from Most Significant Digit (MSD) to
// Least Significant Digit (LSD).
func SymbolsValueBigInt(symbols []DigitBigInt) *big.Int {
	totalValue := big.NewInt(0)
	currentPowerOfBases := big.NewInt(1)
	term := new(big.Int) // To store intermediate multiplication result

	for i := len(symbols) - 1; i >= 0; i-- { // Iterate from LSB to MSB
		digit := symbols[i]
		// term = digit.Value * currentPowerOfBases
		term.Mul(digit.Value, currentPowerOfBases)
		// totalValue = totalValue + term
		totalValue.Add(totalValue, term)
		// currentPowerOfBases = currentPowerOfBases * digit.Base
		currentPowerOfBases.Mul(currentPowerOfBases, digit.Base)
	}
	return totalValue
}

// ValueAppliedToSymbols decomposes a numerical value into a mixed-base representation
// defined by the bases of the provided symbols slice. It updates the value of each
// digit in-place.
//
// The function processes digits from Least Significant (LSB) to Most Significant (MSB).
// If the provided value is too large to be represented by the given symbols' bases,
// it returns an error. A negative input value will also result in an error.
func ValueAppliedToSymbols(value *big.Int, symbols []Digit) error {
	// Make a mutable copy of the value to avoid modifying the caller's *big.Int.
	remainingValue := new(big.Int).Set(value)
	zero := big.NewInt(0)

	// Check for negative input value, which is not supported.
	if remainingValue.Cmp(zero) < 0 {
		return errors.New("negative values are not supported")
	}

	// Iterate from LSB to MSB (right to left in the slice).
	for i := len(symbols) - 1; i >= 0; i-- {
		digit := &symbols[i]
		base := big.NewInt(int64(digit.Base))

		// Use DivMod to get quotient and remainder in one step.
		// remainingValue = quot * base + rem
		quot, rem := new(big.Int), new(big.Int)
		quot.DivMod(remainingValue, base, rem)

		// The remainder is the new value for the current digit.
		// It will always fit in an int because 0 <= rem < base, and base is an int.
		digit.Value = int(rem.Int64())

		// The quotient becomes the new remaining value for the next iteration.
		remainingValue.Set(quot)
	}

	// After processing all digits, if there's still a remaining value,
	// it means the original value was too large for the given symbols.
	if remainingValue.Cmp(zero) != 0 {
		return errors.New("value is too large to be represented by the given symbols")
	}

	return nil
}

// SymbolsRewriteToBigInt converts a number represented as a slice of *big.Int digits
// from an input base to an output base, both also *big.Int.
// It implements the term-rewriting algorithm for direct base conversion, adapted for
// arbitrary-precision integers. This method avoids explicit conversion to and from a
// single large integer representation for the entire number, operating directly on the
// digit sequence.
func SymbolsRewriteToBigInt(symbols []*big.Int, inputBase, outputBase *big.Int) []*big.Int {
	if inputBase.Cmp(outputBase) == 0 {
		result := make([]*big.Int, len(symbols))
		for i, v := range symbols {
			result[i] = new(big.Int).Set(v) // Create a copy
		}
		return result
	}

	if len(symbols) == 0 {
		return []*big.Int{}
	}

	// Initialize the mixed-base list from the input symbols.
	workList := list.New()
	for _, s := range symbols {
		workList.PushBack(&DigitBigInt{Value: new(big.Int).Set(s), Base: inputBase})
	}

	// Optimization for Rule III: For bases up to maxBaseForPrecompute, if the total number of Rule III variants
	// is manageable, precompute them in a lookup table for faster access.
	maxBaseForPrecomputeBig := big.NewInt(maxBaseForPrecompute)
	maxPrecomputeMapSizeBig := big.NewInt(maxPrecomputeMapSize)

	canPrecompute := inputBase.Cmp(maxBaseForPrecomputeBig) <= 0 &&
		outputBase.Cmp(maxBaseForPrecomputeBig) <= 0

	if canPrecompute {
		totalSize := new(big.Int).Mul(inputBase, outputBase)
		if totalSize.Cmp(maxPrecomputeMapSizeBig) >= 0 {
			canPrecompute = false
		}
	}

	type bigIntQuotRem struct{ quot, rem *big.Int }
	var ruleIIIMap [][]*bigIntQuotRem
	if canPrecompute {
		inputBaseInt := inputBase.Int64()
		outputBaseInt := outputBase.Int64()
		ruleIIIMap = make([][]*bigIntQuotRem, outputBaseInt)
		for d_b2_val_int := int64(0); d_b2_val_int < outputBaseInt; d_b2_val_int++ {
			ruleIIIMap[d_b2_val_int] = make([]*bigIntQuotRem, inputBaseInt)
			d_b2_val := big.NewInt(d_b2_val_int)
			for d_b1_val_int := int64(0); d_b1_val_int < inputBaseInt; d_b1_val_int++ {
				d_b1_val := big.NewInt(d_b1_val_int)

				// val = d_b1_val + d_b2_val * inputBase
				val := new(big.Int).Mul(d_b2_val, inputBase)
				val.Add(val, d_b1_val)

				// quot, rem = val / outputBase, val % outputBase
				quot, rem := new(big.Int), new(big.Int)
				quot.DivMod(val, outputBase, rem)

				ruleIIIMap[d_b2_val_int][d_b1_val_int] = &bigIntQuotRem{quot: quot, rem: rem}
			}
		}
	}

	zero := big.NewInt(0)

	// Repeatedly apply rewrite rules until the list is in normal form.
	for { // Main rewrite loop
		// Rule I: `begin · 0_b1 · tl −→ begin · tl`
		if workList.Len() > 0 {
			front := workList.Front()
			digit := front.Value.(*DigitBigInt)
			if digit.Base.Cmp(inputBase) == 0 && digit.Value.Cmp(zero) == 0 {
				workList.Remove(front)
				continue // Restart scan.
			}
		}

		// Rule II: `begin · d'_b1 · tl −→ begin · d_b1 · d_b2 · tl`
		if workList.Len() > 0 {
			front := workList.Front()
			digit := front.Value.(*DigitBigInt)
			if digit.Base.Cmp(inputBase) == 0 {
				dPrimeB1Val := new(big.Int).Set(digit.Value)
				quot, rem := new(big.Int), new(big.Int)
				quot.DivMod(dPrimeB1Val, outputBase, rem)

				// Modify d'_b1 in-place and insert d_b2 after it.
				digit.Value.Set(quot)
				workList.InsertAfter(&DigitBigInt{Value: rem, Base: outputBase}, front)
				continue // Restart scan.
			}
		}

		// Rule III: `d_b2 · d_b1 · tl −→ d'_b1 · d'_b2 · tl`
		ruleIIIApplied := false
		for e := workList.Front(); e != nil && e.Next() != nil; e = e.Next() {
			digit1 := e.Value.(*DigitBigInt)
			digit2 := e.Next().Value.(*DigitBigInt)
			if digit1.Base.Cmp(outputBase) == 0 && digit2.Base.Cmp(inputBase) == 0 {
				var quot, rem *big.Int
				if canPrecompute {
					// Use the precomputed table for a faster lookup.
					// The values are guaranteed to fit in int64 because they are less than their respective bases,
					// which are themselves small enough for precomputation.
					d_b2_val_int := digit1.Value.Int64()
					d_b1_val_int := digit2.Value.Int64()
					result := ruleIIIMap[d_b2_val_int][d_b1_val_int]
					quot = result.quot
					rem = result.rem
				} else {
					// Calculate on the fly for large bases.
					// val = d_b1_val + d_b2_val * inputBase
					val := new(big.Int).Mul(digit1.Value, inputBase)
					val.Add(val, digit2.Value)

					// quot, rem = val / outputBase, val % outputBase
					quot, rem = new(big.Int), new(big.Int)
					quot.DivMod(val, outputBase, rem)
				}

				// Modify the digits in-place.
				digit1.Value.Set(quot)
				digit1.Base = inputBase
				digit2.Value.Set(rem)
				digit2.Base = outputBase

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
		return []*big.Int{big.NewInt(0)}
	}

	// Convert the final list of Digits (now all in outputBase) to the result type.
	result := make([]*big.Int, workList.Len())
	i := 0
	for e := workList.Front(); e != nil; e = e.Next() {
		result[i] = e.Value.(*DigitBigInt).Value
		i++
	}

	return result
}
