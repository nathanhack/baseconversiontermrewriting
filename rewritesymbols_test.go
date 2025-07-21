package utils

import (
	"errors"
	"math/big"
	"reflect"
	"testing"
)

func TestSymbolsRewriteTo(t *testing.T) {
	testCases := []struct {
		name       string
		input      []int
		inputBase  int
		outputBase int
		expected   []int
	}{
		{
			name:       "empty input",
			input:      []int{},
			inputBase:  10,
			outputBase: 2,
			expected:   []int{},
		},
		{
			name:       "zero",
			input:      []int{0},
			inputBase:  10,
			outputBase: 2,
			expected:   []int{0},
		},
		{
			name:       "single digit",
			input:      []int{7},
			inputBase:  10,
			outputBase: 2,
			expected:   []int{1, 1, 1},
		},
		{
			name:       "same base",
			input:      []int{1, 2, 3},
			inputBase:  10,
			outputBase: 10,
			expected:   []int{1, 2, 3},
		},
		{
			name:       "base 10 to 7",
			input:      []int{1, 0, 0}, // 100
			inputBase:  10,
			outputBase: 7,
			expected:   []int{2, 0, 2}, // 100 = 2*49 + 0*7 + 2*1
		},
		{
			name:       "base 6 to 10",
			input:      []int{1, 2, 3}, // 1*36 + 2*6 + 3 = 36 + 12 + 3 = 51
			inputBase:  6,
			outputBase: 10,
			expected:   []int{5, 1},
		},
		{
			name:       "base 2 to 10",
			input:      []int{1, 1, 1, 1, 1, 1, 1, 1}, // 255
			inputBase:  2,
			outputBase: 10,
			expected:   []int{2, 5, 5},
		},
		{
			name:       "base 10 to 16",
			input:      []int{4, 7, 8}, // 478
			inputBase:  10,
			outputBase: 16,
			expected:   []int{1, 13, 14}, // 1*256 + 13*16 + 14 = 478
		},
		{
			name:       "base 16 to 10",
			input:      []int{1, 13, 14}, // 0x1DE
			inputBase:  16,
			outputBase: 10,
			expected:   []int{4, 7, 8},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			got := SymbolsRewriteTo[int, int](tc.input, tc.inputBase, tc.outputBase)
			if !reflect.DeepEqual(got, tc.expected) {
				t.Errorf("SymbolsRewriteTo(%v, %d, %d) = %v; want %v", tc.input, tc.inputBase, tc.outputBase, got, tc.expected)
			}
		})
	}

	// Test with different integer types to verify generic constraints
	t.Run("different generic types", func(t *testing.T) {
		input := []int8{1, 13, 14} // 0x1DE in hex
		inputBase := 16
		outputBase := 10
		expected := []uint{4, 7, 8} // 478
		got := SymbolsRewriteTo[int8, uint](input, inputBase, outputBase)
		if !reflect.DeepEqual(got, expected) {
			t.Errorf("SymbolsRewriteTo with chunking = %v; want %v", got, expected)
		}
	})
}

func TestValueAppliedToSymbols(t *testing.T) {
	testCases := []struct {
		name            string
		value           *big.Int
		symbols         []Digit
		expectedSymbols []Digit
		expectedErr     error
	}{
		{
			name:            "zero value",
			value:           big.NewInt(0),
			symbols:         []Digit{{Value: 9, Base: 10}, {Value: 9, Base: 10}},
			expectedSymbols: []Digit{{Value: 0, Base: 10}, {Value: 0, Base: 10}},
			expectedErr:     nil,
		},
		{
			name:            "simple base 10",
			value:           big.NewInt(123),
			symbols:         []Digit{{Value: 0, Base: 10}, {Value: 0, Base: 10}, {Value: 0, Base: 10}},
			expectedSymbols: []Digit{{Value: 1, Base: 10}, {Value: 2, Base: 10}, {Value: 3, Base: 10}},
			expectedErr:     nil,
		},
		{
			name:            "mixed base time representation",
			value:           big.NewInt(90061), // 1 day, 1 hour, 1 minute, 1 second
			symbols:         []Digit{{Value: 0, Base: 365}, {Value: 0, Base: 24}, {Value: 0, Base: 60}, {Value: 0, Base: 60}},
			expectedSymbols: []Digit{{Value: 1, Base: 365}, {Value: 1, Base: 24}, {Value: 1, Base: 60}, {Value: 1, Base: 60}},
			expectedErr:     nil,
		},
		{
			name:            "value fits exactly",
			value:           big.NewInt(99),
			symbols:         []Digit{{Value: 0, Base: 10}, {Value: 0, Base: 10}},
			expectedSymbols: []Digit{{Value: 9, Base: 10}, {Value: 9, Base: 10}},
			expectedErr:     nil,
		},
		{
			name:            "error: value too large",
			value:           big.NewInt(100),
			symbols:         []Digit{{Value: 0, Base: 10}, {Value: 0, Base: 10}},
			expectedSymbols: nil, // Not checked on error
			expectedErr:     errors.New("value is too large to be represented by the given symbols"),
		},
		{
			name:            "error: negative value",
			value:           big.NewInt(-1),
			symbols:         []Digit{{Value: 0, Base: 10}},
			expectedSymbols: nil, // Not checked on error
			expectedErr:     errors.New("negative values are not supported"),
		},
		{
			name:            "edge case: empty symbols, zero value",
			value:           big.NewInt(0),
			symbols:         []Digit{},
			expectedSymbols: []Digit{},
			expectedErr:     nil,
		},
		{
			name:            "edge case: empty symbols, non-zero value",
			value:           big.NewInt(1),
			symbols:         []Digit{},
			expectedSymbols: nil,
			expectedErr:     errors.New("value is too large to be represented by the given symbols"),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			err := ValueAppliedToSymbols(tc.value, tc.symbols)

			if (err != nil) != (tc.expectedErr != nil) || (err != nil && err.Error() != tc.expectedErr.Error()) {
				t.Errorf("ValueAppliedToSymbols() error = %v, wantErr %v", err, tc.expectedErr)
				return
			}

			if err == nil && !reflect.DeepEqual(tc.symbols, tc.expectedSymbols) {
				t.Errorf("ValueAppliedToSymbols() got symbols = %v, want %v", tc.symbols, tc.expectedSymbols)
			}
		})
	}
}

func TestValueAppliedToSymbolsBigInt(t *testing.T) {
	// Helper to create a deep copy of a slice of DigitBigInt.
	// This is necessary because the function under test modifies the slice in-place.
	deepCopy := func(s []DigitBigInt) []DigitBigInt {
		if s == nil {
			return nil
		}
		c := make([]DigitBigInt, len(s))
		for i, d := range s {
			c[i] = DigitBigInt{
				Value: new(big.Int).Set(d.Value),
				Base:  new(big.Int).Set(d.Base),
			}
		}
		return c
	}

	// Helper to compare slices of DigitBigInt, as reflect.DeepEqual would compare pointers.
	compareDigitBigIntSlices := func(t *testing.T, got, want []DigitBigInt) bool {
		t.Helper()
		if len(got) != len(want) {
			return false
		}
		for i := range got {
			if got[i].Value.Cmp(want[i].Value) != 0 || got[i].Base.Cmp(want[i].Base) != 0 {
				return false
			}
		}
		return true
	}

	testCases := []struct {
		name            string
		expectedSymbols []DigitBigInt
		expectedErr     error
	}{
		{
			name: "zero value",
			expectedSymbols: []DigitBigInt{
				{Value: big.NewInt(0), Base: big.NewInt(10)},
				{Value: big.NewInt(0), Base: big.NewInt(10)},
			},
			expectedErr: nil,
		},
		{
			name: "simple base 10",
			expectedSymbols: []DigitBigInt{
				{Value: big.NewInt(1), Base: big.NewInt(10)},
				{Value: big.NewInt(2), Base: big.NewInt(10)},
				{Value: big.NewInt(3), Base: big.NewInt(10)},
			},
			expectedErr: nil,
		},
		{
			name: "mixed base with large numbers",
			expectedSymbols: []DigitBigInt{
				{Value: big.NewInt(5), Base: new(big.Int).Lsh(big.NewInt(2), 128)},
				{Value: big.NewInt(10), Base: big.NewInt(20)},
			},
			expectedErr: nil,
		},
		{
			name:            "edge case: empty symbols, zero value",
			expectedSymbols: []DigitBigInt{},
			expectedErr:     nil,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			// Create a copy of the symbols for the test run, as the function modifies it in-place.
			value := SymbolsValueBigInt(tc.expectedSymbols)
			inputSymbols := deepCopy(tc.expectedSymbols)

			err := ValueAppliedToSymbolsBigInt(value, inputSymbols)

			if (err != nil) != (tc.expectedErr != nil) || (err != nil && err.Error() != tc.expectedErr.Error()) {
				t.Errorf("ValueAppliedToSymbolsBigInt() error = %v, wantErr %v", err, tc.expectedErr)
				return
			}

			if err == nil && !compareDigitBigIntSlices(t, inputSymbols, tc.expectedSymbols) {
				t.Errorf("ValueAppliedToSymbolsBigInt() got symbols = %v, want %v", inputSymbols, tc.expectedSymbols)
			}
		})
	}

	testCases02 := []struct {
		name        string
		value       *big.Int
		symbols     []DigitBigInt
		expectedErr error
	}{
		{
			name:  "error: negative value",
			value: big.NewInt(-1),
			symbols: []DigitBigInt{
				{Value: new(big.Int), Base: big.NewInt(10)},
			},
			expectedErr: errors.New("negative values are not supported"),
		},
		{
			name:  "error: value larger than capacity",
			value: big.NewInt(100),
			symbols: []DigitBigInt{
				{Value: new(big.Int), Base: big.NewInt(10)},
				{Value: new(big.Int), Base: big.NewInt(10)},
			},
			expectedErr: errors.New("value is too large to be represented by the given symbols"),
		},
	}

	for _, tc := range testCases02 {
		t.Run(tc.name, func(t *testing.T) {
			err := ValueAppliedToSymbolsBigInt(tc.value, tc.symbols)

			if (err != nil) != (tc.expectedErr != nil) || (err != nil && err.Error() != tc.expectedErr.Error()) {
				t.Errorf("ValueAppliedToSymbolsBigInt() error = %v, wantErr %v", err, tc.expectedErr)
			}
		})
	}
}

func TestSymbolsValueBigInt(t *testing.T) {
	// Setup for a test case with very large numbers that would overflow standard integer types.
	twoToThe128 := new(big.Int).Exp(big.NewInt(2), big.NewInt(128), nil)
	// expectedLargeValue represents 1 * (2^128) + 1
	expectedLargeValue := new(big.Int).Add(new(big.Int).Set(twoToThe128), big.NewInt(1))

	testCases := []struct {
		name     string
		symbols  []DigitBigInt
		expected *big.Int
	}{
		{
			name:     "empty slice",
			symbols:  []DigitBigInt{},
			expected: big.NewInt(0),
		},
		{
			name:     "single non-zero digit",
			symbols:  []DigitBigInt{{Value: big.NewInt(7), Base: big.NewInt(10)}},
			expected: big.NewInt(7),
		},
		{
			name: "simple base 10",
			symbols: []DigitBigInt{
				{Value: big.NewInt(1), Base: big.NewInt(10)},
				{Value: big.NewInt(2), Base: big.NewInt(10)},
				{Value: big.NewInt(3), Base: big.NewInt(10)},
			},
			expected: big.NewInt(123), // 1*10*10 + 2*10 + 3
		},
		{
			name: "mixed base time representation (1h 1m 1s)",
			symbols: []DigitBigInt{
				{Value: big.NewInt(1), Base: big.NewInt(24)}, // hours
				{Value: big.NewInt(1), Base: big.NewInt(60)}, // minutes
				{Value: big.NewInt(1), Base: big.NewInt(60)}, // seconds
			},
			expected: big.NewInt(3661), // 1*60*60 + 1*60 + 1
		},
		{
			name: "large numbers that overflow int64",
			symbols: []DigitBigInt{
				{Value: big.NewInt(1), Base: big.NewInt(10)},                // MSB
				{Value: big.NewInt(1), Base: new(big.Int).Set(twoToThe128)}, // LSB
			},
			expected: expectedLargeValue, // 1 * (2^128) + 1
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			got := SymbolsValueBigInt(tc.symbols)
			if got.Cmp(tc.expected) != 0 {
				t.Errorf("SymbolsValueBigInt() = %v, want %v", got, tc.expected)
			}
		})
	}
}

func TestSymbolsRewriteToBigInt(t *testing.T) {
	// Helper to create []*big.Int from int64 values for easier test case definition.
	bigInts := func(vals ...int64) []*big.Int {
		res := make([]*big.Int, len(vals))
		for i, v := range vals {
			res[i] = big.NewInt(v)
		}
		return res
	}

	// Helper to compare slices of *big.Int, as reflect.DeepEqual would compare pointers.
	compareBigIntSlices := func(t *testing.T, got, want []*big.Int) bool {
		t.Helper()
		if len(got) != len(want) {
			return false
		}
		for i := range got {
			if got[i].Cmp(want[i]) != 0 {
				return false
			}
		}
		return true
	}

	// Setup for large number test cases.
	largeBase := new(big.Int).Exp(big.NewInt(2), big.NewInt(128), nil)
	largeVal := new(big.Int).Add(new(big.Int).Set(largeBase), big.NewInt(1)) // Value is 2^128 + 1
	largeValStr := largeVal.String()
	largeValDigitsBase10 := make([]*big.Int, len(largeValStr))
	for i, r := range largeValStr {
		largeValDigitsBase10[i] = big.NewInt(int64(r - '0'))
	}

	testCases := []struct {
		name       string
		input      []*big.Int
		inputBase  *big.Int
		outputBase *big.Int
		expected   []*big.Int
	}{
		{
			name:       "empty input",
			input:      bigInts(),
			inputBase:  big.NewInt(10),
			outputBase: big.NewInt(2),
			expected:   bigInts(),
		},
		{
			name:       "zero",
			input:      bigInts(0),
			inputBase:  big.NewInt(10),
			outputBase: big.NewInt(2),
			expected:   bigInts(0),
		},
		{
			name:       "same base",
			input:      bigInts(1, 2, 3),
			inputBase:  big.NewInt(10),
			outputBase: big.NewInt(10),
			expected:   bigInts(1, 2, 3),
		},
		{
			name:       "base 10 to 16",
			input:      bigInts(4, 7, 8), // 478
			inputBase:  big.NewInt(10),
			outputBase: big.NewInt(16),
			expected:   bigInts(1, 13, 14),
		},
		{
			name:       "large number base 10 to large base",
			input:      largeValDigitsBase10, // Digits of 2^128 + 1
			inputBase:  big.NewInt(10),
			outputBase: largeBase,
			expected:   bigInts(1, 1),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			got := SymbolsRewriteToBigInt(tc.input, tc.inputBase, tc.outputBase)
			if !compareBigIntSlices(t, got, tc.expected) {
				t.Errorf("SymbolsRewriteToBigInt() = %v, want %v", got, tc.expected)
			}
		})
	}
}
