package utils

import (
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
