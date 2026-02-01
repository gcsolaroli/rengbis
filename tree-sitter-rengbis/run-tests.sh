#!/bin/bash
#
# Test tree-sitter grammar against schema samples.
# - Runs tree-sitter unit tests from test/corpus/
# - Files without "-NOT_VALID" should parse without errors
# - Files with "-NOT_VALID" should have parse errors
#

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TREE_SITTER="$SCRIPT_DIR/../tools/bin/tree-sitter"
SCHEMAS_DIR="$SCRIPT_DIR/../modules/core/src/test/resources/schemas"

cd "$SCRIPT_DIR"

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo "========================================"
echo "Running tree-sitter unit tests"
echo "========================================"

if ! "$TREE_SITTER" test; then
    echo "Unit tests failed!"
    exit 1
fi

echo ""
echo "========================================"
echo "Testing schema samples"
echo "========================================"

passed=0
failed=0
failures=""

for f in "$SCHEMAS_DIR"/*.rengbis; do
    filename=$(basename "$f")
    result=$("$TREE_SITTER" parse "$f" 2>&1)
    has_error=$(echo "$result" | grep -c "ERROR" || true)

    if [[ "$filename" == *"-NOT_VALID"* ]]; then
        # Negative test: should have errors
        if [ "$has_error" -gt 0 ]; then
            echo -e "${GREEN}OK${NC} (expected error): $filename"
            ((passed++))
        else
            echo -e "${RED}FAIL${NC} (expected error but parsed ok): $filename"
            ((failed++))
            failures="$failures\n  $filename (expected parse error)"
        fi
    else
        # Positive test: should parse without errors
        if [ "$has_error" -eq 0 ]; then
            echo -e "${GREEN}OK${NC}: $filename"
            ((passed++))
        else
            echo -e "${RED}FAIL${NC} (unexpected error): $filename"
            echo "$result" | grep -A2 "ERROR" | head -5
            ((failed++))
            failures="$failures\n  $filename (unexpected parse error)"
        fi
    fi
done

echo ""
echo "========================================"
echo "Results: $passed passed, $failed failed"
echo "========================================"

if [ "$failed" -gt 0 ]; then
    echo -e "Failures:$failures"
    exit 1
fi

exit 0
