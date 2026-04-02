# Analysis Report: Calculator Bugs

## Student Name: Matthew Keller
## Date: 3/12/26

---

## Part 1: Static Analysis Findings (ESLint)

Run `npx eslint calculator.js` and record all findings below.

| # | Line | Rule | Description | Severity |
|---|------|------|-------------|----------|
| 1 | 16 | `no-undef` | `reslt` is not defined | Error |
| 2 | 21 | `no-unreachable` | `console.log` after `return` in `subtract` | Error |
| 3 | 30 | `no-fallthrough` | `case "add"` missing `break`, falls through to `subtract` | Warning |
| 4 | 61 | `eqeqeq` | `==` used instead of `===` in `multiply` | Warning |
| 5 | 68 | `no-unused-vars` | `temp` is declared but never used in `power` | Warning |
| 6 | 74 | `no-constant-condition` | `if (true)` is always true in `absolute` | Warning |
| 7 | 76 | `no-unreachable` | `return n` after `if (true)` block is unreachable | Error |

**Total static analysis issues found:** 7

---

## Part 2: Dynamic Analysis Findings (Test Suite)

Run `node test-calculator.js` and record all test failures below.

| # | Test Name | Error Message | Root Cause |
|---|-----------|---------------|------------|
| 1 | add(2, 3) should be 5 | reslt is not defined | Undefined variable `reslt` instead of `b` |
| 2 | divide(10, 0) should throw gracefully | Division by zero should be handled, got Infinity | No validation for division by zero |
| 3 | calculate('add', 10, 5) should be 15 | reslt is not defined | Switch fallthrough + undefined variable |
| 4 | factorial(-1) should handle negative input | Infinite recursion detected | Missing base case for negative numbers |
| 5 | absolute(5) should be 5 | expected 5, got -5 | Constant condition always negates input |

**Total dynamic analysis issues found:** 5

---

## Part 3: Comparison

### Which bugs did ONLY static analysis catch?
<!-- List bugs found by ESLint but NOT by running tests -->

1. Unreachable code in `subtract` function (line 21)
2. Unused variable `temp` in `power` function (line 68)
3. Type coercion warning (`==` vs `===`) in `multiply` function

### Which bugs did ONLY dynamic analysis catch?
<!-- List bugs found by tests but NOT by ESLint -->

1. Division by zero handling (runtime behavior)
2. Infinite recursion with negative factorial input (runtime stack overflow)

### Which bugs were found by BOTH approaches?
<!-- List bugs caught by both ESLint and test failures -->

1. Undefined variable `reslt` in add function (ESLint error + runtime crash)
2. Switch fallthrough causing wrong calculation results
3. Constant condition in absolute function (ESLint warning + wrong test results)

---

## Part 4: Reflection

### Why can't static analysis catch all bugs?
<!-- Your answer (2-3 sentences) -->

Static analysis examines code without executing it, so it cannot detect runtime behaviors like division by zero, infinite recursion, or input-dependent logic errors. It also cannot predict all possible execution paths or data values that occur during actual program execution.

### Why can't dynamic analysis catch all bugs?
<!-- Your answer (2-3 sentences) -->

Dynamic analysis only finds bugs in the code paths that are actually executed during testing. It may miss edge cases, rare conditions, or paths not covered by the test suite, and requires comprehensive test cases to be effective.

### When would you prioritize one approach over the other?
<!-- Your answer (2-3 sentences) -->

Use static analysis early in development for quick feedback on syntax, style, and obvious logic errors. Prioritize dynamic analysis for complex runtime behaviors, integration testing, and validating actual program correctness with real data.

